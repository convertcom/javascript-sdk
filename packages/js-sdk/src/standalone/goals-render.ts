import {LogManagerInterface} from '@convertcom/js-sdk-logger';

const LOCATION_CHANGE_EVENT = 'convert:locationchange';

let historyPatched = false;

const patchHistory = (): void => {
  if (historyPatched || typeof window === 'undefined' || !window.history) return;
  historyPatched = true;

  const dispatch = () => window.dispatchEvent(new Event(LOCATION_CHANGE_EVENT));
  const originalPushState = window.history.pushState.bind(window.history);
  const originalReplaceState = window.history.replaceState.bind(window.history);

  window.history.pushState = function (...args: Parameters<History['pushState']>) {
    const output = originalPushState(...args);
    dispatch();
    return output;
  };

  window.history.replaceState = function (
    ...args: Parameters<History['replaceState']>
  ) {
    const output = originalReplaceState(...args);
    dispatch();
    return output;
  };

  window.addEventListener('popstate', dispatch);
  window.addEventListener('hashchange', dispatch);
};

const throttle = <TArgs extends Array<unknown>>(
  callback: (...args: TArgs) => void,
  wait: number
) => {
  let timeoutId: ReturnType<typeof setTimeout> | null = null;
  let lastArgs: TArgs | null = null;

  return (...args: TArgs) => {
    lastArgs = args;
    if (timeoutId) return;
    timeoutId = setTimeout(() => {
      timeoutId = null;
      if (lastArgs) callback(...lastArgs);
    }, wait);
  };
};

type GoalListener = {
  selector: string;
  event: string;
  goalId: string;
  callback: (goalId: string) => void;
};

export class GoalsRender {
  private readonly name = 'GoalsRender';

  private _loggerManager?: LogManagerInterface;
  private _goalQueue: Array<GoalListener> = [];
  private _activeGoalListeners = new Set<string>();
  private _delegatedAbort = new AbortController();
  private _scrollAbort = new AbortController();
  private _mutationObserver: MutationObserver | null = null;
  private _mutationTimer: ReturnType<typeof setTimeout> | null = null;

  constructor({loggerManager}: {loggerManager?: LogManagerInterface} = {}) {
    this._loggerManager = loggerManager;
    this.observeMutations();
    patchHistory();
  }

  onLocationChange(callback: () => void): void {
    if (typeof window === 'undefined') return;
    window.addEventListener(LOCATION_CHANGE_EVENT, callback, {
      signal: this._delegatedAbort.signal
    });
  }

  prepareDOMGoalListeners({
    selector,
    event,
    goalId,
    callback
  }: {
    selector: string;
    event: string;
    goalId: string;
    callback: (goalId: string) => void;
  }): void {
    this._loggerManager?.debug?.(
      `${this.name}.prepareDOMGoalListeners()`,
      `Queue Goal #${goalId} - selector "${selector}" - event "${event}"`
    );
    this._goalQueue.push({selector, event, goalId, callback});
    setTimeout(() => this.processQueue(), 0);
  }

  prepareScrollGoalListener({
    goals,
    getPercentage,
    callback
  }: {
    goals: Array<string>;
    getPercentage: (goalId: string) => number;
    callback: ({goalId}: {goalId: Array<string>}) => void;
  }): void {
    if (typeof document === 'undefined' || typeof window === 'undefined') return;

    this._scrollAbort.abort();
    this._scrollAbort = new AbortController();

    const triggeredGoals = new Set<string>();
    const onScroll = throttle(() => {
      const denominator = document.body.scrollHeight - window.innerHeight;
      const scrollPercentage =
        denominator > 0 ? Math.ceil((window.scrollY / denominator) * 100) : 100;
      const goalIds: Array<string> = [];

      for (const goalId of Array.from(new Set(goals))) {
        const percentage = Number(getPercentage(goalId) || 0);
        if (scrollPercentage > percentage && !triggeredGoals.has(goalId)) {
          triggeredGoals.add(goalId);
          goalIds.push(goalId);
        }
      }

      if (goalIds.length) callback({goalId: goalIds});
    }, 200);

    document.addEventListener('scroll', onScroll, {
      passive: true,
      signal: this._scrollAbort.signal
    });
    onScroll();
  }

  processQueue(): void {
    if (typeof document === 'undefined') return;

    for (const item of this._goalQueue.splice(0)) {
      const {selector, event, goalId, callback} = item;
      const key = `${selector}:${event}:${goalId}`;
      if (this._activeGoalListeners.has(key)) continue;

      try {
        document.querySelector(selector);
      } catch (error) {
        this._loggerManager?.warn?.(
          `${this.name}.processQueue()`,
          `Invalid selector "${selector}": ${
            (error as Error)?.message || String(error)
          }`
        );
        continue;
      }

      const delegatedCallback = () => callback(goalId);
      const listener = (domEvent: Event) =>
        this.delegateEventListenerCallback({
          selector,
          event: domEvent,
          callback: delegatedCallback
        });

      document.addEventListener(event, listener, {
        capture: event === 'submit',
        signal: this._delegatedAbort.signal
      });
      this._activeGoalListeners.add(key);

      this._loggerManager?.debug?.(
        `${this.name}.processQueue()`,
        `Goal #${goalId} - listening for "${event}" on "${selector}"`
      );
    }
  }

  destroy(): void {
    this._delegatedAbort.abort();
    this._scrollAbort.abort();
    if (this._mutationObserver) this._mutationObserver.disconnect();
    if (this._mutationTimer) clearTimeout(this._mutationTimer);
    this._goalQueue = [];
    this._activeGoalListeners.clear();
  }

  private observeMutations(): void {
    if (
      this._mutationObserver ||
      typeof MutationObserver === 'undefined' ||
      typeof document === 'undefined'
    ) {
      return;
    }

    const target = document.documentElement || document.body;
    if (!target) return;

    this._mutationObserver = new MutationObserver(() => {
      if (this._mutationTimer) clearTimeout(this._mutationTimer);
      this._mutationTimer = setTimeout(() => this.processQueue(), 0);
    });
    this._mutationObserver.observe(target, {
      childList: true,
      subtree: true
    });
  }

  private delegateEventListenerCallback({
    selector,
    event,
    callback
  }: {
    selector: string;
    event: Event;
    callback: () => void;
  }): void {
    let target = event.target as HTMLElement | null;
    if (target?.nodeType === Node.TEXT_NODE) {
      target = target.parentElement;
    }

    const path: Array<EventTarget> =
      typeof event.composedPath === 'function'
        ? event.composedPath()
        : this.getFallbackEventPath(target);

    for (const current of path) {
      if (!(current instanceof HTMLElement)) continue;

      if (typeof current.matches === 'function' && current.matches(selector)) {
        callback();
        return;
      }

      if (current.shadowRoot) {
        try {
          if (current.shadowRoot.querySelector(selector)) {
            callback();
            return;
          }
        } catch {
          return;
        }
      }
    }
  }

  private getFallbackEventPath(target: HTMLElement | null): Array<HTMLElement> {
    const path: Array<HTMLElement> = [];
    let current = target;
    while (current) {
      path.push(current);
      current = current.parentElement;
    }
    return path;
  }
}
