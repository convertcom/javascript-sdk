import {ConfigExperience, BucketedVariation} from '@convertcom/js-sdk-types';
import {VariationChangeType} from '@convertcom/js-sdk-enums';
import {ConvertStandaloneRuntime} from './runtime';

type ChangeLike = {
  type?: string;
  data?: unknown;
  id?: string | number;
};

type ChangeCommand = {
  what?: string;
  params?: Record<string, any>;
};

type QueueItem = ChangeCommand | unknown[];

type ConversionWindow = Window & {
  convert?: Record<string, any>;
  _conv_q?: QueueItem[];
};

const isRecord = (value: unknown): value is Record<string, any> =>
  !!value && typeof value === 'object' && !Array.isArray(value);

const isChangeLike = (value: unknown): value is ChangeLike =>
  isRecord(value) &&
  (typeof value.type === 'string' || Object.prototype.hasOwnProperty.call(value, 'id') || isRecord(value.data));

const isChangeArray = (value: unknown): value is Array<ChangeLike> =>
  Array.isArray(value) && value.every(isChangeLike);

const isVariationLike = (value: unknown): value is BucketedVariation =>
  isRecord(value) &&
  (Object.prototype.hasOwnProperty.call(value, 'id') ||
    Object.prototype.hasOwnProperty.call(value, 'key') ||
    Object.prototype.hasOwnProperty.call(value, 'experienceId') ||
    Object.prototype.hasOwnProperty.call(value, 'experienceKey'));

const isBrowser = (): boolean => typeof window !== 'undefined';

const getWindowScope = (): ConversionWindow | null =>
  isBrowser() ? ((window as unknown) as ConversionWindow) : null;

const getDocument = (): Document | null =>
  isBrowser() && typeof document !== 'undefined' ? document : null;

const getApplierTarget = (): {
  documentRef: Document | null;
  appender: Element | null;
} => {
  const documentRef = getDocument();
  if (!documentRef)
    return {documentRef: null, appender: null};

  const appender =
    documentRef.head ||
    documentRef.documentElement ||
    documentRef.body ||
    (typeof documentRef.getElementsByTagName === 'function'
      ? documentRef.getElementsByTagName('head')[0]
      : null);

  return {documentRef, appender: appender || null};
};

const runCss = (css: string): void => {
  const {documentRef, appender} = getApplierTarget();
  if (!documentRef || !appender || typeof appender.appendChild !== 'function') return;
  if (!css) return;

  try {
    const style = documentRef.createElement('style');
    style.type = 'text/css';
    if (
      typeof documentRef.createTextNode === 'function' &&
      typeof style.appendChild === 'function'
    ) {
      style.appendChild(documentRef.createTextNode(css));
    } else {
      style.textContent = css;
    }
    appender.appendChild(style);
  } catch {
    // noop
  }
};

const runJs = (js: string): void => {
  if (!js || typeof js !== 'string') return;

  try {
    const expressionCode = js.trim().replace(/;+$/g, '');
    if (!expressionCode) return;

    const isFunctionCode =
      /^(?:async\s+)?function\b/.test(expressionCode) ||
      /^(?:async\s*)?\(\s*[^)]*\)\s*=>/.test(expressionCode) ||
      /^(?:async\s*)?[A-Za-z_$][\w$]*\s*=>/.test(expressionCode);

    const executableCode = isFunctionCode
      ? `return (${expressionCode})`
      : `return function(){\n${expressionCode}\n}`;

    const result = Function(executableCode)();
    if (typeof result === 'function') result();
  } catch {
    // noop
  }
};

const normalizePayload = (value: unknown): Record<string, any> => {
  const data = isRecord(value) ? value : {};
  return data;
};

type ToolkitInstance = {
  applyChange: (change: ChangeLike) => boolean;
  applyChanges: (changes: Array<ChangeLike> | ChangeLike) => boolean;
  applyVariation: (
    variation: BucketedVariation,
    options?: {experience?: ConfigExperience}
  ) => boolean;
};

type ToolkitApi = ((
  payload?: unknown,
  options?: {experience?: ConfigExperience}
) => boolean | void) & {
  applyChange: (change: ChangeLike) => boolean;
  applyChanges: (changes: Array<ChangeLike> | ChangeLike) => boolean;
  applyVariation: (
    variation: BucketedVariation,
    options?: {experience?: ConfigExperience}
  ) => boolean;
  run: (
    payload?: BucketedVariation,
    options?: {experience?: ConfigExperience}
  ) => boolean | void;
};

const createToolkit = (
  convert: Record<string, any>,
  runtime: ConvertStandaloneRuntime
): ToolkitApi => {
  const executedChanges = new Set<string>();
  const executedVariations = new Set<string>();

  const getChangeType = (change: ChangeLike): string =>
    String(change?.type || '').trim();

  const getChangeKey = (
    variation: BucketedVariation,
    change?: ChangeLike
  ): string => {
    const variationId = String(
      variation.id ||
        variation.key ||
        variation.experienceId ||
        variation.experienceKey ||
        'variation'
    );
    const changeId = change?.id ? String(change.id) : null;
    return changeId ? `variation:${variationId}:change:${changeId}` : `variation:${variationId}`;
  };

  const applySingleChange = (change: ChangeLike): boolean => {
    if (!isChangeLike(change)) return false;

    const type = getChangeType(change);
    const data = normalizePayload(change.data);
    const cssRaw = data?.css;
    const jsRaw = data?.js;
    const customJsRaw = data?.custom_js;
    const css = typeof cssRaw === 'string' ? cssRaw : undefined;
    const js = typeof jsRaw === 'string' ? jsRaw : undefined;
    const customJs = typeof customJsRaw === 'string' ? customJsRaw : undefined;

    if (
      type === VariationChangeType.DEFAULT_REDIRECT ||
      type === VariationChangeType.FULLSTACK_FEATURE
    ) {
      return false;
    }

    if (!css && !js && !customJs) {
      if (typeof cssRaw === 'string') runCss(cssRaw);
      return false;
    }

    if (css) runCss(css);
    if (js) runJs(js);
    if (customJs) runJs(customJs);
    return true;
  };

  const applyChange: ToolkitInstance['applyChange'] = (change) => {
    const payload = normalizePayload(change);

    const hasType = Object.prototype.hasOwnProperty.call(payload, 'type');
    let executed = false;

    if (!hasType && Object.prototype.hasOwnProperty.call(payload, 'data')) {
      const data = normalizePayload(payload.data);
      const cssRaw = data.css;
      const jsRaw = data.js;
      const customJsRaw = data.custom_js;
      if (typeof cssRaw === 'string') {
        runCss(cssRaw);
        executed = true;
      }
      if (typeof jsRaw === 'string') {
        runJs(jsRaw);
        executed = true;
      }
      if (typeof customJsRaw === 'string') {
        runJs(customJsRaw);
        executed = true;
      }
      return executed;
    }

    return applySingleChange(payload as ChangeLike);
  };

  const applyChanges = (changes: Array<ChangeLike> | ChangeLike): boolean => {
    if (isChangeArray(changes)) {
      let changed = false;
      for (const change of changes) changed = applyChange(change) || changed;
      return changed;
    }
    if (isChangeLike(changes)) return applyChange(changes);
    return false;
  };

  const applyVariation: ToolkitInstance['applyVariation'] = (
    variation,
    options = {}
  ) => {
    const experience = options.experience || null;
    const changes = Array.isArray(variation?.changes) ? variation.changes : [];
    const variationExecutionKey = getChangeKey(variation);
    if (!executedVariations.has(variationExecutionKey)) {
      const globalCss = isRecord(experience)
        ? String(experience.global_css || '')
        : '';
      const globalJs = isRecord(experience) ? String(experience.global_js || '') : '';
      if (globalCss) runCss(globalCss);
      if (globalJs) runJs(globalJs);
      executedVariations.add(variationExecutionKey);
    }

    let changed = false;
    for (const change of changes) {
      const changeExecutionKey = getChangeKey(variation, change);
      if (executedChanges.has(changeExecutionKey)) continue;
      if (applyChange(change)) {
        changed = true;
        executedChanges.add(changeExecutionKey);
      }
    }
    return changed;
  };

  const getQueuePayload = (
    item: QueueItem | unknown
  ): {
    what: string;
    params: unknown;
  } | null => {
    if (Array.isArray(item)) {
      return {
        what: typeof item[0] === 'string' ? String(item[0]).trim() : '',
        params: item.length > 1 ? item[1] : null
      };
    }

    if (!isRecord(item)) return null;
    const what = typeof item.what === 'string' ? String(item.what).trim() : '';
    const params = Object.prototype.hasOwnProperty.call(item, 'params')
      ? item.params
      : null;
    return {what, params};
  };

  const processQueueItem = (item: unknown) => {
    const queueCommand = getQueuePayload(item);
    if (!queueCommand || !queueCommand.what) return;

    const {what, params} = queueCommand;
    const payload = normalizePayload(params);

    switch (what) {
      case 'applyChange':
        applyChange(payload as ChangeLike);
        break;
      case 'applyChanges':
        applyChanges(payload as Array<ChangeLike> | ChangeLike);
        break;
      case 'applyVariation':
      case 'runVariation':
        if (isVariationLike(payload)) applyVariation(payload, {});
        break;
      case 'run':
        if (isVariationLike(payload)) applyVariation(payload, {});
        if (
          isRecord(payload) &&
          Array.isArray(payload.changes) &&
          !isVariationLike(payload)
        ) {
          applyChanges(payload.changes);
        }
        break;
      case 'T':
        if (isRecord(payload) && isChangeArray(payload.changes)) {
          applyChanges(payload.changes);
        } else if (isRecord(payload) || isChangeArray(payload)) {
          applyChanges(payload as Array<ChangeLike> | ChangeLike);
        }
        break;
      default:
        break;
    }
  };

  const installQueue = (): void => {
    const scope = getWindowScope();
    if (!scope || !Array.isArray(scope._conv_q)) return;

    const queue = scope._conv_q;
    const existingQueue = queue.slice();
    const originalPush = Array.prototype.push;
    queue.push = (...items: Array<any>) => {
      const output = originalPush.apply(queue, items);
      for (const item of items) processQueueItem(item);
      return output;
    };

    scope._conv_q = queue;
    convert._conv_q = queue;

    for (const item of existingQueue) processQueueItem(item as ChangeCommand);
  };

  const api = ((payload?: unknown, options?: {experience?: ConfigExperience}) => {
    if (isVariationLike(payload)) return applyVariation(payload, options);
    if (isChangeLike(payload) || isChangeArray(payload)) return applyChanges(payload);
    if (typeof payload === 'string') {
      if (payload === 'applyChange' && isChangeLike(options)) {
        return applyChange(options as ChangeLike);
      }
      if (payload === 'applyChanges' && isChangeLike(options)) {
        return applyChanges(options as ChangeLike);
      }
      if (payload === 'runVariation' && isVariationLike(options)) {
        return applyVariation(options as BucketedVariation);
      }
      if (payload === 'run' && isVariationLike(options)) {
        return applyVariation(options as BucketedVariation, {});
      }
      return false;
    }
    return false;
  }) as ToolkitApi;

  api.applyChange = applyChange;
  api.applyChanges = applyChanges;
  api.applyVariation = applyVariation;
  api.run = api;

  if (runtime && isBrowser()) {
    installQueue();
  }

  return api;
};

export const runWithToolkit = (
  convert: Record<string, any>,
  runtime: ConvertStandaloneRuntime
): ((payload?: unknown, options?: {experience?: ConfigExperience}) => boolean | void) => {
  const toolkit = createToolkit(convert, runtime);
  const scope = getWindowScope();
  if (scope) scope.convert = scope.convert || {};
  convert.T = toolkit;
  return toolkit;
};
