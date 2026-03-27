import {
  ConfigExperience,
  ConfigGoal,
  ConversionAttributes,
  GoalData
} from '@convertcom/js-sdk-types';
import {
  ConversionSettingKey,
  GoalDataKey
} from '@convertcom/js-sdk-enums';
import {ConvertStandaloneRuntime} from './runtime';
import {GoalsRender} from './goals-render';

const isRecord = (value: unknown): value is Record<string, any> =>
  !!value && typeof value === 'object' && !Array.isArray(value);

const isGoalArray = (value: unknown): value is Array<string> =>
  Array.isArray(value) && value.every((item) => typeof item === 'string');

const toNumber = (value: unknown): number | undefined => {
  if (typeof value === 'number' && !Number.isNaN(value)) return value;
  if (typeof value === 'string' && value.trim().length) {
    const parsed = Number(value);
    if (!Number.isNaN(parsed)) return parsed;
  }
  return undefined;
};

const getSelectorBy = ({
  action,
  href
}: {
  action?: string;
  href?: string;
}): string | undefined => {
  if (action) return `form[action="${action.replace(/"/g, '\\"')}"]`;
  if (href) return `a[href*="${href.replace(/"/g, '\\"')}"]`;
  return undefined;
};

const isGoalsRenderLike = (value: unknown): value is GoalsRender =>
  isRecord(value) &&
  typeof value.prepareDOMGoalListeners === 'function' &&
  typeof value.prepareScrollGoalListener === 'function' &&
  typeof value.onLocationChange === 'function';

export class Goals {
  private readonly name = 'Goals';

  private _convert: Record<string, any>;
  private _runtime: ConvertStandaloneRuntime;
  private _render: GoalsRender;
  private _gaEventGoals = new Map<string, Set<string>>();
  private _revenueGoalKeys = new Set<string>();
  private _gaInterceptionEnabled = false;
  private _queueInstalled = false;
  private _locationListenerInstalled = false;

  constructor({
    convert,
    runtime,
    render
  }: {
    convert: Record<string, any>;
    runtime: ConvertStandaloneRuntime;
    render?: GoalsRender;
  }) {
    this._convert = convert;
    this._runtime = runtime;
    this._render = isGoalsRenderLike(render)
      ? render
      : isGoalsRenderLike(this._convert.goalsRender)
        ? this._convert.goalsRender
        : new GoalsRender({loggerManager: this._runtime.loggerManager});

    this._convert.goalsRender = this._render;
    this.installApiSurface();
  }

  run(): Goals {
    this.installQueue();
    if (!this._locationListenerInstalled) {
      this._render.onLocationChange(() => this.recheckGoals());
      this._locationListenerInstalled = true;
    }
    this.process();
    this.enableGaInterception();
    return this;
  }

  process(): void {
    const goals = this.getGoals();
    const activeGoalIds = this.getActiveExperienceGoalIds();
    const scrollGoals = new Map<string, number>();

    this._gaEventGoals.clear();
    this._revenueGoalKeys.clear();

    for (const goal of goals) {
      const goalId = String(goal?.id || '');
      const goalKey = String(goal?.key || goalId);
      if (!goalId || !goalKey) continue;
      if (!this.isGoalActive(goal, activeGoalIds)) continue;

      const type = String((goal as Record<string, any>)?.type || '');
      const settings = ((goal as Record<string, any>)?.settings || {}) as Record<
        string,
        any
      >;

      if (type === 'dom_interaction' && Array.isArray(settings.tracked_items)) {
        for (const trackedItem of settings.tracked_items) {
          if (!trackedItem?.selector || !trackedItem?.event) continue;
          this._render.prepareDOMGoalListeners({
            selector: String(trackedItem.selector),
            event: String(trackedItem.event),
            goalId: goalKey,
            callback: (resolvedGoalId) => {
              this.triggerConversion({goalId: resolvedGoalId});
            }
          });
        }
        continue;
      }

      if (type === 'clicks_element') {
        if (!settings.selector) continue;
        this._render.prepareDOMGoalListeners({
          selector: String(settings.selector),
          event: 'click',
          goalId: goalKey,
          callback: (resolvedGoalId) => {
            this.triggerConversion({goalId: resolvedGoalId});
          }
        });
        continue;
      }

      if (type === 'clicks_link' || type === 'submits_form') {
        const selector = getSelectorBy({
          action: settings.action,
          href: settings.href
        });
        if (!selector) continue;
        this._render.prepareDOMGoalListeners({
          selector,
          event: type === 'submits_form' ? 'submit' : 'click',
          goalId: goalKey,
          callback: (resolvedGoalId) => {
            this.triggerConversion({goalId: resolvedGoalId});
          }
        });
        continue;
      }

      if (type === 'scroll_percentage') {
        scrollGoals.set(goalKey, Number(settings.percentage || 0));
        continue;
      }

      if (type === 'ga_import' && settings.ga_event) {
        this.registerGaEventGoal(String(settings.ga_event), goalKey);
        continue;
      }

      if (type === 'revenue' && String(settings.triggering_type) === 'ga') {
        this._revenueGoalKeys.add(goalKey);
      }
    }

    if (scrollGoals.size) {
      this._render.prepareScrollGoalListener({
        goals: Array.from(scrollGoals.keys()),
        getPercentage: (goalId) => scrollGoals.get(goalId) || 0,
        callback: ({goalId}) => {
          if (isGoalArray(goalId) && goalId.length) {
            this.triggerConversions({goalIds: goalId});
          }
        }
      });
    }
  }

  triggerConversion(...args: any[]): boolean {
    const params = isRecord(args[0]) ? args[0] : {goalId: args[0]};
    const goalRef = params.goalKey || params.goalId;
    if (Array.isArray(goalRef)) {
      return goalRef
        .map((item) => this.triggerConversion({...params, goalId: item}))
        .some(Boolean);
    }
    return this.trackGoal(goalRef, {
      ruleData: params.ruleData,
      conversionData: params.conversionData,
      conversionSetting: params.conversionSetting
    });
  }

  triggerConversions(...args: any[]): boolean {
    const params = isRecord(args[0]) ? args[0] : {goalIds: args[0]};
    const goalRefs = params.goalIds || params.goalKeys || params.goalId;
    if (!Array.isArray(goalRefs)) {
      return this.triggerConversion(params);
    }
    return goalRefs
      .map((goalRef) =>
        this.trackGoal(goalRef, {
          ruleData: params.ruleData,
          conversionData: params.conversionData,
          conversionSetting: params.conversionSetting
        })
      )
      .some(Boolean);
  }

  sendRevenue(...args: any[]): boolean {
    if (isRecord(args[0])) {
      return this.trackRevenue(args[0]);
    }
    return this.trackRevenue({
      transactionId: args[0],
      amount: args[1],
      productsCount: args[2],
      goalId: args[3],
      forceMultiple: args[4]
    });
  }

  pushRevenue(...args: any[]): boolean {
    if (isRecord(args[0])) {
      return this.trackRevenue(args[0]);
    }
    return this.trackRevenue({
      amount: args[0],
      productsCount: args[1],
      goalId: args[2],
      forceMultiple: args[3],
      transactionId: args[4]
    });
  }

  recheckGoals(): boolean {
    this.process();
    return true;
  }

  enableGaInterception(): void {
    if (this._gaInterceptionEnabled || typeof window === 'undefined') return;
    this._gaInterceptionEnabled = true;

    const dataLayerName = this.getDataLayerName();
    const dataLayer = ((window as Record<string, any>)[dataLayerName] ||= []);
    if (Array.isArray(dataLayer)) {
      for (const payload of dataLayer) this.captureGaPayload(payload);
      const originalPush = dataLayer.push.bind(dataLayer);
      dataLayer.push = (...items: Array<any>) => {
        for (const payload of items) this.captureGaPayload(payload);
        return originalPush(...items);
      };
    }

    const gaq = (((window as Record<string, any>)._gaq ||= []) as Array<any>);
    if (Array.isArray(gaq)) {
      for (const payload of gaq) this.captureGaPayload(payload);
      const originalPush = gaq.push.bind(gaq);
      gaq.push = (...items: Array<any>) => {
        for (const payload of items) this.captureGaPayload(payload);
        return originalPush(...items);
      };
    }

    const ga = (window as Record<string, any>).ga;
    if (typeof ga === 'function' && Array.isArray((ga as Record<string, any>).q)) {
      const queue = (ga as Record<string, any>).q as Array<any>;
      for (const payload of queue) this.captureGaPayload(payload);
      const originalPush = queue.push.bind(queue);
      queue.push = (...items: Array<any>) => {
        for (const payload of items) this.captureGaPayload(payload);
        return originalPush(...items);
      };
    }
  }

  private installApiSurface(): void {
    this._convert.triggerConversion = (...args: any[]) =>
      this.triggerConversion(...args);
    this._convert.triggerConversions = (...args: any[]) =>
      this.triggerConversions(...args);
    this._convert.sendRevenue = (...args: any[]) => this.sendRevenue(...args);
    this._convert.pushRevenue = (...args: any[]) => this.pushRevenue(...args);
    this._convert.recheckGoals = () => this.recheckGoals();
    this._convert.recheck_goals = () => this.recheckGoals();
  }

  private installQueue(): void {
    if (this._queueInstalled || typeof window === 'undefined') return;
    this._queueInstalled = true;

    const processItem = (item: any) => {
      if (!isRecord(item)) return;
      const what = String(item.what || '');
      const params = isRecord(item.params) ? item.params : {};
      switch (what) {
        case 'triggerConversion':
          this.triggerConversion(params);
          break;
        case 'triggerConversions':
          this.triggerConversions(params);
          break;
        case 'sendRevenue':
          this.sendRevenue(params);
          break;
        case 'pushRevenue':
          this.pushRevenue(params);
          break;
        case 'recheckGoals':
        case 'recheck_goals':
          this.recheckGoals();
          break;
        default:
          break;
      }
    };

    const existingQueue = Array.isArray((window as Record<string, any>)._conv_q)
      ? ((window as Record<string, any>)._conv_q as Array<any>).slice()
      : [];
    const queue = Array.isArray((window as Record<string, any>)._conv_q)
      ? ((window as Record<string, any>)._conv_q as Array<any>)
      : [];
    const originalPush = Array.prototype.push;

    queue.push = (...items: Array<any>) => {
      const size = originalPush.apply(queue, items);
      for (const item of items) processItem(item);
      return size;
    };

    (window as Record<string, any>)._conv_q = queue;
    this._convert._conv_q = queue;

    for (const item of existingQueue) processItem(item);
  }

  private trackGoal(
    goalRef: string,
    attributes?: ConversionAttributes
  ): boolean {
    const goalKey = this.resolveGoalKey(goalRef);
    const context = this._convert.visitor?.context;
    if (!goalKey || !context) return false;
    context.trackConversion(goalKey, attributes);
    return true;
  }

  private trackRevenue(params: Record<string, any>): boolean {
    const goalRefs = params.goalId
      ? [params.goalId]
      : Array.from(this._revenueGoalKeys.values());
    if (!goalRefs.length) return false;

    const conversionData = this.buildRevenueConversionData(params);
    const conversionSetting = params.forceMultiple
      ? {
          [ConversionSettingKey.FORCE_MULTIPLE_TRANSACTIONS]: true
        }
      : undefined;

    return goalRefs
      .map((goalRef) =>
        this.trackGoal(goalRef, {
          conversionData,
          conversionSetting
        })
      )
      .some(Boolean);
  }

  private buildRevenueConversionData(params: Record<string, any>): Array<GoalData> {
    const conversionData = Array.isArray(params.conversionData)
      ? [...params.conversionData]
      : [];
    const amount = toNumber(params.amount ?? params.value ?? params.revenue);
    const productsCount = toNumber(
      params.productsCount ?? params.products_count ?? params.quantity
    );
    const transactionId = params.transactionId || params.transaction_id || params.id;

    if (amount !== undefined) {
      conversionData.push({key: GoalDataKey.AMOUNT, value: amount});
    }
    if (productsCount !== undefined) {
      conversionData.push({
        key: GoalDataKey.PRODUCTS_COUNT,
        value: productsCount
      });
    }
    if (transactionId !== undefined) {
      conversionData.push({
        key: GoalDataKey.TRANSACTION_ID,
        value: String(transactionId)
      });
    }

    return conversionData;
  }

  private registerGaEventGoal(eventName: string, goalKey: string): void {
    if (!this._gaEventGoals.has(eventName)) {
      this._gaEventGoals.set(eventName, new Set<string>());
    }
    this._gaEventGoals.get(eventName)?.add(goalKey);
  }

  private captureGaPayload(payload: any): void {
    const captured = this.normalizeGaPayload(payload);
    if (!captured?.eventName) return;

    const mappedGoals = Array.from(
      this._gaEventGoals.get(captured.eventName)?.values() || []
    );
    if (mappedGoals.length) {
      this.triggerConversions({goalIds: mappedGoals});
    }

    if (captured.eventName === 'purchase' && this._revenueGoalKeys.size) {
      const revenueParams = this.extractRevenueParams(captured.params);
      for (const goalKey of this._revenueGoalKeys) {
        this.trackRevenue({goalId: goalKey, ...revenueParams});
      }
    }
  }

  private normalizeGaPayload(
    payload: any
  ): {eventName?: string; params?: Record<string, any>} | null {
    if (Array.isArray(payload)) {
      if (payload[0] === 'event' && typeof payload[1] === 'string') {
        return {
          eventName: payload[1],
          params: isRecord(payload[2]) ? payload[2] : {}
        };
      }
      if (payload[0] === '_trackEvent') {
        return {
          eventName: String(payload[2] || payload[1] || ''),
          params: {}
        };
      }
      if (typeof payload[0] === 'string' && payload[0]) {
        return {
          eventName: payload[0],
          params: isRecord(payload[1]) ? payload[1] : {}
        };
      }
      return null;
    }

    if (isRecord(payload) && typeof payload.event === 'string') {
      return {
        eventName: payload.event,
        params: payload
      };
    }

    return null;
  }

  private extractRevenueParams(params: Record<string, any> = {}): Record<string, any> {
    const ecommerce = isRecord(params.ecommerce) ? params.ecommerce : {};
    const purchase = isRecord(ecommerce.purchase) ? ecommerce.purchase : {};
    const actionField = isRecord(purchase.actionField)
      ? purchase.actionField
      : {};
    const items = Array.isArray(params.items)
      ? params.items
      : Array.isArray(ecommerce.items)
        ? ecommerce.items
        : Array.isArray(purchase.products)
          ? purchase.products
          : [];

    return {
      transactionId:
        params.transaction_id ||
        params.transactionId ||
        ecommerce.transaction_id ||
        actionField.id,
      amount:
        params.value ||
        params.revenue ||
        ecommerce.value ||
        actionField.revenue,
      productsCount:
        params.productsCount ||
        params.quantity ||
        ecommerce.quantity ||
        items.length
    };
  }

  private getGoals(): Array<ConfigGoal> {
    return (this._runtime.dataManager.getEntitiesList('goals') || []) as Array<ConfigGoal>;
  }

  private getActiveExperienceGoalIds(): Set<string> {
    const context = this._convert.visitor?.context;
    if (!context) return new Set<string>();

    const storeData = context.getVisitorData();
    const bucketing = isRecord(storeData?.bucketing) ? storeData.bucketing : {};
    const activeGoalIds = new Set<string>();

    for (const experienceId of Object.keys(bucketing)) {
      const experience = this._runtime.dataManager.getEntityById(
        experienceId,
        'experiences'
      ) as ConfigExperience;
      for (const goalId of experience?.goals || []) {
        activeGoalIds.add(String(goalId));
      }
    }

    return activeGoalIds;
  }

  private isGoalActive(goal: ConfigGoal, activeGoalIds: Set<string>): boolean {
    if (!activeGoalIds.size) return false;
    const goalId = String(goal?.id || '');
    const goalKey = String(goal?.key || '');
    return activeGoalIds.has(goalId) || activeGoalIds.has(goalKey);
  }

  private resolveGoalKey(goalRef: string): string | null {
    if (!goalRef) return null;
    const byId = this._runtime.dataManager.getEntityById(goalRef, 'goals') as
      | ConfigGoal
      | null;
    if (byId?.key) return byId.key;
    const byKey = this._runtime.dataManager.getEntity(goalRef, 'goals') as
      | ConfigGoal
      | null;
    if (byKey?.key) return byKey.key;
    return goalRef;
  }

  private getDataLayerName(): string {
    const integrationVariables = isRecord(this._convert.integrationVariables)
      ? this._convert.integrationVariables
      : {};
    return typeof integrationVariables.googleAnalytics === 'string'
      ? integrationVariables.googleAnalytics
      : 'dataLayer';
  }
}
