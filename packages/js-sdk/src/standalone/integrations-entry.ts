import {Goals} from './goals';
import {GoalsRender} from './goals-render';
import {IntegrationsProcessor} from './integrations';
import {
  ensureConvertWindow,
  initializeVisitorRuntime
} from './runtime';

const isRecord = (value: unknown): value is Record<string, any> =>
  !!value && typeof value === 'object' && !Array.isArray(value);

const isGoalsLike = (value: unknown): value is Goals =>
  isRecord(value) &&
  typeof value.run === 'function' &&
  typeof value.enableGaInterception === 'function';

const isGoalsRenderLike = (value: unknown): value is GoalsRender =>
  isRecord(value) &&
  typeof value.prepareDOMGoalListeners === 'function' &&
  typeof value.prepareScrollGoalListener === 'function' &&
  typeof value.onLocationChange === 'function';

const isIntegrationsProcessorLike = (
  value: unknown
): value is IntegrationsProcessor =>
  isRecord(value) && typeof value.run === 'function';

export const runIntegrationsEntry = async () => {
  await initializeVisitorRuntime();

  const convert = ensureConvertWindow();
  convert.Goals = Goals;
  convert.GoalsRender = GoalsRender;
  convert.IntegrationsProcessor = IntegrationsProcessor;

  convert.runIntegrations = () => {
    const runtime = convert.remote;
    if (!runtime) return null;

    if (!isGoalsRenderLike(convert.goalsRender)) {
      convert.goalsRender = new GoalsRender({
        loggerManager: runtime.loggerManager
      });
    }
    if (!isGoalsLike(convert.goals)) {
      convert.goals = new Goals({
        convert,
        runtime,
        render: convert.goalsRender
      });
    }
    if (!isIntegrationsProcessorLike(convert.integrationsProcessor)) {
      convert.integrationsProcessor = new IntegrationsProcessor({
        convert,
        runtime
      });
    }

    return convert.integrationsProcessor.run();
  };

  return convert.runIntegrations;
};

void runIntegrationsEntry();
