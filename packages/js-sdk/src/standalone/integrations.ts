import {ConfigExperience} from '@convertcom/js-sdk-types';
import {ConvertStandaloneRuntime} from './runtime';
import {Goals} from './goals';

const isRecord = (value: unknown): value is Record<string, any> =>
  !!value && typeof value === 'object' && !Array.isArray(value);

const isGoalsLike = (value: unknown): value is Goals =>
  isRecord(value) &&
  typeof value.run === 'function' &&
  typeof value.enableGaInterception === 'function';

export class IntegrationsProcessor {
  private readonly name = 'Integrations';

  private _convert: Record<string, any>;
  private _runtime: ConvertStandaloneRuntime;
  private _integrations: Record<
    string,
    {name: string; enabled: boolean; process: () => void}
  > = {};

  constructor({
    convert,
    runtime
  }: {
    convert: Record<string, any>;
    runtime: ConvertStandaloneRuntime;
  }) {
    this._convert = convert;
    this._runtime = runtime;
    this._integrations = this.buildIntegrations();
  }

  run(): IntegrationsProcessor {
    const goals = this.ensureGoals();
    goals.enableGaInterception();

    for (const integration of Object.values(this._integrations)) {
      integration.process();
    }

    this._convert.integrations = this._integrations;
    return this;
  }

  get integrations(): Record<string, {name: string; enabled: boolean; process: () => void}> {
    return this._integrations;
  }

  private ensureGoals(): Goals {
    if (!isGoalsLike(this._convert.goals)) {
      this._convert.goals = new Goals({
        convert: this._convert,
        runtime: this._runtime
      });
    }
    return this._convert.goals as Goals;
  }

  private buildIntegrations(): Record<
    string,
    {name: string; enabled: boolean; process: () => void}
  > {
    const integrations: Record<
      string,
      {name: string; enabled: boolean; process: () => void}
    > = {
      google_analytics: {
        name: 'google_analytics',
        enabled: true,
        process: () => this.ensureGoals().enableGaInterception()
      },
      google_tag_manager: {
        name: 'google_tag_manager',
        enabled: true,
        process: () => this.ensureGoals().enableGaInterception()
      }
    };

    const experiences = (this._runtime.dataManager.getEntitiesList('experiences') || []) as Array<ConfigExperience>;
    for (const experience of experiences) {
      const experienceIntegrations = isRecord(experience?.integrations)
        ? experience.integrations
        : {};
      for (const key of Object.keys(experienceIntegrations)) {
        if (!integrations[key]) {
          integrations[key] = {
            name: key,
            enabled: true,
            process: () => undefined
          };
        }
      }
    }

    const projectIntegrations = isRecord(
      (this._runtime.dataManager.data as Record<string, any>)?.project?.settings
        ?.integrations
    )
      ? ((this._runtime.dataManager.data as Record<string, any>).project.settings
          .integrations as Record<string, any>)
      : {};
    for (const key of Object.keys(projectIntegrations)) {
      if (!integrations[key]) {
        integrations[key] = {
          name: key,
          enabled: true,
          process: () => undefined
        };
      }
    }

    this._runtime.loggerManager?.debug?.(
      `${this.name}.buildIntegrations()`,
      Object.keys(integrations)
    );

    return integrations;
  }
}
