/* eslint-disable mocha/consistent-spacing-between-blocks */
import {expect} from 'chai';
import testConfig from './tests/test-config.json';
import {runGoalsEntry} from './lib/goals-entry';
import {runIntegrationsEntry} from './lib/integrations-entry.umd';
import {runSplitEntry} from './lib/split-entry';

const waitForTurn = () =>
  new Promise((resolve) => {
    setTimeout(resolve, 0);
  });

const SPLIT_COOKIE = '_conv_sptest';

const hashLocation = (value) => {
  let hash = 5381;
  for (let i = 0; i < value.length; i++) {
    hash = (hash * 33) ^ value.charCodeAt(i);
  }
  return (hash >>> 0).toString(16);
};

const cloneConfig = (data) => JSON.parse(JSON.stringify(data));

const baseData = () => ({
  account_id: testConfig.data.account_id,
  project: testConfig.data.project,
  goals: [],
  experiences: [],
  audiences: [],
  segments: [],
  features: [],
  locations: []
});

const resetConvert = (data) => {
  if (window.convert?.goalsRender?.destroy) {
    window.convert.goalsRender.destroy();
  }

  window.convert = {
    bootstrap: {
      config: {
        data
      }
    },
    data: data,
    config: {
      data
    },
    remote: null,
    _conv_q: [],
    ruleData: {
      url: window.location.href
    }
  };

  window._conv_q = [];
  window.convert._conv_q = window._conv_q;
  delete window.convert.redirect;
  delete window.convert.refresh;
};

const readSplitCookie = () => {
  const cookie = document.cookie
    .split('; ')
    .find((item) => item.startsWith(`${SPLIT_COOKIE}=`));
  if (!cookie) return null;
  try {
    return JSON.parse(decodeURIComponent(cookie.split('=').slice(1).join('=')));
  } catch {
    return null;
  }
};

const writeSplitCookie = (payload) => {
  document.cookie = `${SPLIT_COOKIE}=${encodeURIComponent(
    JSON.stringify(payload)
  )}; path=/; SameSite=Lax`;
};

const clearSplitCookie = () => {
  document.cookie = `${SPLIT_COOKIE}=; Max-Age=0; path=/; SameSite=Lax`;
};

const splitExperience = ({variationPattern, originalPattern}) => ({
  id: '100900001',
  key: 'split-web-experience',
  name: 'Split URL Experience',
  type: 'split_url',
  version: 1,
  status: 'active',
  url: window.location.href,
  environments: ['staging', 'live'],
  variations: [
    {
      id: '100900001-var-1',
      name: 'Variation 1',
      status: 'running',
      is_baseline: true,
      key: '100900001-var-1',
      traffic_allocation: 100,
      changes: [
        {
          id: '100900001-change-1',
          type: 'defaultRedirect',
          data: {
            original_pattern: originalPattern,
            variation_pattern: variationPattern,
            case_sensitive: false
          }
        }
      ]
    }
  ]
});

const goalEntity = ({type, selector}) => ({
  id: '100900010',
  key: 'goal-click',
  name: 'Goal click',
  status: 'active',
  type,
  is_system: false,
  selected_default: true,
  settings: {
    selector,
    ga_event: 'goal_reached'
  }
});

const webExperience = ({goalIds, integrations}) => ({
  id: '100900011',
  key: 'web-goal-experience',
  name: 'Web Goal Experience',
  type: 'a/b',
  version: 1,
  status: 'active',
  url: 'https://convert.com',
  environment: 'staging',
  integrations,
  variations: [
    {
      id: '100900011-var-1',
      name: 'Variation 1',
      status: 'running',
      is_baseline: true,
      key: '100900011-var-1',
      traffic_allocation: 100,
      changes: []
    }
  ],
  goals: goalIds
});

const buildGoalsConfig = ({goalType, integrationMap, selector, useGaEvent}) => {
  const data = baseData();
  const primaryGoal = goalEntity({
    type: goalType || 'clicks_element',
    selector: selector || '#goalTarget'
  });
  const extraGoal = useGaEvent
    ? {
        id: '100900011',
        key: 'goal-visit',
        name: 'Goal visit',
        status: 'active',
        type: 'ga_import',
        is_system: false,
        selected_default: true,
        settings: {
          ga_event: 'purchase'
        }
      }
    : null;

  const goals = extraGoal ? [primaryGoal, extraGoal] : [primaryGoal];

  data.goals = goals;
  data.experiences = [
    webExperience({
      goalIds: goals.map(({key}) => key),
      integrations: integrationMap || {}
    })
  ];

  data.project = {
    ...testConfig.data.project,
    settings: {
      ...testConfig.data.project.settings,
      ...(useGaEvent
        ? {integrations: {google_analytics: {enabled: true}}}
        : {})
    }
  };

  return data;
};

const projectIntegrationData = {
  ...testConfig.data.project,
  settings: {
    ...testConfig.data.project.settings,
    integrations: {
      project_tracking: {
        enabled: true
      }
    }
  }
};

describe('Karma browser tests for standalone bundles', function () {
  beforeEach(function () {
    window.convert = {};
    window._conv_q = [];
    document.body.innerHTML = '';
    if (document.cookie.includes(`${SPLIT_COOKIE}=`)) {
      clearSplitCookie();
    }
  });

  describe('split-entry', function () {
    it('Should parse _conv_sptest and clear destination cookie', async function () {
      const currentUrl = window.location.href;
      const data = baseData();
      data.experiences = [
        splitExperience({
          originalPattern: window.location.origin,
          variationPattern: `${window.location.origin}/split-target`
        })
      ];
      resetConvert(data);

      const payload = {
        destinationUrl: `${window.location.origin}/split-target`,
        experienceKey: 'split-web-experience',
        fromHash: hashLocation('origin'),
        toHash: hashLocation(currentUrl),
        timestamp: Date.now(),
        variationKey: '100900001-var-1'
      };
      writeSplitCookie(payload);

      const result = await runSplitEntry();

      expect(result).to.have.property('split-web-experience');
      expect(readSplitCookie()).to.equal(null);
      expect(window.convert.splitTests).to.have.property('split-web-experience');
      expect(window.convert.splitTests['split-web-experience'].toHash).to.equal(
        hashLocation(currentUrl)
      );
      expect(result).to.equal(window.convert.splitTests);
    });

    it('Should ignore destination cookie when target hash does not match and skip redirect', async function () {
      const currentUrl = window.location.href;
      const data = baseData();
      data.experiences = [
        splitExperience({
          originalPattern: 'https://example.com/miss',
          variationPattern: `${window.location.origin}/split-target`
        })
      ];
      resetConvert(data);

      let redirectHref = '';
      window.convert.redirect = (url) => {
        redirectHref = url;
      };

      const payload = {
        destinationUrl: `${window.location.origin}/split-target`,
        experienceKey: 'split-web-experience',
        fromHash: hashLocation('origin'),
        toHash: hashLocation(currentUrl + '?mismatch'),
        timestamp: Date.now(),
        variationKey: '100900001-var-1'
      };
      writeSplitCookie(payload);

      const result = await runSplitEntry();

      expect(readSplitCookie()).to.equal(null);
      expect(redirectHref).to.equal('');
      expect(result).to.not.have.property('split-web-experience');
      expect(window.convert.splitTests).to.deep.equal({});
    });

    it('Should execute redirect from origin page when split rules match', async function () {
      window.history.replaceState({}, '', '/');
      const currentUrl = window.location.href;
      const data = baseData();
      data.experiences = [
        splitExperience({
          originalPattern: window.location.origin,
          variationPattern: '/split-target'
        })
      ];
      resetConvert(data);

      let redirectUrl = '';
      window.convert.redirect = (url) => {
        redirectUrl = url;
      };

      const result = await runSplitEntry();

      await waitForTurn();
      const splitState = window.convert.splitTests['split-web-experience'];

      expect(redirectUrl).to.include('/split-target');
      expect(splitState).to.be.an('object');
      expect(splitState.experienceId).to.equal('100900001');
      expect(splitState.fromHash).to.equal(hashLocation(currentUrl));
      expect(result).to.equal(window.convert.splitTests);
      expect(readSplitCookie()).to.be.an('object');
    });
  });

  describe('goals-entry', function () {
    it('Should process _conv_q commands when runGoals is called', async function () {
      const data = buildGoalsConfig({
        goalType: 'clicks_element',
        selector: '#goalTarget'
      });
      data.experiences[0].goals = ['goal-click'];
      data.goals = [
        {
          id: '100900010',
          key: 'goal-click',
          name: 'Goal click',
          status: 'active',
          type: 'clicks_element',
          is_system: false,
          selected_default: true,
          settings: {
            selector: '#goalTarget'
          }
        }
      ];
      resetConvert(data);

      const runGoals = await runGoalsEntry();
      expect(runGoals).to.be.a('function');

      const context = window.convert.visitor.context;
      context.runExperiences({locationProperties: {url: window.location.href}});

      let conversionCalls = 0;
      context.trackConversion = () => {
        conversionCalls += 1;
        return true;
      };

      runGoals();
      window._conv_q.push({what: 'triggerConversion', params: {goalId: 'goal-click'}});

      await waitForTurn();
      expect(conversionCalls).to.equal(1);
    });

    it('Should activate DOM listeners and register GA intercept for click and GA payloads', async function () {
      const data = buildGoalsConfig({
        goalType: 'clicks_element',
        selector: '#goalTarget',
        useGaEvent: true
      });
      resetConvert(data);

      const triggerButton = document.createElement('button');
      triggerButton.id = 'goalTarget';
      triggerButton.textContent = 'Trigger';
      document.body.appendChild(triggerButton);

      const runGoals = await runGoalsEntry();
      const context = window.convert.visitor.context;
      context.runExperiences({locationProperties: {url: window.location.href}});

      let conversionCalls = 0;
      context.trackConversion = () => {
        conversionCalls += 1;
        return true;
      };

      runGoals();
      await waitForTurn();

      triggerButton.dispatchEvent(
        new MouseEvent('click', {
          bubbles: true,
          cancelable: true
        })
      );

      await waitForTurn();
      window.dataLayer.push(['event', 'purchase']);
      await waitForTurn();

      expect(conversionCalls).to.equal(2);
      expect(window.dataLayer.push).to.not.equal(Array.prototype.push);
      expect(window.dataLayer).to.have.length.of.at.least(1);
    });
  });

  describe('integrations-entry', function () {
    it('Should build and expose integrations map during runIntegrations()', async function () {
      const data = buildGoalsConfig({goalType: 'clicks_element', selector: '#goalTarget'});
      data.experiences = [
        webExperience({
          goalIds: ['goal-click'],
          integrations: {
            custom_widget: {enabled: true}
          }
        })
      ];
      data.project = projectIntegrationData;
      data.goals = [
        {
          id: '100900010',
          key: 'goal-click',
          name: 'Goal click',
          status: 'active',
          type: 'clicks_element',
          is_system: false,
          selected_default: true,
          settings: {
            selector: '#goalTarget'
          }
        }
      ];
      resetConvert(data);

      const runIntegrations = await runIntegrationsEntry();
      expect(runIntegrations).to.be.a('function');

      const processor = runIntegrations();
      const integrations = processor?.integrations || {};

      expect(integrations).to.include.keys(
        'google_analytics',
        'google_tag_manager',
        'custom_widget',
        'project_tracking'
      );
      expect(Object.prototype.hasOwnProperty.call(window.dataLayer, 'push')).to.equal(
        true
      );
      expect(window.convert.integrations).to.deep.equal(integrations);
    });
  });
});
