/* eslint-disable no-console */
import {expect} from 'chai';
import {runToolkitEntry} from './lib/static/toolkit';
import testConfig from './tests/test-config.json';

const bootstrapConfig = () => ({
  ...testConfig,
  bootstrap: {config: testConfig}
});

const resetDomState = () => {
  document.querySelectorAll('style').forEach((style) => style.remove());
};

const waitForTurn = () =>
  new Promise((resolve) => {
    setTimeout(resolve, 0);
  });

export default function runToolkitBrowserTests() {
  describe('Karma browser tests for toolkit bundle', function () {
    beforeEach(async function () {
      window.convert = {};
      window._conv_q = [];
      window.convert._conv_q = [];
      window.convert.bootstrap = bootstrapConfig();
      resetDomState();
      window.__convertToolkitScriptRan = false;
      window.__convertToolkitQueueRan = false;
      await runToolkitEntry();
    });

    it('Should expose convert.T with applyChange API', function () {
      expect(window.convert).to.have.property('T').that.is.a('function');
      expect(window.convert.T.applyChange).to.be.a('function');
    });

    it('Should execute CSS + JS payload from convert.T.applyChange without full SDK', function () {
      window.convert.T.applyChange({
        type: 'css',
        data: {
          css: '#convertToolkitTest{display:none;}'
        }
      });

      const marker = document.getElementById('convertToolkitStyle');
      const markerStyle =
        marker?.style?.getPropertyValue('display') || marker?.getAttribute('style');
      expect(Boolean(markerStyle)).to.equal(false);
      expect(
        Array.from(document.getElementsByTagName('style')).some((el) =>
          String(el.textContent || '').includes('convertToolkitTest')
        )
      ).to.equal(true);

      window.convert.T.applyChange({
        type: 'js',
        data: {js: 'window.__convertToolkitScriptRan = true;'}
      });
      expect(window.__convertToolkitScriptRan).to.equal(true);
    });

    it('Should process queued convert toolkit commands', async function () {
      window._conv_q.push({
        what: 'applyChange',
        params: {
          type: 'js',
          data: {
            js: 'window.__convertToolkitQueueRan = true;'
          }
        }
      });
      await waitForTurn();
      expect(window.__convertToolkitQueueRan).to.equal(true);
    });

    it('Should execute queued tuple commands from window._conv_q', async function () {
      window._conv_q.push([
        'applyChange',
        {
          type: 'js',
          data: {js: 'window.__convertToolkitQueueRan = true;'}
        }
      ]);
      await waitForTurn();
      expect(window.__convertToolkitQueueRan).to.equal(true);
    });

    it('Should execute variation payload through convert.T API without tracking script', function () {
      window.convert.T({
        id: 'variation-1',
        key: 'variation-key',
        changes: [
          {
            type: 'js',
            data: {
              js: 'window.__convertToolkitVariationRan = true;'
            }
          }
        ]
      });
      expect(window.__convertToolkitVariationRan).to.equal(true);
    });
  });
}
