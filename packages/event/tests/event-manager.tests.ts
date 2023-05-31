import 'mocha';
// import * as chai from 'chai';
import {expect} from 'chai';
import {assert} from 'chai';
import chai from 'chai';
import spies from 'chai-spies';

import {EventManager as em} from '../src/event-manager';

import {Config} from '@convertcom/types';

import configuration from './test-config.json';

chai.use(spies);

describe('EventManager tests', function () {
  let eventManager;

  it('Should expose EventManager', function () {
    assert.isDefined(em);
  });
  it('Imported entity should be a constructor of EventManager instance', function () {
    expect(em)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('EventManager');
  });
  it('Should successfully create new EventManager instance with default config', function () {
    eventManager = new em(<Config>{}, {});
    expect(eventManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('EventManager');
  });
  it('Should create new EventManager instance', function () {
    eventManager = new em(configuration as unknown as Config, {});
    expect(eventManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('EventManager');
  });
  it('Should subscribe to event and be fired with provided data and no errors', function () {
    const args = {
      foo: 'bar',
      some: {
        test: {
          data: 'value'
        }
      }
    };
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const fnA = chai.spy((inputArgs, err) => {
      expect(inputArgs).to.be.deep.equal(args);
      expect(err).to.be.a('null');
    });
    eventManager.on('EVENT1', fnA);
    eventManager.fire('EVENT1', args);
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    chai.expect(fnA).to.have.been.called.once;
  });
  it('Should not be fired because event listeners are removed', function () {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const fnA = chai.spy((inputArgs, err) => {
      expect(err).to.be.a('null');
    });
    eventManager.on('EVENT2', fnA);
    eventManager.removeListeners('EVENT2');
    eventManager.fire('EVENT2', {});
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    chai.expect(fnA).to.not.have.been.called.once;
  });
  it('Deferred event listener should be fired even if subscribed after the event', function (done) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const fnA = chai.spy((inputArgs, err) => {
      expect(err).to.be.a('null');
    });
    eventManager.fire('EVENT2', {}, null, true);
    setTimeout(() => {
      eventManager.on('EVENT2', fnA);
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      chai.expect(fnA).to.have.been.called.once;
      done();
    }, 100);
  });
  it('Should subscribe to event and be fired with error provided', function (done) {
    eventManager.on('EVENT3', (inputArgs, err) => {
      expect(err).to.be.an.instanceOf(Error);
      expect(inputArgs).to.be.a('null');
      done();
    });
    eventManager.fire('EVENT3', null, new Error('Custom error message'));
  });
});
