import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {SystemEvents} from '@convertcom/js-sdk-enums';
import testConfig from './test-config.json';
import {Config} from '@convertcom/js-sdk-types';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = {
  ...testConfig,
  tracking: true,
  api: {
    endpoint: {
      config: host + ':' + port,
      track: host + ':' + port
    }
  },
  events: {
    batch_size: batch_size,
    release_interval: release_timeout
  }
} as unknown as Config;
const eventManager = new em(configuration);
describe('ApiManager tests', function () {
  it('Should expose ApiManager', function () {
    assert.isDefined(am);
  });
  it('Imported entity should be a constructor of ApiManager instance', function () {
    expect(am)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('ApiManager');
  });
  let apiManager;

  it('Should successfully create new ApiManager instance with default config', async function () {
    const apiManager = new am();
    expect(apiManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('ApiManager');
  });

  it('Should create new ApiManager instance with visitor provided configuration and EvenManager dependency', async function () {
    apiManager = new am(configuration, {eventManager});
    expect(apiManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('ApiManager');
  });

  describe('Test API Manager request', function () {
    let server;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      server = http.createServer();
      server.listen(port);
    });
    it('Should successfully send test JSON payload', function (done) {
      const testPayload = {
        foo: 'bar',
        some: {
          test: {
            data: 'value'
          }
        }
      };
      server.on('request', (request, res) => {
        if (request.url === '/test') {
          const body = [];
          request
            .on('data', (chunk) => {
              body.push(chunk);
            })
            .on('end', () => {
              const data = JSON.parse(Buffer.concat(body).toString());
              expect(data).to.be.deep.equal(testPayload);
              done();
            });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
      apiManager.request(
        'post',
        {base: host + ':' + port, route: '/test'},
        testPayload
      );
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    after(function () {
      server.close();
    });
  });

  describe('Test requests enqueuing', function () {
    let server;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    afterEach(function () {
      server.close();
    });

    const VID = '1';
    const EXP = '11';
    const VAR = '12';
    const N = batch_size - 2;
    it(
      N +
        ' enqueued requests should be released before exceeding ' +
        test_timeout +
        'ms of timeout. Release interval is ' +
        release_timeout +
        'ms',
      function (done) {
        this.timeout(test_timeout);
        const requestData = {
          eventType: 'bucketing',
          data: {
            experienceId: EXP,
            variationId: VAR
          }
        };
        for (let i = 1; i <= N; i++) {
          apiManager.enqueue(VID + i, requestData);
        }
        server.on('request', (request, res) => {
          if (request.url.startsWith('/track')) {
            const body = [];
            request
              .on('data', (chunk) => {
                body.push(chunk);
              })
              .on('end', () => {
                const data = JSON.parse(Buffer.concat(body).toString());
                expect(data)
                  .to.be.an('object')
                  .that.haveOwnProperty('visitors')
                  .to.be.an('array')
                  .and.has.length(N);
                data.visitors.every((visitor) =>
                  expect(visitor)
                    .to.haveOwnProperty('events')
                    .that.is.an('array')
                    .that.deep.includes(requestData)
                );
                done();
              });
          }
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end();
        });
      }
    );
    it(
      batch_size +
        ' enqueued requests should be released before exceeding ' +
        release_timeout +
        'ms of timeout because batch size limit is ' +
        batch_size,
      function (done) {
        this.timeout(release_timeout);
        const requestData = {
          eventType: 'bucketing',
          data: {
            experienceId: EXP,
            variationId: VAR
          }
        };
        server.on('request', (request, res) => {
          if (request.url.startsWith('/track')) {
            const body = [];
            request
              .on('data', (chunk) => {
                body.push(chunk);
              })
              .on('end', () => {
                const data = JSON.parse(Buffer.concat(body).toString());
                expect(data)
                  .to.be.an('object')
                  .that.haveOwnProperty('visitors')
                  .to.be.an('array')
                  .and.has.length(batch_size);
                done();
              });
          }
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end();
        });
        for (let i = 1; i <= batch_size; i++) {
          apiManager.enqueue(VID + i, requestData);
        }
      }
    );
    it('Should fire the event when enqueued are released because of size and has server response in passed arguments', function (done) {
      const requestData = {
        eventType: 'bucketing',
        data: {
          experienceId: EXP,
          variationId: VAR
        }
      };
      const serverResponseExample = {
        data: 'ok'
      };
      eventManager.on(SystemEvents.API_QUEUE_RELEASED, function (args, err) {
        expect(args).to.haveOwnProperty('reason').that.equal('size');
        expect(args)
          .to.haveOwnProperty('result')
          .that.haveOwnProperty('data')
          .that.deep.equal(serverResponseExample);
        expect(err).to.be.a('null');
        eventManager.removeListeners(SystemEvents.API_QUEUE_RELEASED);
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        }
      });
      for (let i = 1; i <= batch_size; i++) {
        apiManager.enqueue(VID + i, requestData);
      }
    });
    it('Should fire the event when enqueued are released because of release timeout and has server response in passed arguments', function (done) {
      this.timeout(test_timeout);
      const requestData = {
        eventType: 'bucketing',
        data: {
          experienceId: EXP,
          variationId: VAR
        }
      };
      const serverResponseExample = {
        data: 'ok'
      };
      eventManager.on(SystemEvents.API_QUEUE_RELEASED, function (args, err) {
        expect(args).to.haveOwnProperty('reason').that.equal('timeout');
        expect(args)
          .to.haveOwnProperty('result')
          .that.haveOwnProperty('data')
          .that.deep.equal(serverResponseExample);
        expect(err).to.be.a('null');
        eventManager.removeListeners(SystemEvents.API_QUEUE_RELEASED);
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        }
      });
      for (let i = 1; i <= N; i++) {
        apiManager.enqueue(VID + i, requestData);
      }
    });
    it('Should fire the event when enqueued are released with 500 error passed', function (done) {
      const requestData = {
        eventType: 'bucketing',
        data: {
          experienceId: EXP,
          variationId: VAR
        }
      };
      eventManager.on(SystemEvents.API_QUEUE_RELEASED, function (args, err) {
        expect(args).to.haveOwnProperty('reason').that.equal('size');
        expect(err).to.haveOwnProperty('status').that.equal(500);
        eventManager.removeListeners(SystemEvents.API_QUEUE_RELEASED);
        apiManager.stopQueue();
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(500, {'Content-Type': 'application/json'});
          res.end();
        }
      });
      for (let i = 1; i <= batch_size; i++) {
        apiManager.enqueue(VID + i, requestData);
      }
    });
  });
});
