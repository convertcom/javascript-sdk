import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {EventType, SystemEvents} from '@convertcom/js-sdk-enums';
import testConfig from './test-config.json';
import {Config, VisitorEvent} from '@convertcom/js-sdk-types';

const host = 'http://localhost';
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

function buildConfiguration(port: number) {
  return {
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
}

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
    const eventManager = new em(buildConfiguration(8090));
    apiManager = new am(buildConfiguration(8090), {eventManager});
    expect(apiManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('ApiManager');
  });

  const serverResponseExample = {
    data: 'ok'
  };

  describe('Test API Manager request', function () {
    it('Should successfully send test JSON payload', function (done) {
      const port = 8091;
      const server = http.createServer();
      const eventManager = new em(buildConfiguration(port));
      const apiManager = new am(buildConfiguration(port), {eventManager});
      server.listen(port);
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
              server.close();
              done();
            });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end(JSON.stringify(serverResponseExample));
      });
      server.on('listening', () => {
        apiManager.request(
          'post',
          {base: host + ':' + port, route: '/test'},
          testPayload
        );
      });
    });
  });

  describe('Test requests enqueuing', function () {
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
        const port = 8092;
        const eventManager = new em(buildConfiguration(port));
        const apiManager = new am(buildConfiguration(port), {eventManager});
        const server = http.createServer();
        server.listen(port);
        this.timeout(test_timeout);
        const requestData: VisitorEvent = {
          eventType: EventType.BUCKETING,
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
                  .and.has.length(N);
                data.visitors.every((visitor) =>
                  expect(visitor)
                    .to.haveOwnProperty('events')
                    .that.is.an('array')
                    .that.deep.includes(requestData)
                );
                server.close();
                done();
              });
          }
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        });
        server.on('listening', () => {
          for (let i = 1; i <= N; i++) {
            apiManager.enqueue(VID + i, requestData);
          }
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
        const port = 8093;
        const eventManager = new em(buildConfiguration(port));
        const apiManager = new am(buildConfiguration(port), {eventManager});
        const server = http.createServer();
        server.listen(port);
        this.timeout(release_timeout);
        const requestData: VisitorEvent = {
          eventType: EventType.BUCKETING,
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
                server.close();
                done();
              });
          }
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        });
        server.on('listening', () => {
          for (let i = 1; i <= batch_size; i++) {
            apiManager.enqueue(VID + i, requestData);
          }
        });
      }
    );
    it('Should fire the event when enqueued are released because of size and has server response in passed arguments', function (done) {
      const port = 8094;
      const eventManager = new em(buildConfiguration(port));
      const apiManager = new am(buildConfiguration(port), {eventManager});
      const server = http.createServer();
      server.listen(port);
      const requestData: VisitorEvent = {
        eventType: EventType.BUCKETING,
        data: {
          experienceId: EXP,
          variationId: VAR
        }
      };
      eventManager.on(SystemEvents.API_QUEUE_RELEASED, function (args, err) {
        expect(args).to.haveOwnProperty('reason').that.equal('size');
        expect(args)
          .to.haveOwnProperty('result')
          .that.haveOwnProperty('data')
          .that.deep.equal(serverResponseExample);
        expect(err).to.be.a('null');
        eventManager.removeListeners(SystemEvents.API_QUEUE_RELEASED);
        server.close();
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        }
      });
      server.on('listening', () => {
        for (let i = 1; i <= batch_size; i++) {
          apiManager.enqueue(VID + i, requestData);
        }
      });
    });
    it('Should fire the event when enqueued are released because of release timeout and has server response in passed arguments', function (done) {
      this.timeout(test_timeout);
      const port = 8095;
      const eventManager = new em(buildConfiguration(port));
      const apiManager = new am(buildConfiguration(port), {eventManager});
      const server = http.createServer();
      server.listen(port);
      const requestData: VisitorEvent = {
        eventType: EventType.BUCKETING,
        data: {
          experienceId: EXP,
          variationId: VAR
        }
      };

      eventManager.on(SystemEvents.API_QUEUE_RELEASED, function (args, err) {
        expect(args).to.haveOwnProperty('reason').that.equal('timeout');
        expect(args)
          .to.haveOwnProperty('result')
          .that.haveOwnProperty('data')
          .that.deep.equal(serverResponseExample);
        expect(err).to.be.a('null');
        eventManager.removeListeners(SystemEvents.API_QUEUE_RELEASED);
        server.close();
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        }
      });
      server.on('listening', () => {
        for (let i = 1; i <= N; i++) {
          apiManager.enqueue(VID + i, requestData);
        }
      });
    });
    it('Should fire the event when enqueued are released with 500 error passed', function (done) {
      const port = 8096;
      const server = http.createServer();
      const eventManager = new em(buildConfiguration(port));
      const apiManager = new am(buildConfiguration(port), {eventManager});
      server.listen(port);
      const requestData: VisitorEvent = {
        eventType: EventType.BUCKETING,
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
        server.close();
        done();
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith('/track')) {
          res.writeHead(500, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(serverResponseExample));
        }
      });
      server.on('listening', () => {
        for (let i = 1; i <= batch_size; i++) {
          apiManager.enqueue(VID + i, requestData);
        }
      });
    });
  });
});
