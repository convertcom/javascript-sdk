import 'mocha';
import chai from 'chai';
import chaiString from 'chai-string';
chai.use(chaiString);
const {expect, assert} = chai;

import {LogManager as lm} from '../src/log-manager';
import {LogLevel as lv} from '@convertcom/js-sdk-enums';

const DEBUG_MODE = process.env.DEBUG;

function captureConsole() {
  const defaultConsoleLog = console.log;
  const defaultConsoleDebug = console.debug;
  const defaultConsoleInfo = console.info;
  const defaultConsoleWarn = console.warn;
  const defaultConsoleError = console.error;
  const defaultConsoleTrace = console.trace;
  return {
    unhook: () => {
      console.log = defaultConsoleLog;
      console.debug = defaultConsoleDebug;
      console.info = defaultConsoleInfo;
      console.warn = defaultConsoleWarn;
      console.error = defaultConsoleError;
      console.trace = defaultConsoleTrace;
    }
  };
}

function captureLog() {
  let buffer = '';
  console.log =
    console.debug =
    console.trace =
    console.info =
      (...args) => {
        buffer += args.join(' ') + '\n';
        if (DEBUG_MODE) {
          process.stdout.write(args.join(' ') + '\n');
        }
      };
  return {
    captured: () => {
      return buffer;
    }
  };
}

function captureError() {
  let buffer = '';
  console.warn = console.error = (...args) => {
    buffer += args.join(' ') + '\n';
    if (DEBUG_MODE) {
      process.stderr.write(args.join(' ') + '\n');
    }
  };
  return {
    captured: () => {
      return buffer;
    }
  };
}

describe('LogManager tests', function () {
  let mockConsole, hookLog, hookError, logger;
  beforeEach(function () {
    mockConsole = captureConsole();
    hookLog = captureLog();
    hookError = captureError();
    logger = new lm();
  });
  afterEach(function () {
    mockConsole.unhook();
    logger = null;
  });
  it('Should expose LogManager', function () {
    assert.isDefined(lm);
  });
  it('Imported entity should be a constructor of LogManager instance', function () {
    expect(lm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('LogManager');
  });
  it('Should log to Console by default', function () {
    const output = 'testing trace message';
    logger.log(lv.TRACE, output);
    assert.equal(hookLog.captured(), `${output}\n`);
  });
  it('Should support log method with multiple arguments', function () {
    const output = 'testing log method';
    const argument = 'with multiple arguments';
    logger.log(lv.TRACE, output, argument);
    assert.equal(hookLog.captured(), `${output} ${argument}\n`);
  });
  it('Should support trace method with multiple arguments', function () {
    const output = 'testing trace method';
    const argument = 'with multiple arguments';
    logger.trace(output, argument);
    assert.equal(hookLog.captured(), `${output} ${argument}\n`);
  });
  it('Should support debug method with multiple arguments', function () {
    const output = 'testing debug method';
    const argument = 'with multiple arguments';
    logger.debug(output, argument);
    assert.equal(hookLog.captured(), `${output} ${argument}\n`);
  });
  it('Should support info method with multiple arguments', function () {
    const output = 'testing info method';
    const argument = 'with multiple arguments';
    logger.info(output, argument);
    assert.equal(hookLog.captured(), `${output} ${argument}\n`);
  });
  it('Should support warn method with multiple arguments', function () {
    const output = 'testing warn method';
    const argument = 'with multiple arguments';
    logger.warn(output, argument);
    assert.equal(hookError.captured(), `${output} ${argument}\n`);
  });
  it('Should support error method with multiple arguments', function () {
    const output = 'testing error method';
    const argument = 'with multiple arguments';
    logger.error(output, argument);
    assert.equal(hookError.captured(), `${output} ${argument}\n`);
  });
  it('Should not log anything when using silent LogLevel', function () {
    const output = 'testing silent log level';
    logger.log(lv.SILENT, output);
    assert.equal(hookLog.captured(), '');
  });
  it('Should return an error when using an invalid LogLevel', function () {
    // expect(function () {
    //   logger.log(6, 'testing invalid log level');
    // }).to.throw('Invalid Log Level');
    logger.log(6, 'testing invalid log level');
    assert.equal(hookError.captured(), 'Invalid Log Level\n');
  });
  it('Should return an error when adding new client with an invalid SDK', function () {
    // expect(function () {
    //   logger.addClient(null);
    // }).to.throw('Invalid Client SDK');
    logger.addClient(null);
    assert.equal(hookError.captured(), 'Invalid Client SDK\n');
  });
  it('Should return an error when adding new client with an invalid LogLevel', function () {
    // expect(function () {
    //   logger.addClient(console, 6);
    // }).to.throw('Invalid Log Level');
    logger.addClient(console, 6);
    assert.equal(hookError.captured(), 'Invalid Log Level\n');
  });
  it('Should log to Console and to third-party when adding a new client', function () {
    logger.addClient(console);
    const output = 'testing third-party logger';
    logger.trace(output);
    assert.equal(hookLog.captured(), `${output}\n${output}\n`);
  });
  it('Should map custom log method when adding a new client', function () {
    logger.addClient(
      {
        send: console.log
      },
      lv.TRACE,
      {
        trace: 'send'
      }
    );
    const output = 'testing third-party method mapping';
    logger.trace(output);
    assert.equal(hookLog.captured(), `${output}\n${output}\n`);
  });
  it('Should fallback to Console using a missing method by a new client', function () {
    logger.addClient(
      {
        log: console.log
      },
      lv.INFO
    );
    const output = 'testing third-party missing info method';
    logger.info(output);
    assert.equal(
      hookLog.captured(),
      `${output}\nInfo: Unable to find method "info()" in client sdk: Object\n${output}\n`
    );
  });
  it('Should log only matching levels when using a new client', function () {
    logger.addClient(console, lv.ERROR);
    const output = 'testing third-party matching log level';
    logger.warn(output);
    assert.equal(hookError.captured(), `${output}\n`);
  });
});
