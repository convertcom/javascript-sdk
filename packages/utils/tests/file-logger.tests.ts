import 'mocha';
import chai from 'chai';
import chaiString from 'chai-string';
chai.use(chaiString);
const {assert} = chai;
import fs from 'fs';
import util from 'util';

import {FileLogger} from '../src/file-logger';
const readFile = util.promisify(fs.readFile);
const testFile = '/tmp/test.log';
const DEBUG_MODE = process.env.DEBUG;

function captureConsole() {
  const defaultConsoleLog = console.log;
  const defaultConsoleDebug = console.debug;
  const defaultConsoleInfo = console.info;
  const defaultConsoleWarn = console.warn;
  const defaultConsoleError = console.error;
  return {
    unhook: () => {
      console.log = defaultConsoleLog;
      console.debug = defaultConsoleDebug;
      console.info = defaultConsoleInfo;
      console.warn = defaultConsoleWarn;
      console.error = defaultConsoleError;
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

describe('File Logger utils tests', function () {
  let mockConsole, hookError, logger;
  beforeEach(function () {
    mockConsole = captureConsole();
    hookError = captureError();
  });
  afterEach(function () {
    mockConsole.unhook();
    logger = null;
    if (fs.existsSync(testFile)) {
      fs.rmSync(testFile);
    }
  });
  it('Should return an error when using FileLogger with an invalid file', async function () {
    logger = new FileLogger('', fs);
    const log = async function () {
      await logger.log('testing invalid file');
      assert.equal(
        hookError.captured(),
        'Error: ENOENT: no such file or directory, open\n'
      );
    };
    await log();
  });
  it('Should return an error when using FileLogger with a read-only file', async function () {
    fs.writeFileSync(testFile, '');
    fs.chmodSync(testFile, 0o444);
    logger = new FileLogger(testFile, fs);
    const log = async function () {
      await logger.log('testing read-only log file');
      assert.equal(
        hookError.captured(),
        `Error: EACCES: permission denied, open '${testFile}'\n`
      );
    };
    await log();
  });
  it('Should log to file when using FileLogger', async function () {
    logger = new FileLogger(testFile, fs);
    const log = async function () {
      const output = 'testing log file';
      await logger.log(output);
      const readInput = readFile(testFile, {encoding: 'utf-8'});
      const logOutput = await Promise.resolve(readInput);
      assert.endsWith(logOutput, `[LOG] "${output}"\n`);
    };
    await log();
  });
});
