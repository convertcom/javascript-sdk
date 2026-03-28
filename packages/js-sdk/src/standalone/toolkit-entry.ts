import {ensureConvertWindow, initializeVisitorRuntime} from './runtime';
import {runWithToolkit} from './toolkit';

export const runToolkitEntry = async () => {
  const runtime = await initializeVisitorRuntime();
  const convert = ensureConvertWindow();
  if (!runtime || !convert) return null;

  runWithToolkit(convert, runtime);
  return convert.T;
};

void runToolkitEntry();
