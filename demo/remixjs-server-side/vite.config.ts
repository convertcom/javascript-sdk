import {reactRouter} from '@react-router/dev/vite';
import {defineConfig} from 'vite';
import tsconfigPaths from 'vite-tsconfig-paths';

export default defineConfig({
  plugins: [reactRouter(), tsconfigPaths()],
  // README documents http://localhost:3007 for this demo; PORT (.env) wins.
  server: {port: Number(process.env.PORT) || 3007}
});
