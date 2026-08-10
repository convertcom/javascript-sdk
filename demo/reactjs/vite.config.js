import {defineConfig} from 'vite';
import {fileURLToPath} from 'node:url';
import react from '@vitejs/plugin-react';
import svgr from 'vite-plugin-svgr';

const srcDir = (segment) =>
  fileURLToPath(new URL(`./src/${segment}`, import.meta.url));

export default defineConfig({
  plugins: [
    // The sources are .js files containing JSX, as Create React App allowed.
    // Vite's esbuild pass only treats .jsx/.tsx as JSX, so babel is given the
    // JSX transform explicitly via preset-react. Doing it in babel (not esbuild)
    // also means twin.macro's babel-plugin-macros sees the raw JSX, which its
    // `css={...}` prop support depends on.
    react({
      include: /\.(js|jsx)$/,
      babel: {
        plugins: ['babel-plugin-macros'],
        presets: [['@babel/preset-react', {runtime: 'automatic'}]]
      }
    }),
    // Reproduce CRA's SVG handling: `import {ReactComponent as Icon} from './x.svg'`
    // keeps working, including for .svg files inside node_modules.
    // `?url` is left to Vite's own asset handling, so an SVG can still be
    // imported as a plain URL where that is what the component wants.
    svgr({
      include: '**/*.svg',
      exclude: '**/*.svg?url',
      svgrOptions: {exportType: 'named', namedExport: 'ReactComponent'}
    })
  ],
  resolve: {
    // jsconfig.json sets baseUrl to src, so imports like "components/misc/Layouts.js"
    // resolve from there. Vite has no baseUrl, so each src folder gets an alias.
    alias: {
      components: srcDir('components'),
      helpers: srcDir('helpers'),
      images: srcDir('images'),
      landing: srcDir('landing'),
      styles: srcDir('styles')
    }
  },
  // esbuild's dependency scanner reads the entry graph directly and would
  // otherwise choke on JSX inside .js, skipping pre-bundling on every dev start.
  optimizeDeps: {
    esbuildOptions: {loader: {'.js': 'jsx'}}
  },
  // README documents http://localhost:3002 for this demo.
  server: {port: Number(process.env.PORT) || 3002},
  preview: {port: Number(process.env.PORT) || 3002},
  build: {outDir: 'build'}
});
