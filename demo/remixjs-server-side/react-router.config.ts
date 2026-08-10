import type {Config} from '@react-router/dev/config';

export default {
  // This demo runs the SDK in the loader/action, so it must render on the server.
  ssr: true
} satisfies Config;
