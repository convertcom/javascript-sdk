# Welcome to Remix!

- [Remix Docs](https://remix.run/docs)

## Development

From your terminal:

```sh
npm run dev
```

This starts your app in development mode, rebuilding assets on file changes.

## Deployment

First, build your app for production:

```sh
npm run build
```

Then run the app in production mode:

```sh
npm start
```

Now you'll need to pick a host to deploy it to.

## Testing preview links

Force a specific variation to render — regardless of bucketing, audience/segment/location rules, environment, experience/variation status, or traffic allocation, and even for **draft/paused** experiences (the SDK fetches them via `?exp=`). No tracking events or visitor-state writes happen on a preview context (zero-trace).

**URL param format:** `?convert_preview={experienceId}.{variationId}` (dot-separated numeric ids — mirrors the web tracking script's force-param).

**Example:**

```
http://localhost:3007/?convert_preview=100123.200456
```

Replace `100123.200456` with a real experience id / variation id pair from your Convert project.

> The preview link is normally generated from the Convert UI's per-variation "Copy preview link". You can also construct it manually as `{experienceId}.{variationId}`.

### DIY

If you're familiar with deploying node applications, the built-in Remix app server is production-ready.

Make sure to deploy the output of `remix build`

- `build/`
- `public/build/`
