# Convert Insights, Inc JavaScript SDK – NodeJS Demo

This project was bootstrapped with [Express application generator](https://expressjs.com/en/starter/generator.html).

> **Note:** Look for the marker `[ConvertSDK]` at `*.js` files

## Available Script

In the project directory, you can run:

### `yarn start`

Runs the app in the development mode.<br />
Open [http://localhost:3003](http://localhost:3003) to view it in the browser.

You will also see any lint errors in the console.

### `yarn start:debug`

Same as above with detailed logs.

## Testing preview links

Force a specific variation to render — regardless of bucketing, audience/segment/location rules, environment, experience/variation status, or traffic allocation, and even for **draft/paused** experiences (the SDK fetches them via `?exp=`). No tracking events or visitor-state writes happen on a preview context (zero-trace).

**URL param format:** `?convert_preview={experienceId}.{variationId}` (dot-separated numeric ids — mirrors the web tracking script's force-param).

**Example:**

```
http://localhost:3003/events?convert_preview=100123.200456
```

Replace `100123.200456` with a real experience id / variation id pair from your Convert project.

> The preview link is normally generated from the Convert UI's per-variation "Copy preview link". You can also construct it manually as `{experienceId}.{variationId}`.
