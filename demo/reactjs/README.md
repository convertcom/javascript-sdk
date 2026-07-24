# Convert Insights, Inc JavaScript SDK – ReactJS Demo

This project was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).

> **Note:** Look for the marker `[ConvertSDK]` at `*.js` files

## Available Script

In the project directory, you can run:

### `yarn start`

Runs the app in the development mode.<br />
Open [http://localhost:3002](http://localhost:3002) to view it in the browser.

The page will reload if you make edits.<br />
You will also see any lint errors in the console.

## Testing preview links

Force a specific variation to render — regardless of bucketing, audience/segment/location rules, environment, experience/variation status, or traffic allocation, and even for **draft/paused** experiences (the SDK fetches them via `?exp=`). No tracking events or visitor-state writes happen on a preview context (zero-trace).

**URL param format:** `?convert_preview={experienceId}.{variationId}` (dot-separated numeric ids — mirrors the web tracking script's force-param).

**Example:**

```
http://localhost:3002/events?convert_preview=100123.200456
```

Replace `100123.200456` with a real experience id / variation id pair from your Convert project.

> The preview link is normally generated from the Convert UI's per-variation "Copy preview link". You can also construct it manually as `{experienceId}.{variationId}`.

### `yarn build`

Builds the app for production to the `build` folder.<br />
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.<br />
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

## Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

To learn React, check out the [React documentation](https://reactjs.org/).

### Code Splitting

This section has moved here: https://facebook.github.io/create-react-app/docs/code-splitting

### Analyzing the Bundle Size

This section has moved here: https://facebook.github.io/create-react-app/docs/analyzing-the-bundle-size

### Making a Progressive Web App

This section has moved here: https://facebook.github.io/create-react-app/docs/making-a-progressive-web-app

### Advanced Configuration

This section has moved here: https://facebook.github.io/create-react-app/docs/advanced-configuration

### Deployment

This section has moved here: https://facebook.github.io/create-react-app/docs/deployment

### `yarn build` fails to minify

This section has moved here: https://facebook.github.io/create-react-app/docs/troubleshooting#npm-run-build-fails-to-minify
