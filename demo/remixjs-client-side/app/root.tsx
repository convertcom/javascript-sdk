import {Links, Meta, Outlet, Scripts, ScrollRestoration} from 'react-router';
import {ConvertProvider} from './providers/Convert';

export default function App() {
  return (
    <ConvertProvider>
      <html lang="en">
        <head>
          <meta charSet="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <Meta />
          <Links />
        </head>
        <body>
          <Outlet />
          <ScrollRestoration />
          <Scripts />
        </body>
      </html>
    </ConvertProvider>
  );
}
