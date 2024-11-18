import { PassThrough } from "node:stream";
import type { AppLoadContext, EntryContext } from "@remix-run/node";
import { createReadableStreamFromReadable } from "@remix-run/node";
import { RemixServer } from "@remix-run/react";
import { isbot } from "isbot";
import { renderToPipeableStream } from "react-dom/server";

const ABORT_DELAY = 5_000;

export default function handleRequest(
  request: Request,
  responseStatusCode: number,
  responseHeaders: Headers,
  remixContext: EntryContext,
  loadContext: AppLoadContext,
) {
  const userAgent = request.headers.get("user-agent") || "";
  const isBotRequest = isbot(userAgent);

  return new Promise((resolve, reject) => {
    let didError = false;

    const { pipe, abort } = renderToPipeableStream(
      <RemixServer context={remixContext} url={request.url} />,
      {
        // Conditional callbacks based on request type
        ...(isBotRequest
          ? {
            onAllReady() {
              const body = new PassThrough();
              const stream = createReadableStreamFromReadable(body);
              responseHeaders.set("Content-Type", "text/html");

              resolve(
                new Response(stream, {
                  headers: responseHeaders,
                  status: didError ? 500 : responseStatusCode,
                }),
              );
              pipe(body);
            },
          }
          : {
            onShellReady() {
              const body = new PassThrough();
              const stream = createReadableStreamFromReadable(body);
              responseHeaders.set("Content-Type", "text/html");

              resolve(
                new Response(stream, {
                  headers: responseHeaders,
                  status: didError ? 500 : responseStatusCode,
                }),
              );
              pipe(body);
            },
          }),

        onShellError(err: unknown) {
          reject(err);
        },

        onError(err: unknown) {
          didError = true;
          console.error(err);
        },
      },
    );

    setTimeout(abort, ABORT_DELAY);
  });
}
