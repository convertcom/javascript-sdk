/*!
 * Simple origin server for the Cloudflare Workers demo.
 * Generates HTML pages from a shared template that the Worker proxies and modifies.
 *
 * Usage: node origin/server.js
 * Serves on http://localhost:8888
 */

const http = require('http');

const PORT = 8888;

// Page definitions — only the unique content per route
const PAGES = {
  '/': {
    title: 'Convert Edge Demo',
    body: `
    <div class="card">
      <h1>Cloudflare Workers Demo</h1>
      <p>This page is served by a simple origin server and proxied through a Cloudflare Worker.</p>
      <p>The Worker runs Convert A/B tests at the edge and modifies HTML before delivery — zero flicker.</p>
    </div>
    <div class="card" style="background:#e8f4fd;border-left:4px solid #2196F3">
      <strong>Tip:</strong> Visit the <a href="/events">Events</a>, <a href="/statistics">Statistics</a>,
      or <a href="/pricing">Pricing</a> pages to see experiments in action.
      The home page has no experiments configured.
    </div>`
  },
  '/events': {
    title: 'Events - Convert Edge Demo',
    heading: 'Events',
    description: 'Experience: <code>test-experience-ab-fullstack-1</code> | Location: <code>events</code>',
    featureLabel: 'Feature Status'
  },
  '/statistics': {
    title: 'Statistics - Convert Edge Demo',
    heading: 'Statistics',
    description: 'Runs all matching experiences | Feature: <code>feature-4</code> | Location: <code>statistics</code>',
    featureLabel: 'Feature Status (feature-4)'
  },
  '/pricing': {
    title: 'Pricing - Convert Edge Demo',
    heading: 'Pricing',
    description:
      'Runs all matching experiences | Feature: <code>feature-5</code> | Location: <code>pricing</code></p>' +
      '<p>Experiences on this route: <code>test-experience-ab-fullstack-1</code> and <code>test-experience-ab-fullstack-4</code>',
    featureLabel: 'Feature Status (feature-5)'
  }
};

// Shared layout template
function renderPage(page) {
  const nav = `<nav>
    <a href="/">Home</a>
    <a href="/events">Events</a>
    <a href="/statistics">Statistics</a>
    <a href="/pricing">Pricing</a>
  </nav>`;

  // Home page uses custom body; experiment pages use a standard layout
  const content = page.body || `
    <div class="card">
      <h1>${page.heading}</h1>
      <p>${page.description}</p>
    </div>
    <div class="card">
      <h3>Experiment Results</h3>
      <div id="experiment-results">
        <span class="placeholder">No experiment bucketing yet — this content is replaced by the Worker.</span>
      </div>
    </div>
    <div class="card">
      <h3>${page.featureLabel}</h3>
      <div id="feature-status">
        <span class="placeholder">No feature flag evaluated.</span>
      </div>
    </div>
    <div class="card">
      <h3>Variation Caption</h3>
      <div id="variation-caption">
        <span class="placeholder">Original caption (not modified)</span>
      </div>
    </div>`;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${page.title}</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; background: #f5f5f5; }
    nav { background: #333; padding: 10px 20px; border-radius: 8px; margin-bottom: 20px; }
    nav a { color: #fff; text-decoration: none; margin-right: 20px; }
    nav a:hover { text-decoration: underline; }
    .card { background: #fff; border-radius: 8px; padding: 20px; margin-bottom: 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
    h1 { color: #333; }
    .placeholder { color: #999; font-style: italic; }
    .enabled { color: #4CAF50; font-weight: bold; }
  </style>
</head>
<body>
  ${nav}
  ${content}
</body>
</html>`;
}

const server = http.createServer((req, res) => {
  const pathname = req.url.split('?')[0];
  const page = PAGES[pathname];

  if (!page) {
    res.writeHead(404, {'Content-Type': 'text/html'});
    res.end('<h1>404 Not Found</h1>');
    return;
  }

  res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
  res.end(renderPage(page));
});

server.listen(PORT, () => {
  console.log(`Origin server running at http://localhost:${PORT}`);
  console.log('Pages: /, /events, /statistics, /pricing');
});
