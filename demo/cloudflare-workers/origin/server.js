/*!
 * Simple origin server for the Cloudflare Workers demo.
 * Serves static HTML pages that the Worker proxies and modifies.
 *
 * Usage: node origin/server.js
 * Serves on http://localhost:8888
 */

const http = require('http');
const fs = require('fs');
const path = require('path');

const PORT = 8888;
const PAGES_DIR = path.join(__dirname, 'pages');

const server = http.createServer((req, res) => {
  // Map URL path to HTML file
  let filePath;
  const pathname = req.url.split('?')[0]; // strip query string

  switch (pathname) {
    case '/':
      filePath = path.join(PAGES_DIR, 'index.html');
      break;
    case '/events':
      filePath = path.join(PAGES_DIR, 'events.html');
      break;
    case '/statistics':
      filePath = path.join(PAGES_DIR, 'statistics.html');
      break;
    case '/pricing':
      filePath = path.join(PAGES_DIR, 'pricing.html');
      break;
    default:
      res.writeHead(404, {'Content-Type': 'text/html'});
      res.end('<h1>404 Not Found</h1>');
      return;
  }

  fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) {
      res.writeHead(500, {'Content-Type': 'text/plain'});
      res.end('Internal Server Error');
      return;
    }
    res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
    res.end(data);
  });
});

server.listen(PORT, () => {
  console.log(`Origin server running at http://localhost:${PORT}`);
  console.log('Pages: /, /events, /statistics, /pricing');
});
