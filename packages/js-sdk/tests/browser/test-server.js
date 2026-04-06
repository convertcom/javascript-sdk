const http = require('http');
const fs = require('fs');
const path = require('path');

const PORT = process.env.PORT || 3939;
const ROOT = path.resolve(__dirname, '..', '..');

const MIME_TYPES = {
  '.html': 'text/html',
  '.js': 'application/javascript',
  '.mjs': 'application/javascript',
  '.json': 'application/json',
  '.css': 'text/css'
};

const ROUTES = {
  '/': path.join(ROOT, 'tests/browser/test-page.html'),
  '/umd.html': path.join(ROOT, 'tests/browser/test-page.html'),
  '/lib/index.js': path.join(ROOT, 'lib/index.js'),
  '/lib/index.umd.min.js': path.join(ROOT, 'lib/index.umd.min.js'),
  '/test-config.json': path.join(ROOT, 'tests/test-config.json'),
  '/static-config.json': path.join(
    ROOT,
    'tests/integration/static-config.json'
  )
};

const server = http.createServer((req, res) => {
  const url = req.url.split('?')[0];
  const filePath = ROUTES[url];

  if (!filePath) {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end('Not Found');
    return;
  }

  try {
    const content = fs.readFileSync(filePath);
    const ext = path.extname(filePath);
    const contentType = MIME_TYPES[ext] || 'application/octet-stream';
    res.writeHead(200, {'Content-Type': contentType});
    res.end(content);
  } catch (err) {
    res.writeHead(500, {'Content-Type': 'text/plain'});
    res.end('Internal Server Error: ' + err.message);
  }
});

server.listen(PORT, () => {
  console.log(`Test server listening on http://localhost:${PORT}`);
});
