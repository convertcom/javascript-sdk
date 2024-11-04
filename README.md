# Convert Insights, Inc JavaScript SDK

## Installation

1. Install dependencies: `yarn`
2. Copy `env.example` into `.env` at the following packages:
   1. [react demo](packages/demo-react/README.md)
   2. [nodejs demo](packages/demo-nodejs/README.md)
   3. [sdk](packages/js-sdk/README.md)

## JavaScript SDK Documentation

1. Build packages: `yarn build`
2. Start local server: `yarn sdk:docs`
3. Prevview docs: http://localhost:3001

## Demo Instructions

> **Note:** Look for the marker `[ConvertSDK]` at `*.js` files

### React

1. Start demo server: `yarn demo:reactjs:start`
2. Build packages: `yarn build`
3. Create enviroment file: `cp demo/reactjs/.env.example demo/reactjs/.env`
4. Test demo app: http://localhost:3002

### NodeJS

1. Start demo server: `yarn demo:nodejs:start`
2. Build packages: `yarn build`
3. Create enviroment file: `cp demo/nodejs/.env.example demo/nodejs/.env`
4. Test demo app: http://localhost:3003

### NestJS

1. Start demo server: `yarn demo:nestjs:start`
2. Build packages: `yarn build`
3. Create enviroment file: `cp demo/nestjs/.env.example demo/nestjs/.env`
4. Test demo app: http://localhost:3004

### NextJs

1. Start demo server: `yarn demo:nextjs:start`
2. Build packages: `yarn build`
3. Create enviroment file: `cp demo/nextjs/.env.example demo/nextjs/.env`
4. Test demo app: http://localhost:3005
