// lib/DataStore.js

import { getCookie, setCookie } from 'cookies-next';
export default class DataStore {
  constructor(options = {}) {
    this.driver = options.driver || 'cookie'; // 'fs' or 'cookie'
    this.expire = options.expire || 360000; // 1 hour
    this.store = options.store || '/tmp/demo';
    this.req = options.req || null;
    this.res = options.res || null;
    this.data = {};

    if (this.driver === 'fs') {
      if (typeof window === 'undefined') {
        // Server-side: conditionally require 'fs'
        /* eslint-disable @typescript-eslint/no-require-imports */
        this.fs = require('fs');
        /* eslint-enable @typescript-eslint/no-require-imports */
        if (!this.fs.existsSync(this.store)) {
          this.fs.writeFileSync(this.store, '{}');
        }
      } else {
        throw new Error("'fs' driver cannot be used in the browser.");
      }
    }
  }

  get(key) {
    if (this.driver === 'fs') {
      if (!this.fs) {
        throw new Error("'fs' module is not available on the client side.");
      }
      this.data = JSON.parse(this.fs.readFileSync(this.store, 'utf-8') || '{}');
    } else if (this.driver === 'cookie' && (this.req || typeof window !== 'undefined')) {
        console.log("window" , window);
      if (typeof window !== 'undefined') {
        // Client-side
       
        this.data = getCookie('data-store') || {};
      } 
    } 
    if (!key) return this.data;
    return this.data[key.toString()];
  }

  set(key, value) {
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
    if (this.driver === 'fs') {
      if (!this.fs) {
        throw new Error("'fs' module is not available on the client side.");
      }
      this.fs.writeFileSync(this.store, JSON.stringify(this.data));
    } else if (this.driver === 'cookie' && (this.res || typeof window !== 'undefined')) {
      if (typeof window !== 'undefined') {
        // Client-side
        setCookie('data-store', this.data, {
          maxAge: this.expire / 1000, // maxAge in seconds
          httpOnly: false,
        });
      } 
    } 
  }
}
