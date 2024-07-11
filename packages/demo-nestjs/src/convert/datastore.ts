import fs from 'fs';

export default class DataStore {
  driver = 'cookie'; // options: fs, cookie
  response = null;
  expire = 360000; // expires in 1 hour
  store = '/tmp/demo';
  data = {};
  constructor({ driver, expire }: { driver?: string; expire?: number } = {}) {
    if (driver) this.driver = driver;
    if (expire) this.expire = expire;
    if (this.driver === 'fs' && !fs.existsSync(this.store))
      fs.writeFileSync(this.store, '{}');
  }
  get(key) {
    if (this.driver === 'fs')
      this.data = JSON.parse(
        fs.readFileSync(this.store, 'utf-8').toString() || '{}',
      );
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key, value) {
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
    if (this.driver === 'fs') {
      fs.writeFileSync(this.store, JSON.stringify(this.data));
    } else {
      try {
        this.response.cookie(key.toString(), value, {
          maxAge: this.expire,
        });
      } catch (err) {
        // console.error(err);
      }
    }
  }
}
