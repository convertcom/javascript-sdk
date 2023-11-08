var fs = require("fs");
class DataStore {
  driver = "cookie"; // options: fs, cookie
  response = null;
  expire = 360000; // expires in 1 hour
  store = "/tmp/demo";
  data = {};
  constructor(options = {}) {
    if (options.driver) this.driver = options.driver;
    if (options.expire) this.expire = options.expire;
    if (this.driver === "fs" && !fs.existsSync(this.store))
      fs.writeFileSync(this.store, "{}");
  }
  get(key) {
    if (this.driver === "fs")
      this.data = JSON.parse(
        fs.readFileSync(this.store, "utf-8").toString() || "{}"
      );
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key, value) {
    if (!key) throw new Error("Invalid DataStore key!");
    this.data[key.toString()] = value;
    if (this.driver === "fs")
      fs.writeFileSync(this.store, JSON.stringify(this.data));
    else
      this.response.cookie(key.toString(), value, {
        maxAge: this.expire,
      });
  }
}

module.exports = DataStore;
