var fs = require("fs");

class DataStore {
  store = "/tmp/demo";
  data = {};
  constructor() {
    if (!fs.existsSync(this.store)) fs.writeFileSync(this.store, "{}");
  }
  get(key) {
    this.data = JSON.parse(
      fs.readFileSync(this.store, "utf-8").toString() || "{}"
    );
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key, value) {
    if (!key) throw new Error("Invalid DataStore key!");
    this.data[key.toString()] = value;
    fs.writeFileSync(this.store, JSON.stringify(this.data));
  }
}

module.exports = DataStore;
