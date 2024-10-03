export default class DataStore {
  #data = {};
  get(key) {
    this.#data = JSON.parse(localStorage.getItem("data-store") || "{}");
    if (!key) return this.#data;
    return this.#data[key.toString()];
  }
  set(key, value) {
    if (!key) throw new Error("Invalid DataStore key!");
    this.#data[key.toString()] = value;
    localStorage.setItem("data-store", JSON.stringify(this.#data));
  }
}
