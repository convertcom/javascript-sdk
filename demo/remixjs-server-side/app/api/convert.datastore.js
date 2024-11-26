export default class ConvertDataStore {
    driver = "cookie"; // options: 'fs', 'cookie'
    expire = 3600; // expires in seconds
    data = {};
  
    constructor(options = {}) {
      if (options.driver) this.driver = options.driver;
      if (options.expire) this.expire = options.expire / 1000; // Convert milliseconds to seconds
    }
  
    get(key) {
      if (!key) return this.data;
      return this.data[key.toString()];
    }
  
    set(key, value) {
      if (!key) throw new Error("Invalid DataStore key!");
      this.data[key.toString()] = value;
      // In Remix, we'll handle setting the cookie header in the calling code.
    }
  
    // Method to parse cookies from the request
    parseCookies(cookieHeader) {
      const cookies = {};
      cookieHeader.split(";").forEach((cookie) => {
        const [name, ...rest] = cookie.trim().split("=");
        if (!name) return;
        const value = rest.join("=");
        cookies[name] = decodeURIComponent(value);
      });
      this.data = cookies;
      return cookies;
    }
  
    // Method to create the Set-Cookie header
    createSetCookieHeader(name, value, options = {}) {
      let cookieString = `${name}=${encodeURIComponent(value)}`;
  
      if (options.maxAge) {
        cookieString += `; Max-Age=${options.maxAge}`;
      }
      if (options.domain) {
        cookieString += `; Domain=${options.domain}`;
      }
      if (options.path) {
        cookieString += `; Path=${options.path}`;
      } else {
        cookieString += `; Path=/`;
      }
      if (options.httpOnly) {
        cookieString += `; HttpOnly`;
      }
      if (options.secure) {
        cookieString += `; Secure`;
      }
      if (options.sameSite) {
        cookieString += `; SameSite=${options.sameSite}`;
      }
      return cookieString;
    }
  }
  