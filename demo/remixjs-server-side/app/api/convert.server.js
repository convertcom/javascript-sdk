import ConvertSDK, { LogLevel } from "@convertcom/js-sdk";
import { v4 as uuidv4 } from "uuid";
import DataStore from "./convert.datastore";

// Instantiate DataStore with 'cookie' driver
const dataStore = new DataStore({ driver: "cookie", expire: 360000 });

// Map to store contexts per user ID
const convertContextMap = new Map();

// Singleton ConvertSDK instance
let sdkInstance = null;

// Initialize the SDK instance
async function initializeConvertSDK(request) {
  // Parse cookies from the request using DataStore
  const cookieHeader = request.headers.get("Cookie") || "";
  dataStore.parseCookies(cookieHeader);

  const ConvertModule = ConvertSDK.default;
  if (!sdkInstance) {
    sdkInstance = new ConvertModule({
      environment: "staging",
      sdkKey: "100412329/100412881", // Replace with your actual SDK key
      dataStore, // provide persistent data store
      dataRefreshInterval: 180000,
      logger: {
        logLevel: LogLevel.DEBUG,
      },
    });
    try {
      await sdkInstance.onReady();
      console.log("Convert SDK initialized");
    } catch (error) {
      console.error("Convert SDK initialization error:", error);
      sdkInstance = null; // Reset instance on error
    }
  }

  return dataStore;
}

// Main function to get or create the Convert context
export async function getConvertContext(request) {
  const dataStore = await initializeConvertSDK(request);

  if (!sdkInstance) {
    throw new Error("Convert SDK failed to initialize");
  }

  const userIdCookieName = "_conv_uid";

  let userId = dataStore.get(userIdCookieName);

  // Variable to hold the Set-Cookie header
  let setCookieHeader = null;

  // If no userId, create one and set a cookie
  if (!userId) {
    userId = uuidv4();
    dataStore.set(userIdCookieName, userId);
    // Create the Set-Cookie header using DataStore
    setCookieHeader = dataStore.createSetCookieHeader(
      userIdCookieName,
      userId,
      {
        maxAge: dataStore.expire, // expire is in seconds
        httpOnly: true,
        secure: process.env.NODE_ENV === "production",
        sameSite: "lax",
      }
    );
  }

  // Retrieve or create the context for this user
  let context = convertContextMap.get(userId);
  if (!context) {
    context = sdkInstance.createContext(userId, { mobile: true });
    context.setDefaultSegments({ country: "US" });
    convertContextMap.set(userId, context);
    console.log(`Created new context for userId (cookie _conv_uid): ${userId}`);
  } else {
    console.log(
      `Retrieved existing context for userId (cookie _conv_uid): ${userId}`
    );
  }

  return { context, setCookieHeader };
}
