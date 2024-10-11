import React, { useState, useEffect, useCallback } from "react";
import GlobalStyles from "styles/GlobalStyles";

import MainPage from "landing/MainPage.js";
import EventsPage from "landing/EventsPage.js";
import StatisticsPage from "landing/StatisticsPage.js";
import PricingPage from "landing/PricingPage.js";

import { BrowserRouter as Router, Routes, Route } from "react-router-dom";

import DataStore from "./DataStore";
import { UserContext } from "./Context";
import ConvertSDK from "@convertcom/js-sdk"; // [ConvertSDK]

const dataStore = new DataStore();

const sdkConfig = {
  sdkKey: "10035569/10034190",
  dataStore, // optional
}; // [ConvertSDK]

const sdkInstance = new ConvertSDK(sdkConfig);

export default function App(props) {
  const [, updateState] = useState();
  const forceUpdate = useCallback(() => updateState({}), []);

  const storedUser =
    dataStore.get("user-id") || `${new Date().getTime()}-${performance.now()}`;
  const [userId, setUserId] = useState(storedUser);

  const [sdkContext, setSdkContext] = useState();

  useEffect(() => {
    async function initSdk() {
      try {
        await sdkInstance.onReady();
        console.log("SDK Ready");
        const context = sdkInstance.createContext(userId, {
          mobile: true,
        });
        context.setDefaultSegments({ country: "US" });
        setSdkContext(context);
      } catch (error) {
        console.error("SDK Error:", error);
      }
    } // [ConvertSDK]
    initSdk();
  }, []);

  return (
    <UserContext.Provider
      value={{
        sdkContext,
        userId,
        setUserId: (id) => {
          dataStore.set("user-id", id);
          setUserId(id);
          forceUpdate();
        },
        forceUpdate,
      }}
    >
      <GlobalStyles />
      <Router>
        <Routes>
          <Route path="/" element={<MainPage {...props} />} />
          <Route path="/events" element={<EventsPage />} />
          <Route path="/statistics" element={<StatisticsPage />} />
          <Route path="/pricing" element={<PricingPage />} />
        </Routes>
      </Router>
    </UserContext.Provider>
  );
}
