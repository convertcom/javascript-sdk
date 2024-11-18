import { createContext, useContext, useEffect, useState } from "react";
import ConvertSDK from "@convertcom/js-sdk";
import { v4 as uuidv4 } from "uuid";

const ConvertContext = createContext(null);

export function useConvertContext() {
  return useContext(ConvertContext);
}

export function ConvertProvider({ children }) {
  const [convertContext, setConvertContext] = useState(null);

  useEffect(() => {
    async function initializeConvert() {
      const convertSDK = new ConvertSDK({
        sdkKey: "100412329/100412881",
        dataRefreshInterval: 180000,
        environment: "staging",
        network: {
          tracking: true,
        },
      });

      await convertSDK.onReady();
      const convertUserId = uuidv4();
      const context = convertSDK.createContext(convertUserId);
      setConvertContext(context);
    }

    initializeConvert();
  }, []);

  return (
    <ConvertContext.Provider value={convertContext}>
      {children}
    </ConvertContext.Provider>
  );
}
