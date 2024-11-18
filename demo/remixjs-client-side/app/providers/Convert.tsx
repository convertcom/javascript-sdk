import {
  createContext,
  useContext,
  useEffect,
  useState,
  ReactNode,
} from "react";
import ConvertSDK, { LogLevel } from "@convertcom/js-sdk";
import { v4 as uuidv4 } from "uuid";
import type { ContextInterface } from "@convertcom/js-sdk";
interface ConvertProviderProps {
  children: ReactNode;
}

// Define the interfaces for the SDK instance and constructor
interface ConvertSDKInstance {
  onReady(): Promise<void>;
  createContext(userId: string): ContextInterface | null;
}

interface ConvertSDKConstructor {
  new(options: {
    sdkKey: string;
    dataRefreshInterval: number;
    environment: string;
    logger: { logLevel: LogLevel };
  }): ConvertSDKInstance;
}



const ConvertContext = createContext<ContextInterface | null>(null);

export function useConvertContext() {
  return useContext(ConvertContext);
}

export function ConvertProvider({ children }: Readonly<ConvertProviderProps>) {
  const [convertContext, setConvertContext] = useState<ContextInterface | null>(
    null
  );

  useEffect(() => {
    async function initializeConvert() {
      try {
        // Access the ConvertSDK constructor from the module
        const ConvertInstance = (ConvertSDK as unknown as {
          default: ConvertSDKConstructor;
        }).default;
        // Instantiate the SDK
        const convertSDK = new ConvertInstance({
          sdkKey: "100412329/100412881",
          dataRefreshInterval: 180000,
          environment: "staging",
          logger: {
            logLevel: LogLevel.DEBUG,
          },
        });

        await convertSDK.onReady();

        const convertUserId = uuidv4();
        const context = convertSDK.createContext(convertUserId);

        if (!context) {
          console.error("Failed to create context.");
          return;
        }

        setConvertContext(context);
      } catch (error) {
        console.error("Error initializing Convert:", error);
      }
    }

    initializeConvert();
  }, []);

  return (
    <ConvertContext.Provider value={convertContext}>
      {children}
    </ConvertContext.Provider>
  );
}
