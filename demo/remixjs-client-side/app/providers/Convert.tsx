import {createContext, useContext, useEffect, useState, ReactNode} from 'react';
import ConvertSDK, {LogLevel, parsePreviewParam} from '@convertcom/js-sdk';
import {v4 as uuidv4} from 'uuid';
import type {ContextInterface} from '@convertcom/js-sdk';
interface ConvertProviderProps {
  children: ReactNode;
}

// Define the interfaces for the SDK instance and constructor
interface ConvertSDKInstance {
  onReady(): Promise<void>;
  createContext(
    userId: string,
    visitorProperties?: Record<string, unknown>
  ): ContextInterface | null;
}

interface ConvertSDKConstructor {
  new (options: {
    sdkKey: string;
    dataRefreshInterval: number;
    environment: string;
    logger: {logLevel: LogLevel};
  }): ConvertSDKInstance;
}

const ConvertContext = createContext<ContextInterface | null>(null);

export function useConvertContext() {
  return useContext(ConvertContext);
}

export function ConvertProvider({children}: Readonly<ConvertProviderProps>) {
  const [convertContext, setConvertContext] = useState<ContextInterface | null>(
    null
  );

  useEffect(() => {
    async function initializeConvert() {
      try {
        // Depending on how the bundler interops the SDK's CJS build, the default
        // import is either the constructor itself or a wrapper holding it on
        // `.default`. Accept both so this works under any bundler.
        const ConvertInstance = ((
          ConvertSDK as unknown as {default?: ConvertSDKConstructor}
        ).default ?? ConvertSDK) as unknown as ConvertSDKConstructor;
        // Instantiate the SDK
        const convertSDK = new ConvertInstance({
          sdkKey: '10035569/10034190',
          dataRefreshInterval: 180000,
          environment: 'staging',
          logger: {
            logLevel: LogLevel.DEBUG
          }
        });

        await convertSDK.onReady();

        const convertUserId = uuidv4();
        // Visitor properties and default segments must be supplied, or any
        // audience-gated experience is filtered out and runExperiences()
        // returns []. Mirrors the nodejs and server-side demos.
        const context = convertSDK.createContext(convertUserId, {
          mobile: true
        });

        if (!context) {
          console.error('Failed to create context.');
          return;
        }

        context.setDefaultSegments({country: 'US'});

        // Preview link support: ?convert_preview={experienceId}.{variationId}
        // forces that decision on this context (zero-trace). [ConvertSDK]
        const previewParam = new URLSearchParams(window.location.search).get(
          'convert_preview'
        );
        const preview = previewParam ? parsePreviewParam(previewParam) : null;
        if (preview) await context.setPreview(preview);

        setConvertContext(context);
      } catch (error) {
        console.error('Error initializing Convert:', error);
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
