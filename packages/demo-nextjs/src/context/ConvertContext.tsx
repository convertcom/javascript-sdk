'use client';

import { ConvertInterface } from '@convertcom/js-sdk';
import { createContext, useContext, useEffect, useState, ReactNode } from 'react';

// Define the shape of the context
interface ConvertContextType {
  sdkContext: ConvertInterface;
  userId: string;
  setUserId: (userId: string) => void;
}
// Define the initial context value
const ConvertContext = createContext<ConvertContextType | undefined>(undefined);
// Define props for the ConvertProvider
interface ConvertProviderProps {
  children: ReactNode;
}
export function ConvertProvider({ children }: ConvertProviderProps) {
  const [sdkContext, setSdkContext] = useState<ConvertInterface >({} as ConvertInterface);
  const [userId, setUserId] = useState<string>(() => {
    if (typeof window !== 'undefined') {
      const storedUserId = localStorage.getItem('user-id');
      return storedUserId || `${Date.now()}`;
    }
    return `${Date.now()}`;
  });

  useEffect(() => {
    if (typeof window !== 'undefined') {
      localStorage.setItem('user-id', userId);
    }

    fetch('/api/convert')
      .then((res) => res.json())
      .then((data) => {
        if (!data) {
          return;
        }
        const { _visitorId } = data;
        setUserId(_visitorId);
        setSdkContext(data);
      })
      .catch((error) => {
        console.error('Error fetching SDK context:', error);
      });
  }, [userId]);

  return (
    <ConvertContext.Provider value={{ sdkContext, userId, setUserId }}>
      {children}
    </ConvertContext.Provider>
  );
}

// Custom hook to use the ConvertContext
export function useConvert() {
  const context = useContext(ConvertContext);
  if (context === undefined) {
    throw new Error('useConvert must be used within a ConvertProvider');
  }
  return context;
}
