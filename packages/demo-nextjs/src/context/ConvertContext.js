// context/ConvertContext.js

'use client';

import { createContext, useContext, useEffect, useState } from 'react';

const ConvertContext = createContext();

export function ConvertProvider({ children }) {
  const [sdkContext, setSdkContext] = useState(null);
  const [userId, setUserId] = useState(() => {
    // Access localStorage directly
    if (typeof window !== 'undefined') {
      const storedUserId = localStorage.getItem('user-id');
      return storedUserId || `${Date.now()}`;
    }
    return `${Date.now()}`;
  });

  useEffect(() => {
    // Save userId to localStorage
    if (typeof window !== 'undefined') {
      localStorage.setItem('user-id', userId);
    }

    fetch('/api/convert')
      .then((res) => res.json())
      .then((data) => {
        console.log('data', data);
        if(!data?.success) {
          console.error('Error fetching SDK context:', data?.error);
          return;
        }
        const { userId, context } = data?.data;
        setUserId(userId);
        setSdkContext(context);
        console.log('SDK Ready');
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

export function useConvert() {
  return useContext(ConvertContext);
}
