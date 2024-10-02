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
        if(!data) {
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

export function useConvert() {
  return useContext(ConvertContext);
}
