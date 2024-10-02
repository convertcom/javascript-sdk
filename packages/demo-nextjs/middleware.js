// middleware.js

import { NextResponse } from 'next/server';
import sdkInstance from '@/lib/convertSdk';
export async function middleware(request) {
  try {
    await sdkInstance.onReady();

    const userId = request.cookies.get('user-id') || 'default-user-id';
    const context = sdkInstance.createContext(userId, {
      mobile: false,
    });
    context.setDefaultSegments({ country: 'US' });

    // You can attach the context to the request if needed
    request.convertContext = context;

    // Continue processing the request
    return NextResponse.next();
  } catch (error) {
    console.error('SDK Error:', error);
    return NextResponse.next();
  }
}

export const config = {
    matcher: ['/((?!api|_next/static|_next/image|favicon.ico).*)'],
  };