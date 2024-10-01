// app/api/convert/route.js
import sdkInstance from '@/lib/convertSdk';
export async function GET() {
  try {
    await sdkInstance.onReady();
    console.log('SDK Ready');

    // You can create a context here if needed
    const userId = 'user-id-123'; // Replace with actual user ID
    const context = sdkInstance.createContext(userId, {
      mobile: false,
    });
    context.setDefaultSegments({ country: 'US' });

    // Perform SDK operations...
    const contextData = {
        userId,
        context: context,
        // Include any other necessary data from the context
      };
    return new Response(JSON.stringify({ success: true , data: contextData }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    });
  } catch (error) {
    console.error('SDK Error:', error);
    return new Response(JSON.stringify({ error: 'SDK Initialization Failed' }), {
      status: 500,
      headers: { 'Content-Type': 'application/json' },
    });
  }
}
