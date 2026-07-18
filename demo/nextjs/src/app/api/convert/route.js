// app/api/convert/route.js
import sdkInstance from '@/convert/convertSdk'; // [ConvertSDK]
import { parsePreviewParam } from '@/convert/sdkWrapper'; // [ConvertSDK]
export async function GET(request) {
  try {
    await sdkInstance.onReady();

    // You can create a context here if needed
    const userId = 'user-id-123'; // Replace with actual user ID // [ConvertSDK]
    const context = sdkInstance.createContext(userId, {
      mobile: false,
    }); // [ConvertSDK]
    context.setDefaultSegments({ country: 'US' }); // [ConvertSDK]

    // Preview link support: ?convert_preview={experienceId}.{variationId}
    // forces that decision on this context (zero-trace). [ConvertSDK]
    const previewParam = new URL(request.url).searchParams.get('convert_preview');
    const preview = previewParam ? parsePreviewParam(previewParam) : null;
    if (preview) await context.setPreview(preview); // [ConvertSDK]

    const replacer = (key, value) => {
      // Exclude properties causing circular reference issues
      if (key === '_idlePrev' || key === '_idleNext') {
        return undefined;
      }
      return value;
    };

    // Convert the context object to JSON, excluding specific properties
    const serializableContextData = JSON.stringify(context, replacer);

    return new Response( serializableContextData, {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    });
  } catch (error) {
    console.error('SDK Error:', error);
    return new Response(stringify({ error: 'SDK Initialization Failed' }), {
      status: 500,
      headers: { 'Content-Type': 'application/json' },
    });
  }
}
