import type { ContextInterface } from '@convertcom/js-sdk'; // [ConvertSDK]

export default function (sdkInstance, dataStore) {
  return function (req, res, next) {
    if (dataStore.driver === 'cookie') {
      dataStore.response = res;
      if (!Object.keys(dataStore.data).length) dataStore.data = req.cookies;
    }
    const userId =
      req.cookies && req.cookies.userId
        ? req.cookies.userId
        : `${new Date().getTime()}-${performance.now()}`;
    if (req.cookies && !req.cookies.userId) {
      res.cookie('userId', userId, {
        maxAge: 360000, // expires in 1 hour
      });
    }
    sdkInstance
      .onReady()
      .then(function () {
        console.log('SDK Ready');
        const context: ContextInterface = sdkInstance.createContext(userId, {
          mobile: true,
        });
        context.setDefaultSegments({ country: 'US' });
        req.sdkContext = context;
        next();
      })
      .catch(function (error) {
        console.error('SDK Error:', error);
        next();
      }); // [ConvertSDK]
  };
}
