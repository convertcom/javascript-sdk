module.exports = function (sdkInstance) {
  return function (req, res, next) {
    var userId =
      req.cookies && req.cookies.userId
        ? req.cookies.userId
        : `${new Date().getTime()}-${performance.now()}`;
    if (req.cookies && !req.cookies.userId) {
      res.cookie("userId", userId, {
        maxAge: 360000, // expires in 1 hour
      });
    }
    sdkInstance
      .onReady()
      .then(function () {
        console.log("SDK Ready");
        var context = sdkInstance.createContext(userId, {
          mobile: true,
        });
        context.setDefaultSegments({ country: "US" });
        req.sdkContext = context;
        next();
      })
      .catch(function (error) {
        console.error("SDK Error:", error);
        next();
      }); // [ConvertSDK]
  };
};
