var express = require("express");
var router = express.Router();

var featureKey = "feature-5"; // [ConvertSDK]
var goalKey = "button-primary-click"; // [ConvertSDK]

function decide(sdkContext) {
  var output = {
    variations: [],
    feature: null,
  };

  var bucketedVariations = sdkContext.runExperiences({
    locationProperties: { location: "pricing" },
  });

  console.log("bucketed variation(s):", bucketedVariations);
  output.variations = bucketedVariations;

  var bucketedFeature = sdkContext.runFeature(featureKey, {
    locationProperties: { location: "pricing" },
  });
  console.log("bucketed feature:", bucketedFeature);
  if (bucketedFeature && bucketedFeature.status === "enabled") {
    output.feature = bucketedFeature;
  }

  return output;
} // [ConvertSDK]

/* GET pricing page. */
router.get("/", function (req, res, next) {
  // console.log("userId:", req.cookies.userId);
  var data = decide(req.sdkContext);
  res.render("pricing", {
    title: "Pricing",
    variations: data.variations,
    feature: data.feature,
    goalKey: goalKey,
    visitorId: req.cookies.userId,
  });
});

module.exports = router;
