var express = require("express");
var router = express.Router();

var featureKey = "feature-4"; // [ConvertSDK]

function decide(sdkContext) {
  var output = {
    variations: [],
    feature: null,
  };

  var bucketedVariations = sdkContext.runExperiences({
    locationProperties: { location: "statistics" },
  });

  console.log("bucketed variation(s):", bucketedVariations);
  output.variations = bucketedVariations;

  var bucketedFeature = sdkContext.runFeature(featureKey, {
    locationProperties: { location: "statistics" },
  });
  console.log("bucketed feature:", bucketedFeature);
  if (bucketedFeature && bucketedFeature.status === "enabled") {
    output.feature = bucketedFeature;
  }

  return output;
} // [ConvertSDK]

/* GET statistics page. */
router.get("/", function (req, res, next) {
  // console.log("userId:", req.cookies.userId);
  var data = decide(req.sdkContext);
  res.render("statistics", {
    title: "Statistics",
    variations: data.variations,
    feature: data.feature,
  });
});

module.exports = router;
