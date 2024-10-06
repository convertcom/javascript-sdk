var express = require("express");
var router = express.Router();

var experienceKey = "test-experience-ab-fullstack-1"; // [ConvertSDK]
var featureRolloutKey = "test-feature-rollout-1"; // [ConvertSDK]
var segmentsKey = "test-segment-1"; // [ConvertSDK]

function decide(sdkContext) {
  var output = {
    variation: false,
    callForActionLabel: false,
    feature: null,
  };

  var bucketedVariation = sdkContext.runExperience(experienceKey, {
    locationProperties: { location: "events" },
  });
  console.log("bucketed variation:", bucketedVariation);
  output.variation = bucketedVariation;

  var bucketedFeatureRollout = sdkContext.runExperience(featureRolloutKey, {
    locationProperties: { location: "events" },
  });
  console.log("bucketed feature rollout:", bucketedFeatureRollout);
  output.feature = !!bucketedFeatureRollout;

  if (!!bucketedFeatureRollout) {
    if (
      Array.isArray(bucketedFeatureRollout.changes) &&
      bucketedFeatureRollout.changes.length
    ) {
      if (
        bucketedFeatureRollout.changes[0].data &&
        bucketedFeatureRollout.changes[0].data.variables_data &&
        bucketedFeatureRollout.changes[0].data.variables_data.caption
      ) {
        output.callForActionLabel =
          bucketedFeatureRollout.changes[0].data.variables_data.caption;
      }
    }
  }
  // test custom segments
  sdkContext.setCustomSegments(segmentsKey, {
    ruleData: {
      enabled: false,
    },
  });

  return output;
} // [ConvertSDK]

/* GET events page. */
router.get("/", function (req, res, next) {
  // console.log("userId:", req.cookies.userId);
  var data = decide(req.sdkContext); // [ConvertSDK]
  res.render("events", {
    title: "Events",
    variation: data.variation,
    feature: data.feature,
    callForActionLabel: data.callForActionLabel,
  });
});

module.exports = router;
