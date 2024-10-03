var expressStats = require('express');
var routerStats = expressStats.Router();

var featureKeyStats = 'feature-4'; // [ConvertSDK]

function decideStats(sdkContext) {
  var output = {
    variations: [],
    feature: null
  };

  var bucketedVariations = sdkContext.runExperiences({
    locationProperties: {location: 'statistics'}
  });

  console.log('bucketed variation(s):', bucketedVariations);
  output.variations = bucketedVariations;

  var bucketedFeatureStats = sdkContext.runFeature(featureKeyStats, {
    locationProperties: {location: 'statistics'}
  });
  console.log('bucketed feature:', bucketedFeatureStats);
  if (bucketedFeatureStats && bucketedFeatureStats.status === 'enabled') {
    output.feature = bucketedFeatureStats;
  }

  return output;
} // [ConvertSDK]

/* GET statistics page. */
routerStats.get('/', function (req, res, next) {
  // console.log("userId:", req.cookies.userId);
  var dataStats = decideStats(req.sdkContext);
  res.render('statistics', {
    title: 'Statistics',
    variations: dataStats.variations,
    feature: dataStats.feature
  });
});

module.exports = routerStats;
