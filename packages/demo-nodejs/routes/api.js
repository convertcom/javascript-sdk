var express = require("express");
var router = express.Router();

function primaryButtonAction(sdkContext, goalKey) {
  sdkContext.trackConversion(goalKey, {
    ruleData: {
      action: "buy",
    },
    conversionData: [
      {
        amount: 10.3,
        productsCount: 2,
      },
    ],
  });
} // [ConvertSDK]

/* POST buy route. */
router.post("/buy", function (req, res, next) {
  // console.log("userId:", req.cookies.userId);
  console.log("goalKey:", req.body.goalKey);
  primaryButtonAction(req.sdkContext, req.body.goalKey);
  res.render("buy");
});

module.exports = router;
