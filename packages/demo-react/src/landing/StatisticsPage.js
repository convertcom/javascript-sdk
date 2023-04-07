import React, { useState, useEffect, useContext } from "react";
import AnimationRevealPage from "helpers/AnimationRevealPage.js";
import Hero from "components/hero/FullWidthWithImage.js";
import HeroEmpty from "components/hero/Empty.js";
import MainFeature from "components/features/TwoColSingleFeatureWithStats.js";
import Footer from "components/footers/MiniCenteredFooter.js";

import { UserContext } from "../Context";

const featureKey = "feature-4"; // [ConvertSDK]

export default () => {
  const { sdkContext } = useContext(UserContext);
  const [experiences, setExperiences] = useState([]);
  const [feature, setFeature] = useState(null);

  const decide = () => {
    const bucketedVariations = sdkContext.runExperiences({
      locationProperties: { location: "statistics" },
    });

    console.log("bucketed variation(s):", bucketedVariations);
    setExperiences(bucketedVariations.map((e) => e.experienceKey));

    const bucketedFeature = sdkContext.runFeature(featureKey, {
      locationProperties: { location: "statistics" },
    });
    console.log("bucketed feature:", bucketedFeature);
    if (bucketedFeature && bucketedFeature.status === "enabled")
      setFeature(bucketedFeature);
  }; // [ConvertSDK]

  useEffect(() => sdkContext && decide(), [sdkContext]);

  return (
    <AnimationRevealPage>
      {experiences.find((e) => e === "test-experience-ab-fullstack-1") ? (
        <Hero />
      ) : (
        <HeroEmpty />
      )}
      {experiences.find((e) => e === "test-experience-ab-fullstack-2") && (
        <MainFeature
          useFeature={!!feature} // simulates feature flag - a feature that does not have variables (or not used)
          statistics={feature?.variables?.statistics}
        />
      )}
      <Footer />
    </AnimationRevealPage>
  );
};
