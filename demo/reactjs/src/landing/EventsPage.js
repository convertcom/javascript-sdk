import React, {useState, useEffect, useContext} from 'react';
import AnimationRevealPage from 'helpers/AnimationRevealPage.js';
import Hero from 'components/hero/BackgroundAsImageWithCenteredContent.js';
import RolloutFeature from 'components/features/TwoColWithButton.js';
import Footer from 'components/footers/SimpleFiveColumn.js';

import {UserContext} from '../Context';

const experienceKey = 'test-experience-ab-fullstack-1'; // [ConvertSDK]
const featureRolloutKey = 'test-feature-rollout-1'; // [ConvertSDK]
const segmentsKey = 'test-segment-1'; // [ConvertSDK]

export default () => {
  const {sdkContext} = useContext(UserContext);
  const [useVariation, setVariation] = useState(false);
  const [useFeature, setFeature] = useState(false);
  const [caption, setCaption] = useState('Learn more');

  const decide = () => {
    const bucketedVariation = sdkContext.runExperience(experienceKey, {
      locationProperties: {location: 'events'}
    });
    console.log('bucketed variation:', bucketedVariation);
    setVariation(!!bucketedVariation);

    const bucketedFeatureRollout = sdkContext.runExperience(featureRolloutKey, {
      locationProperties: {location: 'events'}
    });
    console.log('bucketed feature rollout:', bucketedFeatureRollout);
    setFeature(!!bucketedFeatureRollout);
    if (bucketedFeatureRollout)
      setCaption(
        bucketedFeatureRollout?.changes?.[0]?.data?.variables_data?.caption
      );
    // test custom segments
    sdkContext.setCustomSegments(segmentsKey, {
      ruleData: {
        enabled: true
      }
    });
  }; // [ConvertSDK]

  useEffect(() => sdkContext && decide(), [sdkContext]);

  return (
    <AnimationRevealPage>
      <Hero useVariation={useVariation} />
      {useFeature && (
        <RolloutFeature
          primaryButtonUrl="#"
          heading={caption}
          subheading="Feature Rollout"
        />
      )}
      <Footer />
    </AnimationRevealPage>
  );
};
