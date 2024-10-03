import React, {useContext} from 'react';
import AnimationRevealPage from 'helpers/AnimationRevealPage.js';
import Hero from 'components/hero/FullWidthWithImage.js';
import HeroEmpty from 'components/hero/Empty.js';
import MainFeature from 'components/features/TwoColSingleFeatureWithStats.js';
import Footer from 'components/footers/MiniCenteredFooter.js';
import {useDecideFeature} from 'helpers/useDecideFeature';
import {UserContext} from '../Context';
const featureKey = 'feature-4'; // [ConvertSDK]

export default () => {
  const {sdkContext} = useContext(UserContext);
  const {experiences, feature} = useDecideFeature(featureKey, sdkContext);

  return (
    <AnimationRevealPage>
      {experiences.find((e) => e === 'test-experience-ab-fullstack-1') ? (
        <Hero />
      ) : (
        <HeroEmpty />
      )}
      {experiences.find((e) => e === 'test-experience-ab-fullstack-2') && (
        <MainFeature
          useFeature={!!feature} // simulates feature flag - a feature that does not have variables (or not used)
          statistics={feature?.variables?.statistics}
        />
      )}
      <Footer />
    </AnimationRevealPage>
  );
};
