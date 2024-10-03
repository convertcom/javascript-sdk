import React, {useContext} from 'react';
import tw from 'twin.macro';
import AnimationRevealPage from 'helpers/AnimationRevealPage.js';
import Hero from 'components/hero/FullWidthWithImage.js';
import HeroEmpty from 'components/hero/Empty.js';
import Pricing from 'components/pricing/ThreePlans.js';
import Footer from 'components/footers/FiveColumnWithBackground.js';
import {useDecideFeature} from 'helpers/useDecideFeature';
import {UserContext} from '../Context';

const featureKey = 'feature-5'; // [ConvertSDK]
const goalKey = 'button-primary-click'; // [ConvertSDK]

export default () => {
  const Subheading = tw.span`uppercase tracking-widest font-bold text-primary-500`;
  const HighlightedText = tw.span`text-primary-500`;

  const {sdkContext} = useContext(UserContext);
  const {experiences, feature} = useDecideFeature(featureKey, sdkContext);

  return (
    <AnimationRevealPage>
      {experiences.find((e) => e === 'test-experience-ab-fullstack-1') ? (
        <Hero />
      ) : (
        <HeroEmpty />
      )}
      <Pricing
        subheading={<Subheading>Pricing</Subheading>}
        heading={
          <>
            Reasonable & Flexible <HighlightedText>Plans.</HighlightedText>
          </>
        }
        plans={
          feature?.variables?.plans
            ? Object.values(feature.variables.plans)
            : [
                {
                  name: 'Personal',
                  price: '$17.99',
                  duration: 'Monthly',
                  mainFeature: 'For Individuals',
                  features: [
                    '30 Templates',
                    '7 Landing Pages',
                    '12 Internal Pages',
                  'Basic Assistance'
                  ]
                },
                {
                  name: 'Business',
                  price: '$37.99',
                  duration: 'Monthly',
                  mainFeature: 'For Small Businesses',
                  features: [
                    '60 Templates',
                    '15 Landing Pages',
                    '22 Internal Pages',
                  'Priority Assistance'
                  ],
                  featured: true
                },
                {
                  name: 'Enterprise',
                  price: '$57.99',
                  duration: 'Monthly',
                  mainFeature: 'For Large Companies',
                  features: [
                    '90 Templates',
                    '27 Landing Pages',
                    '37 Internal Pages',
                  'Personal Assistance'
                  ]
                }
              ]
        }
        primaryButtonAction={() => {
          sdkContext.trackConversion(goalKey, {
            ruleData: {
              action: 'buy'
            },
            conversionData: [
              {
                key: 'amount',
                value: 10.3
              },
              {
                key: 'productsCount',
                value: 2
              }
            ]
          }); // [ConvertSDK]
        }}
      />
      <Footer />
    </AnimationRevealPage>
  );
};
