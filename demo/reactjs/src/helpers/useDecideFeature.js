import {useState, useEffect} from 'react';

export const useDecideFeature = (featureKey, sdkContext) => {
  const [experiences, setExperiences] = useState([]);
  const [feature, setFeature] = useState(null);

  const decide = () => {
    const bucketedVariations = sdkContext.runExperiences({
      locationProperties: {location: 'statistics'}
    });

    console.log('bucketed variation(s):', bucketedVariations);
    setExperiences(bucketedVariations.map((e) => e.experienceKey));

    const bucketedFeature = sdkContext.runFeature(featureKey, {
      locationProperties: {location: 'statistics'}
    });
    console.log('bucketed feature:', bucketedFeature);

    if (bucketedFeature && bucketedFeature.status === 'enabled') {
      setFeature(bucketedFeature);
    }
  }; // [ConvertSDK]

  useEffect(() => {
    if (sdkContext) {
      decide();
    }
  }, [sdkContext]);

  return {experiences, feature};
};
