import { Injectable } from '@nestjs/common';
import {
  BucketingError,
  BucketedVariation,
  RuleError,
} from '@convertcom/js-sdk'; // [ConvertSDK]

const experienceKey = 'test-experience-ab-fullstack-1'; // [ConvertSDK]

@Injectable()
export class AppService {
  getHello(req): string {
    try {
      const { sdkContext, cookies } = req; // [ConvertSDK]
      console.log('cookies:', cookies);
      const bucketedVariation: BucketedVariation | RuleError | BucketingError =
        sdkContext.runExperiences(experienceKey, {
          locationProperties: { screen: 'home' },
        }); // [ConvertSDK]
      console.log('bucketed variation:', bucketedVariation);
    } catch (error) {
      console.error('SDK Error:', error);
    }
    return 'Hello World!';
  }
}
