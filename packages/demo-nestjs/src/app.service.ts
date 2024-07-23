import { Injectable } from '@nestjs/common';
import { BucketingError, RuleError } from '@convertcom/js-sdk'; // [ConvertSDK]
import type { BucketedVariation } from '@convertcom/js-sdk'; // [ConvertSDK]

const experienceKey = 'test-experience-ab-fullstack-1'; // [ConvertSDK]

@Injectable()
export class AppService {
  getHello(req): string {
    try {
      const { sdkContext, cookies } = req; // [ConvertSDK]
      console.log('cookies:', cookies);
      const bucketedVariation: BucketedVariation | RuleError | BucketingError =
        sdkContext.runExperience(experienceKey, {
          locationProperties: { location: 'events' },
        }); // [ConvertSDK]
      console.log('bucketed variation:', bucketedVariation);
    } catch (error) {
      console.error('SDK Error:', error);
    }
    return 'Hello World!';
  }
}
