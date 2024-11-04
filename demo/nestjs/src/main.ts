import * as cookieParser from 'cookie-parser';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import ConvertContext from './convert/context'; // [ConvertSDK]
import ConvertSDK, { LogLevel } from '@convertcom/js-sdk'; // [ConvertSDK]
import type { ConvertConfig, ConvertInterface } from '@convertcom/js-sdk'; // [ConvertSDK]
import DataStore from './convert/datastore';

const PORT = typeof process.env.PORT !== 'undefined' ? process.env.PORT : 3004;

const dataStore = new DataStore();

const sdkConfig: ConvertConfig = {
  environment: 'live',
  sdkKey: '10035569/10034190',
  dataStore, // optional
  logger: {
    logLevel: LogLevel.DEBUG,
  },
}; // [ConvertSDK]

const sdkInstance: ConvertInterface = new ConvertSDK(sdkConfig);

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  app.use(cookieParser());
  app.use(ConvertContext(sdkInstance, dataStore)); // [ConvertSDK]
  await app.listen(PORT);
}
bootstrap();
