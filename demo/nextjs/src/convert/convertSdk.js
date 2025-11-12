// lib/convertSdk.js
import ConvertSDK from './sdkWrapper'; // [ConvertSDK]
import DataStore from './DataStore';
const dataStore = new DataStore();
const sdkConfig = {
  sdkKey: '10035569/10034190', // [ConvertSDK]
  dataStore // // [ConvertSDK] optional
};
const sdkInstance = new ConvertSDK(sdkConfig); // [ConvertSDK]
export default sdkInstance;
