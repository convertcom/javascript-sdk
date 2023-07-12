/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020-2022 Convert Insights, Inc
 * License Apache-2.0
 */
import e from"murmurhash";import{objectDeepValue as t}from"@convertcom/js-sdk-utils";import{MESSAGES as r}from"@convertcom/js-sdk-enums";class s{constructor(e,{loggerManager:s}={}){var i,a;this._max_traffic=1e4,this._hash_seed=9999,this._loggerManager=s,this._max_traffic=t(e,"bucketing.max_traffic",1e4,!0),this._hash_seed=t(e,"bucketing.hash_seed",9999,!0),null===(a=null===(i=this._loggerManager)||void 0===i?void 0:i.trace)||void 0===a||a.call(i,r.BUCKETING_CONSTRUCTOR,this)}selectBucket(e,t,r=0){var s,i;let a=null,l=0;return Object.keys(e).some((s=>(l+=100*e[s]+r,t<l&&(a=s,!0)))),null===(i=null===(s=this._loggerManager)||void 0===s?void 0:s.debug)||void 0===i||i.call(s,"BucketingManager.selectBucket()",{buckets:e,value:t,redistribute:r},{variation:a}),a||null}getValueVisitorBased(t,r=this._hash_seed){var s,i;const a=e.v3(String(t),r)/4294967296*this._max_traffic,l=parseInt(String(a),10);return null===(i=null===(s=this._loggerManager)||void 0===s?void 0:s.debug)||void 0===i||i.call(s,"BucketingManager.getValueVisitorBased()",{visitorId:t,seed:r,val:a,result:l}),l}getBucketForVisitor(e,t,r=0,s){const i=this.getValueVisitorBased(t,s);return this.selectBucket(e,i,r)}}export{s as BucketingManager};
//# sourceMappingURL=index.min.mjs.map
