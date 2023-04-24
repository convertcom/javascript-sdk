/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020-2022 Convert Insights, Inc
 * License Apache-2.0
 */
import e from"murmurhash";import{objectDeepValue as t}from"@convertcom/utils";import{MESSAGES as r}from"@convertcom/enums";class i{constructor(e,{loggerManager:i}={}){var s,a;this._max_traffic=1e4,this._hash_seed=9999,this._loggerManager=i,this._max_traffic=t(e,"bucketing.max_traffic",1e4,!0),this._hash_seed=t(e,"bucketing.hash_seed",9999,!0),null===(a=null===(s=this._loggerManager)||void 0===s?void 0:s.trace)||void 0===a||a.call(s,r.BUCKETING_CONSTRUCTOR,this)}selectBucket(e,t,r=0){var i,s;let a=null,l=0;return Object.keys(e).some((i=>(l+=100*e[i]+r,t<l&&(a=i,!0)))),null===(s=null===(i=this._loggerManager)||void 0===i?void 0:i.debug)||void 0===s||s.call(i,"BucketingManager.selectBucket()",{buckets:e,value:t,redistribute:r},{variation:a}),a||null}getValueVisitorBased(t,r=this._hash_seed){var i,s;const a=e.v3(String(t),r)/4294967296*this._max_traffic,l=parseInt(String(a),10);return null===(s=null===(i=this._loggerManager)||void 0===i?void 0:i.debug)||void 0===s||s.call(i,"BucketingManager.getValueVisitorBased()",{visitorId:t,seed:r,val:a,result:l}),l}getBucketForVisitor(e,t,r=0,i){const s=this.getValueVisitorBased(t,i);return this.selectBucket(e,s,r)}}export{i as BucketingManager};
//# sourceMappingURL=index.min.mjs.map
