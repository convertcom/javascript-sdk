/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020-2022 Convert Insights, Inc
 * License Apache-2.0
 */
import{MESSAGES as e}from"@convertcom/js-sdk-enums";class t{constructor(t,{dataManager:a,loggerManager:r}){var i,n;this._dataManager=a,this._loggerManager=r,null===(n=null===(i=this._loggerManager)||void 0===i?void 0:i.trace)||void 0===n||n.call(i,e.EXPERIENCE_CONSTRUCTOR)}getList(){return this._dataManager.getEntitiesList("experiences")}getExperience(e){return this._dataManager.getEntity(e,"experiences")}getExperienceById(e){return this._dataManager.getEntityById(e,"experiences")}getExperiences(e){return this._dataManager.getItemsByKeys(e,"experiences")}selectVariation(e,t,a,r,i){return this._dataManager.getBucketing(e,t,a,r,i)}selectVariationById(e,t,a,r,i){return this._dataManager.getBucketingById(e,t,a,r,i)}selectVariations(e,t,a,r){return this.getList().map((i=>this.selectVariation(e,null==i?void 0:i.key,t,a,r))).filter(Boolean)}getVariation(e,t){return this._dataManager.getSubItem("experiences",e,"variations",t,"key","key")}getVariationById(e,t){return this._dataManager.getSubItem("experiences",e,"variations",t,"id","id")}}export{t as ExperienceManager};
//# sourceMappingURL=index.min.mjs.map
