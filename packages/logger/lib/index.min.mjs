/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020-2022 Convert Insights, Inc
 * License Apache-2.0
 */
import{LogLevel as e,LogMethod as l}from"@convertcom/enums";const o=e.TRACE;class s{constructor(e=console,s=o,i){this._defaultMapper={[l.LOG]:l.LOG,[l.DEBUG]:l.DEBUG,[l.INFO]:l.INFO,[l.WARN]:l.WARN,[l.ERROR]:l.ERROR},this._clients=[],this.addClient(e,s,i)}_isValidLevel(l){return Object.values(e).includes(l)}_isValidMethod(e){return Object.values(l).includes(e)}_log(l,o,...s){this._clients.forEach((i=>{if(o>=i.level&&e.SILENT!==o){const e=i.sdk[i.mapper[l]];e?e.call(i.sdk,...s):(console.log(`Info: Unable to find method "${l}()" in client sdk:`,i.sdk.constructor.name),console[l](...s))}}))}log(e,...o){this._isValidLevel(e)?this._log(l.LOG,e,...o):console.error("Invalid Log Level")}trace(...o){this._log(l.LOG,e.TRACE,...o)}debug(...o){this._log(l.DEBUG,e.DEBUG,...o)}info(...o){this._log(l.INFO,e.INFO,...o)}warn(...o){this._log(l.WARN,e.WARN,...o)}error(...o){this._log(l.ERROR,e.ERROR,...o)}addClient(e=console,l=o,s){if(!e)return void console.error("Invalid Client SDK");if(!this._isValidLevel(l))return void console.error("Invalid Log Level");const i=Object.assign({},this._defaultMapper);s&&Object.keys(s).filter(this._isValidMethod).forEach((e=>{i[e]=s[e]})),this._clients.push({sdk:e,level:l,mapper:i})}}export{s as LogManager};
//# sourceMappingURL=index.min.mjs.map
