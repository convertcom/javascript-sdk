(()=>{var e={};e.id=409,e.ids=[409],e.modules={2934:e=>{"use strict";e.exports=require("next/dist/client/components/action-async-storage.external.js")},4580:e=>{"use strict";e.exports=require("next/dist/client/components/request-async-storage.external.js")},5869:e=>{"use strict";e.exports=require("next/dist/client/components/static-generation-async-storage.external.js")},399:e=>{"use strict";e.exports=require("next/dist/compiled/next-server/app-page.runtime.prod.js")},2344:(e,t,r)=>{"use strict";r.r(t),r.d(t,{GlobalError:()=>o.a,__next_app__:()=>x,originalPathname:()=>u,pages:()=>c,routeModule:()=>p,tree:()=>i}),r(7352),r(5866),r(5490);var s=r(3191),n=r(8716),a=r(7922),o=r.n(a),d=r(5231),l={};for(let e in d)0>["default","tree","pages","GlobalError","originalPathname","__next_app__","routeModule"].indexOf(e)&&(l[e]=()=>d[e]);r.d(t,l);let i=["",{children:["/_not-found",{children:["__PAGE__",{},{page:[()=>Promise.resolve().then(r.t.bind(r,5866,23)),"next/dist/client/components/not-found-error"]}]},{}]},{layout:[()=>Promise.resolve().then(r.bind(r,5490)),"/Users/joe/Desktop/sdk/javascript-sdk/packages/demo-nextjs/src/app/layout.tsx"],"not-found":[()=>Promise.resolve().then(r.t.bind(r,5866,23)),"next/dist/client/components/not-found-error"]}],c=[],u="/_not-found/page",x={require:r,loadChunk:()=>Promise.resolve()},p=new s.AppPageRouteModule({definition:{kind:n.x.APP_PAGE,page:"/_not-found/page",pathname:"/_not-found",bundlePath:"",filename:"",appPaths:[]},userland:{loaderTree:i}})},9149:(e,t,r)=>{Promise.resolve().then(r.t.bind(r,2994,23)),Promise.resolve().then(r.t.bind(r,6114,23)),Promise.resolve().then(r.t.bind(r,9727,23)),Promise.resolve().then(r.t.bind(r,9671,23)),Promise.resolve().then(r.t.bind(r,1868,23)),Promise.resolve().then(r.t.bind(r,4759,23))},492:(e,t,r)=>{Promise.resolve().then(r.bind(r,8015)),Promise.resolve().then(r.bind(r,8007))},8015:(e,t,r)=>{"use strict";r.d(t,{default:()=>o});var s=r(326),n=r(7577),a=r(434);let o=function(){let[e,t]=(0,n.useState)(!1);return(0,s.jsxs)("nav",{className:"bg-white shadow-md fixed w-full z-10 top-0",children:[s.jsx("div",{className:"max-w-7xl mx-auto px-4 sm:px-6 lg:px-8",children:(0,s.jsxs)("div",{className:"flex items-center justify-between h-16",children:[s.jsx("div",{className:"flex-shrink-0",children:s.jsx(a.default,{href:"/",children:s.jsx("span",{className:"text-xl font-bold text-gray-800",children:"MyApp"})})}),s.jsx("div",{className:"hidden md:block",children:(0,s.jsxs)("div",{className:"ml-10 flex items-baseline space-x-4",children:[s.jsx(a.default,{href:"/",children:s.jsx("span",{className:"text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium",children:"Home"})}),s.jsx(a.default,{href:"/about",children:s.jsx("span",{className:"text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium",children:"About"})}),s.jsx(a.default,{href:"/services",children:s.jsx("span",{className:"text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium",children:"Services"})}),s.jsx(a.default,{href:"/contact",children:s.jsx("span",{className:"text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium",children:"Contact"})})]})}),s.jsx("div",{className:"md:hidden",children:s.jsx("button",{onClick:()=>t(!e),type:"button",className:"text-gray-800 hover:text-gray-600 focus:outline-none focus:text-gray-600","aria-label":"toggle menu",children:s.jsx("svg",{viewBox:"0 0 24 24",className:"h-6 w-6 fill-current",children:e?s.jsx("path",{fillRule:"evenodd",clipRule:"evenodd",d:"M18.3 5.71L12 12l6.3 6.29-1.42 1.42L12 14.84l-6.29 6.29-1.42-1.42L10.58 12 4.29 5.71 5.71 4.29 12 10.58l6.29-6.29 1.42 1.42z"}):s.jsx("path",{fillRule:"evenodd",d:"M4 5h16v2H4V5zm0 6h16v2H4v-2zm0 6h16v2H4v-2z"})})})})]})}),e&&s.jsx("div",{className:"md:hidden",children:(0,s.jsxs)("div",{className:"px-2 pt-2 pb-3 space-y-1 sm:px-3",children:[s.jsx(a.default,{href:"/",children:s.jsx("span",{className:"block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium",children:"Home"})}),s.jsx(a.default,{href:"/about",children:s.jsx("span",{className:"block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium",children:"About"})}),s.jsx(a.default,{href:"/services",children:s.jsx("span",{className:"block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium",children:"Services"})}),s.jsx(a.default,{href:"/contact",children:s.jsx("span",{className:"block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium",children:"Contact"})})]})})]})}},8007:(e,t,r)=>{"use strict";r.d(t,{ConvertProvider:()=>o,t:()=>d});var s=r(326),n=r(7577);let a=(0,n.createContext)(void 0);function o({children:e}){let[t,r]=(0,n.useState)({}),[o,d]=(0,n.useState)(()=>`${Date.now()}`);return s.jsx(a.Provider,{value:{sdkContext:t,userId:o,setUserId:d},children:e})}function d(){let e=(0,n.useContext)(a);if(void 0===e)throw Error("useConvert must be used within a ConvertProvider");return e}},6399:(e,t)=>{"use strict";Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var r in t)Object.defineProperty(e,r,{enumerable:!0,get:t[r]})}(t,{isNotFoundError:function(){return n},notFound:function(){return s}});let r="NEXT_NOT_FOUND";function s(){let e=Error(r);throw e.digest=r,e}function n(e){return"object"==typeof e&&null!==e&&"digest"in e&&e.digest===r}("function"==typeof t.default||"object"==typeof t.default&&null!==t.default)&&void 0===t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},7352:(e,t,r)=>{"use strict";Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var r in t)Object.defineProperty(e,r,{enumerable:!0,get:t[r]})}(t,{PARALLEL_ROUTE_DEFAULT_PATH:function(){return n},default:function(){return a}});let s=r(6399),n="next/dist/client/components/parallel-route-default.js";function a(){(0,s.notFound)()}("function"==typeof t.default||"object"==typeof t.default&&null!==t.default)&&void 0===t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},5490:(e,t,r)=>{"use strict";r.r(t),r.d(t,{default:()=>p,metadata:()=>x});var s=r(9510),n=r(5326),a=r.n(n),o=r(1409),d=r.n(o);r(5023);var l=r(8570);let i=(0,l.createProxy)(String.raw`/Users/joe/Desktop/sdk/javascript-sdk/packages/demo-nextjs/src/components/Navbar.tsx#default`);r(1159);let c=()=>s.jsx("footer",{className:"bg-white w-full h-16 flex items-center justify-center",children:s.jsx("span",{className:"text-gray-800 font-medium",children:"Convert NextJs Demo"})}),u=(0,l.createProxy)(String.raw`/Users/joe/Desktop/sdk/javascript-sdk/packages/demo-nextjs/src/context/ConvertContext.tsx#ConvertProvider`);(0,l.createProxy)(String.raw`/Users/joe/Desktop/sdk/javascript-sdk/packages/demo-nextjs/src/context/ConvertContext.tsx#useConvert`);let x={title:"Create Next App",description:"Generated by create next app"};function p({children:e}){return s.jsx("html",{lang:"en",children:(0,s.jsxs)("body",{className:`${a().variable} ${d().variable} antialiased`,children:[s.jsx(i,{}),s.jsx("div",{className:"pt-16 h-[calc(100vh-64px)]",children:s.jsx(u,{children:e})}),s.jsx(c,{})]})})}},5023:()=>{}};var t=require("../../webpack-runtime.js");t.C(e);var r=e=>t(t.s=e),s=t.X(0,[948,902],()=>r(2344));module.exports=s})();