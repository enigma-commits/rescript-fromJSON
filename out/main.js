var y=(e,r)=>()=>(r||e((r={exports:{}}).exports,r),r.exports);var j=y(d=>{"use strict";function tr(e,r){return e<r?-1:e===r?0:1}function ur(e,r){return e?r?0:1:r?-1:0}function ir(e,r){return e===r?0:e<r?-1:e>r||e===e?1:r===r?-1:0}function or(e,r){return e===r?0:e<r?-1:1}function cr(e,r){return e&&r}function fr(e,r){return e<r?e:r}function ar(e,r){return e<r?e:r}function lr(e,r){return e<r?e:r}function sr(e,r){return e||r}function vr(e,r){return e>r?e:r}function dr(e,r){return e>r?e:r}function _r(e,r){return e>r?e:r}function Y(e,r){return e[1]===r[1]?e[0]===r[0]:!1}function x(e,r){var n=r[0],t=e[0];return t>n?!0:t<n?!1:e[1]>=r[1]}function pr(e,r){return!Y(e,r)}function gr(e,r){return!x(e,r)}function C(e,r){return e[0]>r[0]?!0:e[0]<r[0]?!1:e[1]>r[1]}function hr(e,r){return!C(e,r)}function mr(e,r){return x(e,r)?r:e}function wr(e,r){return C(e,r)?e:r}d.int_compare=tr;d.bool_compare=ur;d.float_compare=ir;d.string_compare=or;d.bool_min=cr;d.int_min=fr;d.float_min=ar;d.string_min=lr;d.bool_max=sr;d.int_max=vr;d.float_max=dr;d.string_max=_r;d.i64_eq=Y;d.i64_neq=pr;d.i64_lt=gr;d.i64_gt=C;d.i64_le=hr;d.i64_ge=x;d.i64_min=mr;d.i64_max=wr});var q=y(A=>{"use strict";function Er(e,r,n){for(var t=new Array(n),u=0,i=r;u<n;)t[u]=e[i],u=u+1|0,i=i+1|0;return t}function yr(e,r){for(;;){var n=r,t=e;if(!n)return t;r=n.tl,e=n.hd.length+t|0}}function Ar(e,r,n){for(;;){var t=n,u=r;if(!t)return;for(var i=t.hd,o=i.length,c=u,f=0;f<o;)e[c]=i[f],c=c+1|0,f=f+1|0;n=t.tl,r=c}}function Ur(e){var r=yr(0,e),n=new Array(r);return Ar(n,0,e),n}function Or(e,r,n){if(r<0||r>=e.length)throw{RE_EXN_ID:"Invalid_argument",_1:"index out of bounds",Error:new Error};e[r]=n}function Dr(e,r){if(r<0||r>=e.length)throw{RE_EXN_ID:"Invalid_argument",_1:"index out of bounds",Error:new Error};return e[r]}function Br(e,r){for(var n=new Array(e),t=0;t<e;++t)n[t]=r;return n}function Nr(e){for(var r=new Array(e),n=0;n<e;++n)r[n]=0;return r}function Mr(e,r,n,t,u){if(t<=r){for(var i=0;i<u;++i)n[i+t|0]=e[i+r|0];return}for(var o=u-1|0;o>=0;--o)n[o+t|0]=e[o+r|0]}function Ir(e){return e.slice(0)}A.dup=Ir;A.sub=Er;A.concat=Ur;A.make=Br;A.make_float=Nr;A.blit=Mr;A.get=Dr;A.set=Or});var z=y(p=>{"use strict";var ee=q();function l(e,r){for(;;){var n=r,t=e,u=t.length,i=u===0?1:u,o=n.length,c=i-o|0;if(c===0)return t.apply(null,n);if(c>=0)return function(f,v){return function(h){return l(f,v.concat([h]))}}(t,n);r=ee.sub(n,i,-c|0),e=t.apply(null,ee.sub(n,0,i))}}function re(e,r){var n=e.length;if(n===1)return e(r);switch(n){case 1:return e(r);case 2:return function(t){return e(r,t)};case 3:return function(t,u){return e(r,t,u)};case 4:return function(t,u,i){return e(r,t,u,i)};case 5:return function(t,u,i,o){return e(r,t,u,i,o)};case 6:return function(t,u,i,o,c){return e(r,t,u,i,o,c)};case 7:return function(t,u,i,o,c,f){return e(r,t,u,i,o,c,f)};default:return l(e,[r])}}function br(e){var r=e.length;return r===1?e:function(n){return re(e,n)}}function ne(e,r,n){var t=e.length;if(t===2)return e(r,n);switch(t){case 1:return l(e(r),[n]);case 2:return e(r,n);case 3:return function(u){return e(r,n,u)};case 4:return function(u,i){return e(r,n,u,i)};case 5:return function(u,i,o){return e(r,n,u,i,o)};case 6:return function(u,i,o,c){return e(r,n,u,i,o,c)};case 7:return function(u,i,o,c,f){return e(r,n,u,i,o,c,f)};default:return l(e,[r,n])}}function kr(e){var r=e.length;return r===2?e:function(n,t){return ne(e,n,t)}}function te(e,r,n,t){var u=e.length;if(u===3)return e(r,n,t);switch(u){case 1:return l(e(r),[n,t]);case 2:return l(e(r,n),[t]);case 3:return e(r,n,t);case 4:return function(i){return e(r,n,t,i)};case 5:return function(i,o){return e(r,n,t,i,o)};case 6:return function(i,o,c){return e(r,n,t,i,o,c)};case 7:return function(i,o,c,f){return e(r,n,t,i,o,c,f)};default:return l(e,[r,n,t])}}function Sr(e){var r=e.length;return r===3?e:function(n,t,u){return te(e,n,t,u)}}function ue(e,r,n,t,u){var i=e.length;if(i===4)return e(r,n,t,u);switch(i){case 1:return l(e(r),[n,t,u]);case 2:return l(e(r,n),[t,u]);case 3:return l(e(r,n,t),[u]);case 4:return e(r,n,t,u);case 5:return function(o){return e(r,n,t,u,o)};case 6:return function(o,c){return e(r,n,t,u,o,c)};case 7:return function(o,c,f){return e(r,n,t,u,o,c,f)};default:return l(e,[r,n,t,u])}}function Fr(e){var r=e.length;return r===4?e:function(n,t,u,i){return ue(e,n,t,u,i)}}function ie(e,r,n,t,u,i){var o=e.length;if(o===5)return e(r,n,t,u,i);switch(o){case 1:return l(e(r),[n,t,u,i]);case 2:return l(e(r,n),[t,u,i]);case 3:return l(e(r,n,t),[u,i]);case 4:return l(e(r,n,t,u),[i]);case 5:return e(r,n,t,u,i);case 6:return function(c){return e(r,n,t,u,i,c)};case 7:return function(c,f){return e(r,n,t,u,i,c,f)};default:return l(e,[r,n,t,u,i])}}function Tr(e){var r=e.length;return r===5?e:function(n,t,u,i,o){return ie(e,n,t,u,i,o)}}function oe(e,r,n,t,u,i,o){var c=e.length;if(c===6)return e(r,n,t,u,i,o);switch(c){case 1:return l(e(r),[n,t,u,i,o]);case 2:return l(e(r,n),[t,u,i,o]);case 3:return l(e(r,n,t),[u,i,o]);case 4:return l(e(r,n,t,u),[i,o]);case 5:return l(e(r,n,t,u,i),[o]);case 6:return e(r,n,t,u,i,o);case 7:return function(f){return e(r,n,t,u,i,o,f)};default:return l(e,[r,n,t,u,i,o])}}function Wr(e){var r=e.length;return r===6?e:function(n,t,u,i,o,c){return oe(e,n,t,u,i,o,c)}}function ce(e,r,n,t,u,i,o,c){var f=e.length;if(f===7)return e(r,n,t,u,i,o,c);switch(f){case 1:return l(e(r),[n,t,u,i,o,c]);case 2:return l(e(r,n),[t,u,i,o,c]);case 3:return l(e(r,n,t),[u,i,o,c]);case 4:return l(e(r,n,t,u),[i,o,c]);case 5:return l(e(r,n,t,u,i),[o,c]);case 6:return l(e(r,n,t,u,i,o),[c]);case 7:return e(r,n,t,u,i,o,c);default:return l(e,[r,n,t,u,i,o,c])}}function Jr(e){var r=e.length;return r===7?e:function(n,t,u,i,o,c,f){return ce(e,n,t,u,i,o,c,f)}}function fe(e,r,n,t,u,i,o,c,f){var v=e.length;if(v===8)return e(r,n,t,u,i,o,c,f);switch(v){case 1:return l(e(r),[n,t,u,i,o,c,f]);case 2:return l(e(r,n),[t,u,i,o,c,f]);case 3:return l(e(r,n,t),[u,i,o,c,f]);case 4:return l(e(r,n,t,u),[i,o,c,f]);case 5:return l(e(r,n,t,u,i),[o,c,f]);case 6:return l(e(r,n,t,u,i,o),[c,f]);case 7:return l(e(r,n,t,u,i,o,c),[f]);default:return l(e,[r,n,t,u,i,o,c,f])}}function Rr(e){var r=e.length;return r===8?e:function(n,t,u,i,o,c,f,v){return fe(e,n,t,u,i,o,c,f,v)}}p.app=l;p._1=re;p.__1=br;p._2=ne;p.__2=kr;p._3=te;p.__3=Sr;p._4=ue;p.__4=Fr;p._5=ie;p.__5=Tr;p._6=oe;p.__6=Wr;p._7=ce;p.__7=Jr;p._8=fe;p.__8=Rr});var ae=y(T=>{"use strict";function xr(e,r){return e===r}var Cr=2147483647,qr=-2147483648;T.equal=xr;T.max=Cr;T.min=qr});var se=y(N=>{"use strict";var B=ae();function zr(e){return Math.ceil(e)}function le(e){return e>B.max?B.max:e<B.min?B.min:Math.ceil(e)}function Lr(e){return Math.floor(e)}function L(e){return e>B.max?B.max:e<B.min?B.min:Math.floor(e)}function Zr(e,r){return L(Math.random()*(r-e|0))+e|0}var Pr=le,Vr=L;N.unsafe_ceil=zr;N.ceil_int=le;N.ceil=Pr;N.unsafe_floor=Lr;N.floor_int=L;N.floor=Vr;N.random_int=Zr});var Z=y(U=>{"use strict";function Xr(e){return e.BS_PRIVATE_NESTED_SOME_NONE!==void 0}function W(e){return e===void 0?{BS_PRIVATE_NESTED_SOME_NONE:0}:e!==null&&e.BS_PRIVATE_NESTED_SOME_NONE!==void 0?{BS_PRIVATE_NESTED_SOME_NONE:e.BS_PRIVATE_NESTED_SOME_NONE+1|0}:e}function Gr(e){if(e!=null)return W(e)}function $r(e){if(e!==void 0)return W(e)}function Kr(e){if(e!==null)return W(e)}function ve(e){if(!(e!==null&&e.BS_PRIVATE_NESTED_SOME_NONE!==void 0))return e;var r=e.BS_PRIVATE_NESTED_SOME_NONE;if(r!==0)return{BS_PRIVATE_NESTED_SOME_NONE:r-1|0}}function Hr(e){if(e!==void 0)return ve(e)}function Qr(e){return e!==void 0?e.VAL:e}U.nullable_to_opt=Gr;U.undefined_to_opt=$r;U.null_to_opt=Kr;U.valFromOption=ve;U.some=W;U.isNested=Xr;U.option_get=Hr;U.option_unwrap=Qr});var xe=y(a=>{"use strict";var E=j(),s=z(),Yr=se(),P=Z();function jr(e,r){if(r>=0&&r<e.length)return P.some(e[r])}function en(e,r){if(!(r>=0&&r<e.length))throw{RE_EXN_ID:"Assert_failure",_1:["belt_Array.ml",35,2],Error:new Error};return e[r]}function rn(e,r,n){return r>=0&&r<e.length?(e[r]=n,!0):!1}function nn(e,r,n){if(!(r>=0&&r<e.length))throw{RE_EXN_ID:"Assert_failure",_1:["belt_Array.ml",45,2],Error:new Error};e[r]=n}function de(e,r,n){var t=e[r];e[r]=e[n],e[n]=t}function V(e){for(var r=e.length,n=0;n<r;++n)de(e,n,Yr.random_int(n,r))}function tn(e){var r=e.slice(0);return V(r),r}function un(e){for(var r=e.length,n=0,t=0,u=r/2|0;t<u;++t)de(e,n+t|0,((n+r|0)-t|0)-1|0)}function on(e){for(var r=e.length,n=new Array(r),t=0;t<r;++t)n[t]=e[(r-1|0)-t|0];return n}function cn(e,r){if(e<=0)return[];for(var n=new Array(e),t=0;t<e;++t)n[t]=r;return n}function X(e,r){if(e<=0)return[];for(var n=new Array(e),t=0;t<e;++t)n[t]=r(t);return n}function fn(e,r){return X(e,s.__1(r))}function _e(e,r){var n=X(e,r);return V(n),n}function an(e,r){return _e(e,s.__1(r))}function ln(e,r){var n=r-e|0;if(n<0)return[];for(var t=new Array(n+1|0),u=0;u<=n;++u)t[u]=e+u|0;return t}function sn(e,r,n){var t=r-e|0;if(t<0||n<=0)return[];for(var u=(t/n|0)+1|0,i=new Array(u),o=e,c=0;c<u;++c)i[c]=o,o=o+n|0;return i}function vn(e,r){for(var n=e.length,t=r.length,u=n<t?n:t,i=new Array(u),o=0;o<u;++o)i[o]=[e[o],r[o]];return i}function pe(e,r,n){for(var t=e.length,u=r.length,i=t<u?t:u,o=new Array(i),c=0;c<i;++c)o[c]=n(e[c],r[c]);return o}function dn(e,r,n){return pe(e,r,s.__2(n))}function _n(e,r){for(var n=e.length,t=r.length,u=new Array(n+t|0),i=0;i<n;++i)u[i]=e[i];for(var o=0;o<t;++o)u[n+o|0]=r[o];return u}function G(e){for(var r=e.length,n=0,t=0;t<r;++t)n=n+e[t].length|0;var u=new Array(n);n=0;for(var i=0;i<r;++i)for(var o=e[i],c=0,f=o.length;c<f;++c)u[n]=o[c],n=n+1|0;return u}function pn(e,r,n){if(n<=0)return[];var t=e.length,u=r<0?E.int_max(t+r|0,0):r,i=t-u|0,o=i<n?i:n;if(o<=0)return[];for(var c=new Array(o),f=0;f<o;++f)c[f]=e[u+f|0];return c}function gn(e,r){for(var n=e.length,t=r<0?E.int_max(n+r|0,0):r,u=n>t?n-t|0:0,i=new Array(u),o=0;o<u;++o)i[o]=e[t+o|0];return i}function hn(e,r,n,t){if(!(n<=0)){var u=e.length,i=r<0?E.int_max(u+r|0,0):r,o=u-i|0,c=o<n?o:n;if(!(c<=0))for(var f=i,v=i+c|0;f<v;++f)e[f]=t}}function mn(e,r,n,t,u){if(t<=r){for(var i=0;i<u;++i)n[i+t|0]=e[i+r|0];return}for(var o=u-1|0;o>=0;--o)n[o+t|0]=e[o+r|0]}function wn(e,r,n,t,u){var i=e.length,o=n.length,c=r<0?E.int_max(i+r|0,0):r,f=t<0?E.int_max(o+t|0,0):t,v=E.int_min(u,E.int_min(i-c|0,o-f|0));if(f<=c){for(var h=0;h<v;++h)n[h+f|0]=e[h+c|0];return}for(var D=v-1|0;D>=0;--D)n[D+f|0]=e[D+c|0]}function ge(e,r){for(var n=0,t=e.length;n<t;++n)r(e[n])}function En(e,r){ge(e,s.__1(r))}function J(e,r){for(var n=e.length,t=new Array(n),u=0;u<n;++u)t[u]=r(e[u]);return t}function yn(e,r){return J(e,s.__1(r))}function An(e,r){return G(J(e,r))}function Un(e,r){return G(J(e,s.__1(r)))}function he(e,r){for(var n=e.length,t=0,u;u===void 0&&t<n;){var i=e[t];r(i)&&(u=P.some(i)),t=t+1|0}return u}function On(e,r){return he(e,s.__1(r))}function me(e,r){for(var n=e.length,t=0,u;u===void 0&&t<n;){var i=e[t];r(i)&&(u=t),t=t+1|0}return u}function Dn(e,r){return me(e,s.__1(r))}function we(e,r){for(var n=e.length,t=new Array(n),u=0,i=0;i<n;++i){var o=e[i];r(o)&&(t[u]=o,u=u+1|0)}return t.length=u,t}function Bn(e,r){return we(e,s.__1(r))}function Ee(e,r){for(var n=e.length,t=new Array(n),u=0,i=0;i<n;++i){var o=e[i];r(o,i)&&(t[u]=o,u=u+1|0)}return t.length=u,t}function Nn(e,r){return Ee(e,s.__2(r))}function ye(e,r){for(var n=e.length,t=new Array(n),u=0,i=0;i<n;++i){var o=e[i],c=r(o);c!==void 0&&(t[u]=P.valFromOption(c),u=u+1|0)}return t.length=u,t}function Mn(e,r){return ye(e,s.__1(r))}function Ae(e,r){for(var n=0,t=e.length;n<t;++n)r(n,e[n])}function In(e,r){Ae(e,s.__2(r))}function Ue(e,r){for(var n=e.length,t=new Array(n),u=0;u<n;++u)t[u]=r(u,e[u]);return t}function bn(e,r){return Ue(e,s.__2(r))}function Oe(e,r,n){for(var t=r,u=0,i=e.length;u<i;++u)t=n(t,e[u]);return t}function kn(e,r,n){return Oe(e,r,s.__2(n))}function De(e,r,n){for(var t=r,u=e.length-1|0;u>=0;--u)t=n(t,e[u]);return t}function Sn(e,r,n){return De(e,r,s.__2(n))}function Be(e,r,n,t){for(var u=n,i=E.int_min(e.length,r.length),o=i-1|0;o>=0;--o)u=t(u,e[o],r[o]);return u}function Fn(e,r,n,t){return Be(e,r,n,s.__3(t))}function Ne(e,r,n){for(var t=r,u=0,i=e.length;u<i;++u)t=n(t,e[u],u);return t}function Tn(e,r,n){return Ne(e,r,s.__3(n))}function Me(e,r){for(var n=e.length,t=0;;){var u=t;if(u===n)return!0;if(!r(e[u]))return!1;t=u+1|0}}function Wn(e,r){return Me(e,s.__1(r))}function Ie(e,r){for(var n=e.length,t=0;;){var u=t;if(u===n)return!1;if(r(e[u]))return!0;t=u+1|0}}function Jn(e,r){return Ie(e,s.__1(r))}function be(e,r,n,t,u){for(;;){var i=n;if(i===u)return!0;if(!t(e[i],r[i]))return!1;n=i+1|0}}function ke(e,r,n){return be(e,r,0,n,E.int_min(e.length,r.length))}function Rn(e,r,n){return ke(e,r,s.__2(n))}function Se(e,r,n){for(var t=0,u=E.int_min(e.length,r.length);;){var i=t;if(i===u)return!1;if(n(e[i],r[i]))return!0;t=i+1|0}}function xn(e,r,n){return Se(e,r,s.__2(n))}function Fe(e,r,n){var t=e.length,u=r.length;return t===u?be(e,r,0,n,t):!1}function Cn(e,r,n){return Fe(e,r,s.__2(n))}function Te(e,r,n){var t=e.length,u=r.length;if(t>u)return 1;if(t<u)return-1;for(var i=0;;){var o=i;if(o===t)return 0;var c=n(e[o],r[o]);if(c!==0)return c;i=o+1|0}}function qn(e,r,n){return Te(e,r,s.__2(n))}function We(e,r){for(var n=e.length,t=0,u=0,i=new Array(n),o=new Array(n),c=0;c<n;++c){var f=e[c];r(f)?(i[t]=f,t=t+1|0):(o[u]=f,u=u+1|0)}return i.length=t,o.length=u,[i,o]}function zn(e,r){return We(e,s.__1(r))}function Ln(e){for(var r=e.length,n=new Array(r),t=new Array(r),u=0;u<r;++u){var i=e[u];n[u]=i[0],t[u]=i[1]}return[n,t]}function Je(e,r,n){var t=e.length;if(t===0)return"";for(var u=t-1|0,i=0,o="";;){var c=o,f=i;if(f===u)return c+n(e[f]);o=c+(n(e[f])+r),i=f+1|0}}function Zn(e,r,n){return Je(e,r,s.__1(n))}function Re(e,r){for(var n=new Array(e),t=0;t<e;++t)n[t]=r(t);return n}function Pn(e,r){return Re(e,s.__1(r))}a.get=jr;a.getExn=en;a.set=rn;a.setExn=nn;a.shuffleInPlace=V;a.shuffle=tn;a.reverseInPlace=un;a.reverse=on;a.make=cn;a.range=ln;a.rangeBy=sn;a.makeByU=X;a.makeBy=fn;a.makeByAndShuffleU=_e;a.makeByAndShuffle=an;a.zip=vn;a.zipByU=pe;a.zipBy=dn;a.unzip=Ln;a.concat=_n;a.concatMany=G;a.slice=pn;a.sliceToEnd=gn;a.fill=hn;a.blit=wn;a.blitUnsafe=mn;a.forEachU=ge;a.forEach=En;a.mapU=J;a.map=yn;a.flatMapU=An;a.flatMap=Un;a.getByU=he;a.getBy=On;a.getIndexByU=me;a.getIndexBy=Dn;a.keepU=we;a.keep=Bn;a.keepWithIndexU=Ee;a.keepWithIndex=Nn;a.keepMapU=ye;a.keepMap=Mn;a.forEachWithIndexU=Ae;a.forEachWithIndex=In;a.mapWithIndexU=Ue;a.mapWithIndex=bn;a.partitionU=We;a.partition=zn;a.reduceU=Oe;a.reduce=kn;a.reduceReverseU=De;a.reduceReverse=Sn;a.reduceReverse2U=Be;a.reduceReverse2=Fn;a.reduceWithIndexU=Ne;a.reduceWithIndex=Tn;a.joinWithU=Je;a.joinWith=Zn;a.someU=Ie;a.some=Jn;a.everyU=Me;a.every=Wn;a.every2U=ke;a.every2=Rn;a.some2U=Se;a.some2=xn;a.cmpU=Te;a.cmp=qn;a.eqU=Fe;a.eq=Cn;a.initU=Re;a.init=Pn});var Xe=y(_=>{"use strict";var M=z(),w=Z();function Ce(e,r){if(e!==void 0&&r(w.valFromOption(e)))return e}function Vn(e,r){return Ce(e,M.__1(r))}function qe(e,r){if(e!==void 0)return r(w.valFromOption(e))}function Xn(e,r){qe(e,M.__1(r))}function Gn(e){if(e!==void 0)return w.valFromOption(e);throw{RE_EXN_ID:"Not_found",Error:new Error}}function ze(e,r,n){return e!==void 0?n(w.valFromOption(e)):r}function $n(e,r,n){return ze(e,r,M.__1(n))}function Le(e,r){if(e!==void 0)return w.some(r(w.valFromOption(e)))}function Kn(e,r){return Le(e,M.__1(r))}function Ze(e,r){if(e!==void 0)return r(w.valFromOption(e))}function Hn(e,r){return Ze(e,M.__1(r))}function Qn(e,r){return e!==void 0?w.valFromOption(e):r}function Yn(e,r){return e!==void 0?e:r}function jn(e){return e!==void 0}function et(e){return e===void 0}function Pe(e,r,n){return e!==void 0?r!==void 0?n(w.valFromOption(e),w.valFromOption(r)):!1:r===void 0}function rt(e,r,n){return Pe(e,r,M.__2(n))}function Ve(e,r,n){return e!==void 0?r!==void 0?n(w.valFromOption(e),w.valFromOption(r)):1:r!==void 0?-1:0}function nt(e,r,n){return Ve(e,r,M.__2(n))}_.keepU=Ce;_.keep=Vn;_.forEachU=qe;_.forEach=Xn;_.getExn=Gn;_.mapWithDefaultU=ze;_.mapWithDefault=$n;_.mapU=Le;_.map=Kn;_.flatMapU=Ze;_.flatMap=Hn;_.getWithDefault=Qn;_.orElse=Yn;_.isSome=jn;_.isNone=et;_.eqU=Pe;_.eq=rt;_.cmpU=Ve;_.cmp=nt});var er=y(g=>{"use strict";var S=xe(),I=q(),tt=Xe();function ut(e){return e.length>0?e.charAt(0).toLowerCase()+e.slice(1):e}function O(e){return e.length>0?e.charAt(0).toUpperCase()+e.slice(1):e}function R(e,r){var n=new RegExp("(type\\s"+r+`+)\\s[=]
*\\s+({([a-zA-Z0-9_ :.,
*\\s<>]+)})+`),t=new RegExp("type\\s+"+r+"+\\s*=\\s*(|[^|]+)"),u=new RegExp("type\\s+"+r+`\\s*=\\s*([A-Z][a-zA-Z0-9()\\s|]+)
`),i=e.match(n);if(i!==null)return[1,I.get(S.keepMap(i,function(f){return f}),2)];var o=e.match(t);if(o===null)return[1,"// Unable to find type"];var c=e.match(u);return c!==null?[0,I.get(S.keepMap(c,function(f){return f}),1)]:[0,"// Unable to find type"]}function Ge(e){return e.startsWith("option")?"None":e.startsWith("array")?"[]":"default"+O(e)}function $e(e){switch(e){case"Js.Json.t":return"Js.Dict.empty()->Js.Json.object_";case"array<int>":case"array<string>":return"[]";case"bool":return"false";case"float":return"0.0";case"int":return"0";case"option<Js.Json.t>":case"option<array<int>>":case"option<array<string>>":case"option<bool>":case"option<float>":case"option<int>":case"option<string>":return"None";case"string":return'""';default:return Ge(e)}}function $(e){return e.replace(/array<([a-zA-Z\.<>.]+)>/g,"$1").replace(/option<([a-zA-Z\.<>.]+)>/g,"$1")}function Ke(e,r){var n=$(r),t=R(e,n),u=t[1],i=u.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g,function(c,f,v,h,D){var k=$e(v);return""+f+" : "+k});if(t[0])return"let default"+O(n)+" = "+i+`

`;var o=S.get(u.split("|"),1);return o!==void 0?"let default"+O(n)+" = "+o+`
`:""}function K(e){return e.replace(/\|\s\"[A-Z][A-Za-z0-9]+[\(]+[a-zA-Z0-9_]+[\)]+\"\s=>\s([A-Z][A-Za-z0-9]+)[\(]+([a-zA-Z0-9_]+)[\)]+/g,function(r,n,t,u,i){return'| "'+n+'" => get'+O(t)+"() // Call you custom function here"})}function He(e){return e.replace(/(\([a-zA-Z0-9_\",\s]+\))/g,"")}function H(e,r){switch(e){case"Js.Json.t":return'getJsonObjectFromDict(dict, "'+r+'")';case"array<int>":return'getIntArrayFromDict(dict, "'+r+'", [])';case"array<string>":return'getStrArrayFromDict(dict, "'+r+'", [])';case"bool":return'getBool(dict, "'+r+'", false)';case"float":return'getFloat(dict, "'+r+'", 0.0)';case"int":return'getInt(dict, "'+r+'", 0)';case"option<Js.Json.t>":return'getOptionalJsonFromDict(dict, "'+r+'")';case"option<array<int>>":return'getOptionIntArrayFromDict(dict, "'+r+'")';case"option<array<string>>":return'getOptionStrArrayFromDict(dict, "'+r+'")';case"option<bool>":return'getOptionBool(dict, "'+r+'")';case"option<float>":return'getOptionFloat(dict, "'+r+'")';case"option<int>":return'getOptionInt(dict, "'+r+'")';case"option<string>":return'getOptionString(dict, "'+r+'")';case"string":return'getString(dict, "'+r+'", "")';default:return"||get"+O(r)+'(dict, "'+r+'")**'+e+"**||"}}function Qe(e,r,n,t){var u="let "+e+` = (dict, key) => {
  dict
  ->Js.Dict.get(key)`,i="->Belt.Option.flatMap(Js.Json.decodeObject)";if(/^option<([a-zA-Z0-9]+)>/.test(r)&&t===1)return""+u+`
  `+i+`
  ->Belt.Option.map(dict => {
`+n+`
  })
}
`;if(/^option<([a-zA-Z0-9]+)>/.test(r)&&t===0)return""+u+`
  ->Belt.Option.flatMap(Js.Json.decodeString)
  ->Belt.Option.map(str => {
      
`+n+`
  })
}
`;if(/^array<([a-zA-Z0-9.]+)>/.test(r)){var o=!1;return""+u+`
   ->Belt.Option.flatMap(Js.Json.decodeArray)
   ->Belt.Option.getWithDefault([])
   ->Belt.Array.keepMap(Js.Json.decodeObject)
   ->Js.Array2.map(dict => {
       `+n+`
   })`+(o?"->Some":"")+`
}
`}else return/^option<array<([a-zA-Z0-9]+)>>/.test(r)?"let "+e+` = (dict, key) => {
    switch dict->Js.Dict.get(key)->Belt.Option.flatMap(Js.Json.decodeArray) {
  | Some(arr) =>
    arr
    `+i+`
    ->Js.Array2.map(dict => {
       `+n+`
    })
    ->Some
  | None => None
   }
}
`:t===1?""+u+`
  `+i+`
  ->Belt.Option.map(dict => {
     `+n+`
  })->Belt.Option.getWithDefault(default`+O(r)+`)
}
`:""+u+`
  ->Belt.Option.flatMap(Js.Json.decodeString)
  ->Belt.Option.map(str => {
      
`+n+`
  })->Belt.Option.getWithDefault(default`+O(r)+`)
}
`}function Ye(e,r,n){var t=$(n),u=R(e,t),i=u[1],o=u[0],c;if(o)c=i.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g,function(k,b,F,st,vt){var nr=H(F,b);return"	"+b+" : "+nr+" "});else{var f=i.split("|"),v=K(f.reduce(function(k,b){var F=b.replace(`
`,"");return k+('| "'+F.trim()+'" => '+F+`
`)},"")),h=tt.getWithDefault(S.get(f,0),"No Default Varient found (Please Provide one)");c=`  switch str {
      
`+v+`
| _ => `+h+` //Ensure that appropriate default values are provided.

    }`}var D=He(r);return Qe(D,n,c,o)}function je(e,r){for(var n={contents:e},t=e;;){var u=t,i=u.match(/\|\|(.*?)\*\*(.*?)\*\*\|\|/);if(i===null)return n.contents;var o=S.keepMap(i,function(v){return v}),c=Ke(r,I.get(o,2)),f=Ye(r,I.get(o,1),I.get(o,2));n.contents=(c+`
`+f+`
`+n.contents).replace(I.get(o,0),I.get(o,1)),t=n.contents}}function it(e,r){var n=R(r,e),t=n[1],u=n[0],i;if(u)i=t.replace(/([A-Za-z0-9_^]+)+\s*[:]\s+([a-zA-Z0-9_.<>]+)/g,function(f,v,h,D,k){var b=H(h,v);return"	"+v+" : "+b});else{var o=K(t.split("|").reduce(function(f,v){var h=v.replace(`
`,"");return f+('| "'+h.trim()+'" => '+h+`
`)},""));i=`  switch str {
      
`+o+`
    }`}var c=u?`let itemToObjectMapper = dict => {
`+i+`
}
 `:"let get"+O(e)+` = str => {
`+i+`
}
 `;return je(c,r)}g.replaceFirstLetterLower=ut;g.replaceFirstLetterUpper=O;g.getRecordType=R;g.defaultUserTypedValue=Ge;g.defaultValueMapper=$e;g.filterTypeName=$;g.getDefaultValue=Ke;g.funcWarning=K;g.getFunctionName=He;g.typeFunctionMapper=H;g.getFuntionStr=Qe;g.getObjectFunction=Ye;g.generateNestedObject=je;g.generateDecode=it});var m=require("vscode"),{languages:ot,commands:rr,workspace:ct}=m,ft=er(),Q=class{constructor(){this.codeLenses=[],this.regex=/type\s+(\w+)\s+=\s+\{/g,this._onDidChangeCodeLenses=new m.EventEmitter,this.onDidChangeCodeLenses=this._onDidChangeCodeLenses.event,m.workspace.onDidChangeConfiguration(r=>{this._onDidChangeCodeLenses.fire()})}provideCodeLenses(r,n){if(m.workspace.getConfiguration("rescript-decode").get("enableDecodeButton",!0)){this.codeLenses=[];let t=new RegExp(this.regex),u=r.getText(),i;for(;(i=t.exec(u))!==null;){let o=r.lineAt(r.positionAt(i.index).line),c=o.text.indexOf(i[0]),f=new m.Position(o.lineNumber,c),v=r.getWordRangeAtPosition(f,new RegExp(this.regex));v&&this.codeLenses.push(new m.CodeLens(v))}return this.codeLenses}return[]}resolveCodeLens(r,n){if(m.workspace.getConfiguration("rescript-decode").get("enableDecodeButton",!0)){let u=m.window.activeTextEditor.document.lineAt(r.range.start.line).text;return r.command={title:"Generate Decode",tooltip:"Click to generate decode function",command:"rescript-decode.generateDecode",arguments:[u,!1]},r}return null}};function at(e){let r=new Q;ot.registerCodeLensProvider("*",r),rr.registerCommand("rescript-decode.enableDecodeButton",()=>{ct.getConfiguration("rescript-decode").update("enableDecodeButton",!0,!0)}),rr.registerCommand("rescript-decode.generateDecode",n=>{let t=/type\s+(\w+)\s+=/,u=m.window.activeTextEditor;if(!u){m.window.showErrorMessage("No active editor found.");return}let i=n.match(t);if(i){let o=i[1],c=u.document.getText(),f=ft.generateDecode(o,c);m.env.clipboard.writeText(f).then(()=>{m.window.showInformationMessage(`${o} : Generated code copied to clipboard.`)})}else m.window.showErrorMessage("No Type Found")})}function lt(){}module.exports={activate:at,deactivate:lt};
