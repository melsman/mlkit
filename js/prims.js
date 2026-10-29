CompilerInitial = {}

CompilerInitial.en$Bind$47 = new String("Bind");
CompilerInitial.exn$Bind$47 = Array(CompilerInitial.en$Bind$47);
CompilerInitial.en$Match$46 = new String("Match");
CompilerInitial.exn$Match$46 = Array(CompilerInitial.en$Match$46);
CompilerInitial.en$Div$45 = new String("Div");
CompilerInitial.exn$Div$45 = Array(CompilerInitial.en$Div$45);
CompilerInitial.en$Interrupt = new String("Interrupt");
CompilerInitial.exn$Interrupt = Array(CompilerInitial.en$Interrupt);
CompilerInitial.en$Overflow$48 = new String("Overflow");
CompilerInitial.exn$Overflow$48 = Array(CompilerInitial.en$Overflow$48);
CompilerInitial.en$Subscript$50 = new String("Subscript");
CompilerInitial.exn$Subscript$50 = Array(CompilerInitial.en$Subscript$50);
CompilerInitial.en$Size$51 = new String("Size");
CompilerInitial.exn$Size$51 = Array(CompilerInitial.en$Size$51);

// Stable fields used by new emitters; retain historical runtime aliases.
['Bind','Match','Div','Overflow','Subscript','Size','Interrupt'].forEach(function(name,i) {
  var old = ['Bind$47','Match$46','Div$45','Overflow$48','Subscript$50','Size$51','Interrupt'][i];
  CompilerInitial['en$'+name] = CompilerInitial['en$'+old];
  CompilerInitial['exn$'+name] = CompilerInitial['exn$'+old];
});

Con = {}
Con.some = 0;
Con.none = 1;
Con.intinf = 0;

SMLtoJs = {}

SmlPrims = {}

SmlPrims.option = function(e) {
  if ( e ) {
    return [Con.some,e];
  } else {
    return [Con.none];
  }
}

SmlPrims.explode = function(s) {
  var i;
  var res = null;
  for ( i = s.length ; i > 0 ; i-- ) {
    res = [s.charCodeAt(i-1),res];
  }
  return res;
}

SmlPrims.implode = function(xs) {
  var i;
  var a = [];
  for ( i = 0 ; xs != null ; xs = xs[1], i++ ) {
    a[i] = String.fromCharCode(xs[0]);
  }
  return a.join("");
}

SmlPrims.charsToCharArray = function(xs) {
  var i;
  var a = [];
  for ( i = 0 ; xs != null ; xs = xs[1], i++ ) {
    a[i] = xs[0];
  }
  return a;
}

SmlPrims.listToArray = function(xs) {
  var i;
  var a = [];
  for ( i = 0 ; xs != null ; xs = xs[1], i++ ) {
    a[i] = xs[0];
  }
  return a;
}

SmlPrims.charArraysConcat = function(xs) {
  var i;
  var a = [];
  for ( i = 0 ; xs != null ; xs = xs[1], i++ ) {
    a = Array.concat(a, xs[0]);
  }
  return a;
}

SmlPrims.concat = function(xs) {
  var i;
  var a = [];
  for ( i = 0 ; xs != null ; xs = xs[1], i++ ) {
    a[i] = xs[0];
  }
  return a.join("");
}

SmlPrims.length = function len(a) {
    if (a == null) {
	return 0;
    } else {
	return(1 + len(a[1]));
    }
}

SmlPrims.arrayMap = function(f) {
    return function(a) {
        var i;
        var a2 = new Array(a.length);
        for (i = 0; i < a.length; i++ ) {
          a2[i] = f(a[i]);
        };
        return a2;
    };
}

SmlPrims.charArrayToString = function(a) {
 var a2 = SmlPrims.arrayMap(String.fromCharCode)(a);
 return a2.join("");
}

SmlPrims.real_to_bytes = function(r) {
    var buf = new ArrayBuffer(8);
    var arr1 = new Float64Array(buf,0,1);
    arr1[0] = r;
    var arr2 = new Uint8Array(buf,0,8);
    var arr3 = SmlPrims.arrayMap(String.fromCharCode)(arr2);
    return arr3.join("");
}

SmlPrims.bytes_to_real = function(s) {
    var buf = new ArrayBuffer(8);
    var arr1 = new Uint8Array(buf,0,8);
    var i;
    for (i=0 ; i<8 && i<s.length; i++) {
	arr1[i] = s.charCodeAt(i);
    }
    var arr2 = new Float64Array(buf,0,1);
    return arr2[0];
}

SmlPrims.wordTableInit = function(n,x) {
  var i;
  var a = new Array(n);
  for ( i = 0 ; i < n ; i++) {
    a[i] = x;
  };
  return a;
}

SmlPrims.chk_ovf_i32 = function (i) {
  if ( i < -2147483648 || i > 2147483647 ) {
    throw(CompilerInitial.exn$Overflow$48);
  }
  return i;
}

SmlPrims.chk_ovf_i31 = function (i) {
  if ( i < -1073741824 || i > 1073741823 ) {
    throw(CompilerInitial.exn$Overflow$48);
  }
  return i;
}

SmlPrims.cut_w32 = function (w) {
  return w & 0xFFFFFFFF;
}

SmlPrims.cut_w31 = function (w) {
  return w & 0x7FFFFFFF;
}

SmlPrims.mod_i32 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) ) {
    return x % y;
  }
  return (x % y) + y;
}

SmlPrims.div_i32 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  if ( y == -1 && x == -2147483648 ) { throw(CompilerInitial.exn$Overflow$48); }
  return Math.floor(x / y);
}

SmlPrims.mod_i31 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) ) {
    return x % y;
  }
  return (x % y) + y;
}

SmlPrims.div_i31 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  if ( y == -1 && x == -1073741824 ) { throw(CompilerInitial.exn$Overflow$48); }
  return Math.floor(x / y);
}

SmlPrims.div_w31 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  return Math.floor(x / y);
}

SmlPrims.div_w32 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  return Math.floor(x / y);
}

SmlPrims.mod_w31 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  return x % y;
}

SmlPrims.mod_w32 = function (x,y,exn) {
  if ( y == 0 ) { throw(exn); }
  return x % y;
}

SmlPrims.quot = function (x,y) {
  if ((x < 0 && y >= 0) || (x >= 0 && y < 0)) {
     return Math.ceil(x / y);
  } else {
     return Math.floor(x / y);
  }
}

SmlPrims.w32_to_i32_X = function(x) {
  if ( x > 0x7FFFFFFF ) {
    return -(0xFFFFFFFF - x) - 1;
  } else {
    return x;
  }
}

SmlPrims.w31_to_i32_X = function(x) {
  if ( x > 0x3FFFFFFF ) {
    return -(0x7FFFFFFF - x) - 1;
  } else {
    return x;
  }
}

SmlPrims.w31_to_w32_X = function(x) {
  if ( x > 0x3FFFFFFF ) {
    return (x & 0x3FFFFFFF) | (1 << 31);
  } else {
    return x;
  }
}

SmlPrims.i32_to_w32 = function(x) {
  if ( x < 0 ) {
    return 0xFFFFFFFF - (-x) + 1;
  } else {
    return x;
  }
}

SmlPrims.i32_to_w31 = function(x) {
  if ( x < 0 ) {
    return SmlPrims.cut_w31(0xFFFFFFFF - (-x) + 1);
  } else {
    return SmlPrims.cut_w31(x);
  }
}

SmlPrims.sinh = function(x) {
  var tmp = Math.exp(x);
  return (tmp - 1 / tmp) / 2;
}

SmlPrims.cosh = function(x) {
  var tmp = Math.exp(x);
  return (tmp + 1 / tmp) / 2;
}

SmlPrims.tanh = function(x) {
  var tmp = Math.exp(x);
  return (tmp - 1 / tmp) / (tmp + 1 / tmp);
}

SmlPrims.trunc = function(x) {
  if ( x >= 0 ) { return Math.floor(x); }
  return Math.ceil(x);
}

SmlPrims.getrealtime = function() {
  var timebase = -2147483648;
  var d = new Date();
  var t = d.getTime();
  var s = Math.floor(t/1000);
  var u = (t % 1000) * 1000;
  return [SmlPrims.chk_ovf_i32(s+timebase),SmlPrims.chk_ovf_i32(u)];
}

SmlPrims.monthDays = function(Y,m) {
  switch(m) {
    case 0 :
    case 2 :
    case 4 :
    case 6 :
    case 7 :
    case 9 :
    case 11 : return 31;
    case 1 : { if ( Y%4 == 0 && ( (Y%100 != 0) || ( Y%400 == 0 ) ) ) {
                 return 29;
               } else {
                 return 28;
               }
             }
    default : return 30;
  }
}

SmlPrims.yearDays = function(Y,m,D) {
  var d = D - 1;
  var i;
  for ( i = 0 ; i < m ; i ++ ) {
    d += SmlPrims.monthDays(Y,i);
  }
  return d;
}

SmlPrims.localtime = function(t) {
  var d = new Date(t*1000);
  var H = d.getHours();
  var M = d.getMinutes();
  var Y = d.getFullYear();
  var D = d.getDate();
  var wd = d.getDay();
  var m = d.getMonth();
  var yd = SmlPrims.yearDays(Y,m,D);
  var S = d.getSeconds();
  var dst = -1;
  return [H,dst,D,M,m,S,wd,yd,Y];
}

SmlPrims.gmtime = function(t) {
  var d = new Date(t*1000);
  var H = d.getUTCHours();
  var M = d.getUTCMinutes();
  var Y = d.getUTCFullYear();
  var D = d.getUTCDate();
  var wd = d.getUTCDay();
  var m = d.getUTCMonth();
  var yd = SmlPrims.yearDays(Y,m,D);
  var S = d.getUTCSeconds();
  var dst = -1;
  return [H,dst,D,M,m,S,wd,yd,Y];
}

SmlPrims.mktime = function(r) {
  var H = r[0];
  var D = r[2];
  var M = r[3];
  var m = r[4];
  var S = r[5];
  var Y = r[8];
  var d = new Date(Y,m,D,H,M,S,0);
  var t = d.getTime();
  return Math.floor(t / 1000);
}

// return localoffset in seconds
SmlPrims.localoffset = function() {
  var d = new Date();
  var m = d.getTimezoneOffset();
  return 60 * m;
}

// The following code is from Chapter 20 in David Flanagan. JavaScript, The
// Definitive Guide. Fifth Edition. 2006. O'Reilly.
SmlPrims._factories =
    [function() { return new XMLHttpRequest(); },
     function() { return new ActiveXObject("Msxml2.XMLHTTP"); },
     function() { return new ActiveXObject("Microsoft.XMLHTTP"); }
     ];

SmlPrims._factory = null; // When we find a factory that works, store it here

SmlPrims.newRequest = function() {
    if (SmlPrims._factory != null) return SmlPrims._factory();

    for(var i = 0; i < SmlPrims._factories.length; i++) {
	try {
	    var factory = SmlPrims._factories[i];
	    var request = factory();
	    if (request != null) {
		SmlPrims._factory = factory;
		return request;
	    }
	}
	catch(e) {
	    continue;
	}
    }
    SmlPrims._factory = function() {
	throw new Error("SmlPrims.newRequest not supported by browser");
    }
    SmlPrims._factory();
}

SmlPrims.arraybufferToString = function (ab) {
    if (ab == null) { return null; }
    var arr = new Uint8Array(ab);
    var ss = [];            // break up into blocks
    var blocksz = 0xffff;   // to avoid stack overflow
    for (var i=0; i*blocksz < arr.length; i++) {
        ss.push(String.fromCharCode.apply(null, arr.subarray(i*blocksz,(i+1)*blocksz)));
    }
    return ss.join('');
}

SmlPrims.stringToArrayBuffer = function (s) {
    var sz = s.length;
    var ab = new ArrayBuffer(s.length);
    var barr = new Uint8Array(ab);
    for (var i=0; i < barr.length; i++) {
	barr[i] = s.charCodeAt(i);
    }
    return ab;
}
;

/* Narrow scalars are unsigned IEEE bits, not JavaScript floating values.
 * BigInt is looked up lazily: legacy programs need not use narrow arithmetic.
 * All core operations quantize exact rationals once, including subnormals. */
SmlPrims.narrow = (function () {
  var mode = 0;
  var formats = {};
  function B(n) {
    if (typeof BigInt !== 'function') throw new Error('Narrow real arithmetic requires BigInt');
    return BigInt(n);
  }
  function format(bits) {
    if (!formats[bits]) {
      var frac = bits === 16 ? 10 : 23, eb = bits === 16 ? 5 : 8;
      formats[bits] = {frac:frac, bias:Math.pow(2,eb-1)-1,
        hidden:Math.pow(2,frac), inf:(Math.pow(2,eb)-1)*Math.pow(2,frac),
        sign:Math.pow(2,bits-1), quiet:Math.pow(2,frac-1)};
    }
    return formats[bits];
  }
  function magnitude(h,f) { return h % f.sign; }
  function nan(h,f) { return magnitude(h,f) > f.inf; }
  function inf(h,f) { return magnitude(h,f) === f.inf; }
  function neg(h,f) { return h >= f.sign; }
  function signed(h,sign,f) { return h + (sign ? f.sign : 0); }
  function quiet(h,f) { return (h | f.quiet) >>> 0; }
  function invalid(f) { return f.inf+f.quiet; }
  function decode(h,f) {
    var a = magnitude(h,f), e = Math.floor(a/f.hidden), m = a%f.hidden;
    return {n:B(e ? m+f.hidden : m), e:(e || 1)-f.bias-f.frac};
  }
  function width(n) { return n.toString(2).length; }
  function away(sign) { return mode === 1 && sign || mode === 2 && !sign; }
  function overflow(sign,f) { return signed(mode === 0 || away(sign) ? f.inf : f.inf-1,sign,f); }
  function quantize(n,d,e,sign,f) {
    if (n === B(0)) return signed(0,sign,f);
    var k = width(n)-width(d), exponent = k+e;
    if (k >= 0 ? n < (d << B(k)) : (n << B(-k)) < d) --exponent;
    var min = 1-f.bias;
    if (exponent > f.bias) return overflow(sign,f);
    if (exponent < min-f.frac-1) return signed(away(sign) ? 1 : 0,sign,f);
    var unit = Math.max(exponent,min)-f.frac, shift = e-unit;
    if (shift >= 0) n <<= B(shift); else d <<= B(-shift);
    var q = n/d, r = n%d;
    if (r !== B(0) && (away(sign) || mode === 0 &&
        (r > d-r || r === d-r && (q%B(2)) !== B(0)))) ++q;
    var bits = Math.max(0,exponent-min)*f.hidden+Number(q);
    return bits >= f.inf ? overflow(sign,f) : signed(bits,sign,f);
  }
  function binary(op,a,b,bits) {
    var f = format(bits), sa = neg(a,f), sb = neg(b,f), sign = sa !== sb;
    if (nan(a,f)) return quiet(a,f);
    if (nan(b,f)) return quiet(b,f);
    if (op === 'sub') { b = signed(magnitude(b,f),!sb,f); sb = !sb; op = 'add'; }
    if (op === 'add') {
      if (inf(a,f)) return inf(b,f) && sa !== sb ? invalid(f) : a;
      if (inf(b,f)) return b;
      var x = decode(a,f), y = decode(b,f), e = Math.min(x.e,y.e);
      var n = (sa ? -x.n : x.n) << B(x.e-e);
      n += (sb ? -y.n : y.n) << B(y.e-e);
      sign = n < B(0) || n === B(0) && (sa && sb || sa !== sb && mode === 1);
      return quantize(n < B(0) ? -n : n,B(1),e,sign,f);
    }
    if (op === 'mul') {
      if (inf(a,f) || inf(b,f)) return !magnitude(a,f) || !magnitude(b,f) ? invalid(f) : signed(f.inf,sign,f);
      var x = decode(a,f), y = decode(b,f);
      return quantize(x.n*y.n,B(1),x.e+y.e,sign,f);
    }
    if (op === 'div') {
      if (inf(a,f)) return inf(b,f) ? invalid(f) : signed(f.inf,sign,f);
      if (inf(b,f)) return signed(0,sign,f);
      if (!magnitude(b,f)) return !magnitude(a,f) ? invalid(f) : signed(f.inf,sign,f);
      var x = decode(a,f), y = decode(b,f);
      return quantize(x.n,y.n,x.e-y.e,sign,f);
    }
    if (inf(a,f) || !magnitude(b,f)) return invalid(f);
    if (inf(b,f)) return a;
    var x = decode(a,f), y = decode(b,f), e = Math.min(x.e,y.e);
    return quantize((x.n << B(x.e-e)) % (y.n << B(y.e-e)),B(1),e,sa,f);
  }
  function sqrt(a,bits) {
    var f = format(bits);
    if (nan(a,f)) return quiet(a,f);
    if (!magnitude(a,f)) return a;
    if (neg(a,f)) return invalid(f);
    if (inf(a,f)) return a;
    var x = decode(a,f), e = Math.floor((width(x.n)-1+x.e)/2);
    var unit = Math.max(e,1-f.bias)-f.frac, shift = x.e-2*unit;
    var n = x.n, d = B(1);
    if (shift >= 0) n <<= B(shift); else d <<= B(-shift);
    var lo = B(0), hi = B(2*f.hidden);
    while (lo+B(1) < hi) {
      var mid = (lo+hi)/B(2);
      if (mid*mid*d <= n) lo = mid; else hi = mid;
    }
    if (lo*lo*d !== n) {
      var middle = (B(2)*lo+B(1))*(B(2)*lo+B(1))*d;
      if (mode === 2 || mode === 0 && (B(4)*n > middle || B(4)*n === middle && lo%B(2) !== B(0))) ++lo;
    }
    return Math.max(0,e-(1-f.bias))*f.hidden+Number(lo);
  }
  function view() {
    if (typeof DataView !== 'function') throw new Error('Narrow reals require DataView');
    return new DataView(new ArrayBuffer(8));
  }
  function fromDouble(x,bits) {
    var f = format(bits), v = view(); v.setFloat64(0,x,false);
    var hi = v.getUint32(0,false), low = v.getUint32(4,false);
    var sign = hi >= 2147483648, e = (hi >>> 20)&2047;
    var m = B(hi&1048575)*B(4294967296)+B(low);
    if (e === 2047) return signed(m === B(0) ? f.inf : f.inf+f.quiet+Number(m >> B(52-f.frac))%f.quiet,sign,f);
    return quantize(e ? m+(B(1)<<B(52)) : m,B(1),e ? e-1075 : -1074,sign,f);
  }
  function toDouble(h,bits) {
    var f = format(bits), a = magnitude(h,f);
    if (a >= f.inf) return a === f.inf ? (neg(h,f) ? -Infinity : Infinity) : NaN;
    var e = Math.floor(a/f.hidden), m = a%f.hidden;
    var x = (e ? f.hidden+m : m)*Math.pow(2,(e || 1)-f.bias-f.frac);
    return neg(h,f) ? -x : x;
  }
  function compare(a,b,bits) {
    var f = format(bits);
    if (nan(a,f) || nan(b,f)) return 2;
    if (a === b || !magnitude(a,f) && !magnitude(b,f)) return 0;
    if (neg(a,f) !== neg(b,f)) return neg(a,f) ? -1 : 1;
    return (a < b) !== neg(a,f) ? -1 : 1;
  }
  function ldexp(a,e,bits) {
    var f = format(bits);
    if (nan(a,f)) return quiet(a,f);
    if (inf(a,f)) return a;
    var x = decode(a,f);
    return quantize(x.n,B(1),x.e+Math.max(-10000,Math.min(10000,e)),neg(a,f),f);
  }
  function next(a,b,bits) {
    var f = format(bits);
    if (nan(a,f)) return quiet(a,f);
    if (nan(b,f)) return quiet(b,f);
    if (inf(a,f) || compare(a,b,bits) === 0) return a;
    if (!magnitude(a,f)) return signed(1,neg(b,f),f);
    return a+((compare(a,b,bits) < 0) !== neg(a,f) ? 1 : -1);
  }
  return {binary:binary,sqrt:sqrt,fromDouble:fromDouble,toDouble:toDouble,
    compare:compare,ldexp:ldexp,next:next,
    setMode:function(m){mode=m;return 0;}, getMode:function(){return mode;}};
})();

(function () {
  var n = SmlPrims.narrow;
  [16,32].forEach(function (bits) {
    var prefix = 'real'+bits+'_', sign = Math.pow(2,bits-1);
    ['add','sub','mul','div','rem'].forEach(function(op) {
      SmlPrims[prefix+op] = function(a,b) { return n.binary(op,a,b,bits); };
    });
    SmlPrims[prefix+'neg'] = function(a) { return (a ^ sign) >>> 0; };
    SmlPrims[prefix+'abs'] = function(a) { return (a & (sign-1)) >>> 0; };
    SmlPrims[prefix+'sqrt'] = function(a) { return n.sqrt(a,bits); };
    SmlPrims[prefix+'from'] = function(a) { return n.fromDouble(a,bits); };
    SmlPrims[prefix+'to'] = function(a) { return n.toDouble(a,bits); };
    SmlPrims[prefix+'ldexp'] = function(a,e) { return n.ldexp(a,e,bits); };
    SmlPrims[prefix+'next'] = function(a,b) { return n.next(a,b,bits); };
    ['less','lesseq','greater','greatereq','equal','unequal'].forEach(function(op) {
      SmlPrims[prefix+op] = function(a,b) {
        var c = n.compare(a,b,bits);
        return op === 'less' ? c === -1 : op === 'lesseq' ? c === -1 || c === 0 :
          op === 'greater' ? c === 1 : op === 'greatereq' ? c === 1 || c === 0 :
          op === 'equal' ? c === 0 : c !== 0;
      };
    });
    ['sin','cos','tan','asin','acos','atan','atan2','exp','log','log10','sinh','cosh','tanh','pow','floor','ceil','trunc'].forEach(function(op) {
      SmlPrims[prefix+op] = function(a,b) {
        var fn = Math[op] || SmlPrims[op];
        return n.fromDouble(b === undefined ? fn(n.toDouble(a,bits)) :
          fn(n.toDouble(a,bits),n.toDouble(b,bits)),bits);
      };
    });
  });
})();
SmlPrims.narrowAlloc = function(n) { return new Uint8Array(n); };
SmlPrims.narrowSub16 = function(t,i) { return t[2*i]+256*t[2*i+1]; };
SmlPrims.narrowSub32 = function(t,i) { return (t[4*i]+256*t[4*i+1]+65536*t[4*i+2]+16777216*t[4*i+3]); };
SmlPrims.narrowUpdate16 = function(t,i,x) { t[2*i]=x&255; t[2*i+1]=(x>>>8)&255; return 0; };
SmlPrims.narrowUpdate32 = function(t,i,x) {
  for (var j=0;j<4;++j) t[4*i+j]=(x>>>(8*j))&255;
  return 0;
};
SmlPrims.narrowWideRound = function(x) {
  if (!isFinite(x) || x === 0) return x;
  var a=Math.abs(x), lo=Math.floor(a), r=a-lo;
  if (r > 0.5 || r === 0.5 && lo%2) ++lo;
  return x < 0 ? -lo : lo;
};
SmlPrims.narrowWideFrexp = function(x) {
  if (!isFinite(x) || x === 0) return [x,0];
  var e=Math.floor(Math.log(Math.abs(x))/Math.LN2)+1;
  var man=x*Math.pow(2,-e);
  if (Math.abs(man)<0.5) {man*=2;--e;}
  if (Math.abs(man)>=1) {man/=2;++e;}
  return [man,e];
};
SmlPrims.narrowIntegerString = function(x) { return BigInt(x).toString().replace('-','~'); };
