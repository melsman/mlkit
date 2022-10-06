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
