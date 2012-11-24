// Object.keys compatibility from MDC

if(!Object.keys) {
  Object.keys = function(o){
    if (o !== Object(o)) {
      throw new TypeError('Object.keys called on non-object');
    }
    var ret=[],p;
    for(p in o) {
      if (Object.prototype.hasOwnProperty.call(o,p)) {
        ret.push(p);
      }
    }
    return ret;
  };
}

// Reduce compatibility from MDC

if (!Array.prototype.reduce)
{
  Array.prototype.reduce = function(fun /*, initialValue */)
  {
    "use strict";

    if (this === void 0 || this === null) {
      throw new TypeError();
    }

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function") {
      throw new TypeError();
    }

    // no value to return if no initial value and an empty array
    if (len === 0 && arguments.length === 1) {
      throw new TypeError();
    }

    var k = 0;
    var accumulator;
    if (arguments.length >= 2)
    {
      accumulator = arguments[1];
    }
    else
    {
      do
      {
        if (k in t)
        {
          accumulator = t[k++];
          break;
        }

        // if array contains no values, no initial value to return
        if (++k >= len) {
          throw new TypeError();
        }
      }
      while (true);
    }

    while (k < len)
    {
      if (k in t) {
        accumulator = fun.call(undefined, accumulator, t[k], k, t);
      }
      k++;
    }

    return accumulator;
  };
}
