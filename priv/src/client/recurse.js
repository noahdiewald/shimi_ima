// # Recursion
//
// Tail call optimization taken from Spencer Tipping's Javascript in Ten
// Minutes.
//
// For more information see:
// <https://github.com/spencertipping/js-in-ten-minutes>

// ## Exported Functions

// Identity function
var identity = function (x) {
  'use strict';

  return x;
};

// Adds the prototype functions
(function () {
  'use strict';

  // Return the values to apply
  Function.prototype.r = function () {
    return [this, arguments];
  };

  // Tail call function
  Function.prototype.t = function () {
    var c = [this, arguments];
    var escape = arguments[arguments.length - 1];
    while (c[0] !== escape) {
      c = c[0].apply(this, c[1]);
    }
    return escape.apply(this, c[1]);
  };

  return true;
})();

exports.identity = identity;
