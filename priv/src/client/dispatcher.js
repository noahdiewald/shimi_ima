// # Dispatcher for clicks and double clicks
//
// *Implicit depends:* DOM, JQuery
//
// See [`click-dispatch.js`](./click-dispatch.html) and
// [`dblclick-dispatch.js`](./dblclick-dispatch.html).

// # Exported Functions

// This is a combinator for matching JQuery selector patterns to the
// target of a click event.
var dispatcher = function (patterns)
{
  'use strict';

  var d = function (e)
  {
    var target = $(e.target);

    Object.keys(patterns).forEach(function (pattern)
    {
      if (target.is(pattern))
      {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

exports(dispatcher);