// # Dispatcher for clicks and double clicks
//
// *Implicit depends:* DOM
//
// See [`click-dispatch.js`](./click-dispatch.html) and
// [`dblclick-dispatch.js`](./dblclick-dispatch.html).

// # Exported Functions

// Match the target to a pattern and run its action.
var dispatcher = function (patterns)
{
  'use strict';

  var d = function (e)
  {
    var target = e.target;

    Object.keys(patterns).forEach(function (pattern)
    {
      if (target.matches(pattern))
      {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

exports.dispatcher = dispatcher;
