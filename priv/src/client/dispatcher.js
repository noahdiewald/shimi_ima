shimi.dispatcher = function (patterns)
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
