exports.shared = (function ()
{
  'use strict';
  var mod = {};

  mod.projectName = (function ()
  {
    var text = '';
    var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

    for (var i = 0; i < 5; i++)
    {
      text += possible.charAt(Math.floor(Math.random() * possible.length));
    }

    return 'tester_' + text;
  })();

  mod.totalProjects = function (c)
  {
    return c.evaluate(function ()
    {
      return document.getElementsByTagName('tr').length - 1;
    });
  };

  mod.getProjectIdByName = function (c, name)
  {
    return c.evaluate(function (n)
    {
      var elem = [].slice.call(document.getElementsByTagName('td')).filter(function (x)
      {
        return x.innerHTML.match(n);
      })[0];

      if (elem)
      {
        var par = elem.parentNode.querySelectorAll('.project-delete-button')[0].id;
        return par;
      }
      else
      {
        return false;
      }
    }, name);
  };

  return mod;
})();
