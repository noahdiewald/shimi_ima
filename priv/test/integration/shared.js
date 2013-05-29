exports.projectName = (function ()
{
  'use strict';
  var text = '';
  var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  for (var i = 0; i < 5; i++)
  {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }

  return 'tester_' + text;
})();

var totalProjects = function (c)
{
  'use strict';
  return c.evaluate(function ()
  {
    return document.getElementsByTagName('tr').length - 1;
  });
};

var getProjectRow = function (c, name)
{
  'use strict';
  return c.evaluate(function (n)
  {
    var elem = [].slice.call(document.getElementsByTagName('td')).filter(function (x)
    {
      return x.innerHTML.match(n);
    })[0];

    if (elem)
    {
      var par = elem.parentNode;
      return par;
    } else {
      return false;
    }
  }, name);
};

exports.getProjectIdByName = function (c, name)
{
  'use strict';
  return c.evaluate(function (n)
  {
    var elem = [].slice.call(document.getElementsByTagName('td')).filter(function (x)
    {
      return x.innerHTML.match(n);
    })[0];

    if (elem)
    {
      var par = elem.parentNode;
      return par.querySelectorAll('.project-delete-button')[0].id;
    } else {
      return false;
    }
  }, name);
};

exports.getDeleteButton = function (c, name)
{
  'use strict';
  return c.evaluate(function (n)
  {
    var elem = [].slice.call(document.getElementsByTagName('td')).filter(function (x)
    {
      return x.innerHTML.match(n);
    })[0];

    if (elem)
    {
      return elem.parentNode.querySelectorAll('.project-delete-button')[0];
    } else {
      return elem;
    }
  }, name);
};
