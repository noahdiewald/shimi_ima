exports.deleteProject = function (total, project, casper)
{
  'use strict';

  var totalProjects = function (c)
  {
    return c.evaluate(function ()
    {
      return document.getElementsByTagName('tr').length - 1;
    });
  };

  casper.then(function ()
  {
    var title = 'Deleting a project: ';

    casper.echo(title);
    casper.click('a[href="/projects"]');
    casper.setFilter('page.confirm', function ()
    {
      return true;
    });
    // This is unreliable. I don't know why yet.
    casper.waitWhileVisible('#loading', function ()
    {
      casper.click('#' + project);
    });
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertEquals(totalProjects(casper), total - 1, title + 'there is one less project');
      //casper.test.assertNot(project, title + 'the correct project was deleted');
    });
  });
};
