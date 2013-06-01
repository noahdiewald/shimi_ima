var s = require('./shared').shared;
var totalProjects = s.totalProjects;

// This is called from projects.js
exports.deleteProject = function (total, project, casper)
{
  'use strict';
  var deleteSelector = 'a#' + project + '.project-delete-button.link-button';

  casper.then(function ()
  {
    var title = 'Deleting a project: ';

    casper.echo(title);
    casper.click('a[href="/projects"]');
    casper.setFilter('page.confirm', function ()
    {
      return true;
    });
    // This is unreliable. I don't know why it times out.
    //casper.waitForSelector(deleteSelector, function ()
    casper.waitWhileVisible('#loading', function ()
    {
      casper.click(deleteSelector);
    });
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertEquals(totalProjects(casper), total - 1, title + 'there is one less project');
      //casper.test.assertNot(project, title + 'the correct project was deleted');
    });
  });

  return casper;
};
