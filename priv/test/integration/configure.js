exports.configure = function (project, casper)
{
  'use strict';

  var projectUrlPart = 'project-' + project;

  casper.then(function ()
  {
    var title = 'Follow configure link';

    casper.echo(title);
    casper.click('a[href="/projects/' + projectUrlPart + '/config"]');
  });
};
