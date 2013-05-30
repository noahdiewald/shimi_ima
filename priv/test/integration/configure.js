// This is called from projects.js
exports.configure = function (project, casper)
{
  'use strict';
  var projectUrlPart = 'project-' + project;

  // Follow configure link from projects page
  casper.then(function ()
  {
    var title = 'Follow configure link';

    casper.echo(title);
    casper.click('a[href="/projects/' + projectUrlPart + '/config"]');
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertExists('#doctype-add-button', 'doctype add button exists');
    });
  });

  // Add document type
  casper.then(function ()
  {
    var title = 'Add document type';

    casper.echo(title);
    casper.click('#doctype-add-button');
    casper.fill('#doctype-dialog form',
    {
      'doctype-doctype-input': 'FirstDoctype',
      'doctype-description-input': 'First Doctype Description'
    }, false);
    casper.clickLabel('Save', 'span');
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertExists('[href="config/doctypes/FirstDoctype"]', title + 'doctype tab exists');
    });
  });
};
