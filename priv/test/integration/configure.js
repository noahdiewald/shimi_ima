// This is called from projects.js
exports.configure = function (project, casper)
{
  'use strict';
  var projectUrlPart = 'project-' + project;
  var firstFieldsetId;

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
    var title = 'Add document type: ';

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

  // Add fieldset
  casper.then(function ()
  {
    var title = 'Add fieldset: ';

    casper.echo(title);
    casper.clickLabel('FirstDoctype', 'a');
    casper.click('a#add-fieldset-to-FirstDoctype.add-fieldset-button.link-button');
    casper.fill('#fieldset-dialog form',
    {
      'fieldset-name-input': 'first_fieldset',
      'fieldset-label-input': 'First Fieldset'
    }, false);
    casper.clickLabel('Save', 'span');
    casper.clickLabel('FirstDoctype', 'a');
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertTextExists('First Fieldset', title + 'fieldset label is found on page');
      casper.test.assertExists('#fieldsets-FirstDoctype h3', title + 'fieldset h3 exists');
      firstFieldsetId = casper.evaluate(function ()
      {
        return document.querySelectorAll('#fieldsets-FirstDoctype h3')[0].id.substring(5);
      });
      casper.click('#head-' + firstFieldsetId + ' a');
      casper.waitUntilVisible('#' + firstFieldsetId, function ()
      {
        casper.test.assertExists('#' + firstFieldsetId, title + 'fieldset exists');
      });
    });
  });

  // Add field
  casper.then(function ()
  {
    var fieldId;
    var title = 'Add field: ';

    casper.echo(title);
    casper.click('#add-field-to-' + firstFieldsetId);
    casper.waitWhileVisible('#loading', function ()
    {
      casper.fill('#field-dialog form',
      {
        'field-name-input': 'first_field',
        'field-label-input': 'First Field',
        'field-subcategory-input': 'text',
        'field-head-input': true
      }, false);
      casper.clickLabel('Save', 'span');
    });
    casper.waitWhileVisible('#loading', function ()
    {
      casper.test.assertExists('.field-row', title + 'a new field row exists');
      fieldId = casper.evaluate(function ()
      {
        return document.querySelectorAll('.field-row')[0].id;
      });
      casper.test.assertExists('#' + fieldId, title + 'field exists');
    });
  });

  return casper;
};
