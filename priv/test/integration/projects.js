var casper = require('casper').create();
var deleteProject = require('delete_project').deleteProject;
var configure = require('configure').configure;
var s = require('shared').shared;
var projectName = s.projectName;
var totalProjects = s.totalProjects;
var getProjectIdByName = s.getProjectIdByName;

// Initial page load: 
casper.start('http://127.0.0.1:8000/projects', function ()
{
  'use strict';
  var title = 'Initial page load: ';

  casper.echo(title);
  casper.test.assertHttpStatus(200, title + 'login accepted');
  casper.test.assertNotVisible('input#project-name', title + 'the project creation dialog is hidden');
});

// Openning the create new project dialog: 
casper.then(function ()
{
  'use strict';
  var validateMessage, projectNameValue;
  var title = 'Openning the create new project dialog: ';

  casper.echo(title);
  casper.click('#create-project');
  projectNameValue = casper.getFormValues('form')['project-name'];
  validateMessage = casper.fetchText('.validate-tips');
  casper.test.assertEqual(validateMessage, 'All fields are required.', title + 'the validation tip is correct');
  casper.test.assertEqual(projectNameValue, '', title + 'project name input value is blank');
  casper.test.assertVisible('input#project-name', title + 'the project creation dialog is open');
});

// Filling and closing the create new project dialog: 
casper.then(function ()
{
  'use strict';
  var projectNameValue;
  var title = 'Filling and closing the create new project dialog: ';

  casper.echo(title);
  casper.fill('form',
  {
    'project-name': projectName,
    'project-description': 'testing'
  }, false);
  projectNameValue = casper.getFormValues('form')['project-name'];
  casper.test.assertEqual(projectNameValue, projectName, title + 'project name input value is tester');
  casper.clickLabel('Cancel', 'span');
  casper.test.assertNotVisible('input#project-name', title + 'the project creation dialog is closed');
});

// Validations work: 
casper.then(function ()
{
  'use strict';
  var validateMessage, projectNameValue;
  var title = 'Validations work: ';

  casper.echo(title);
  casper.click('#create-project');
  casper.fill('form',
  {
    'project-description': 'testing'
  }, false);

  projectNameValue = casper.getFormValues('form')['project-name'];
  validateMessage = casper.fetchText('.validate-tips');

  casper.test.assertEqual(validateMessage, 'All fields are required.', title + 'the starting validation tip is correct');
  casper.test.assertEqual(projectNameValue, '', title + 'project name input value is blank');
  casper.clickLabel('Add project', 'span');

  validateMessage = casper.fetchText('.validate-tips');

  casper.test.assertEqual(validateMessage, 'Length of project name must be between 1 and 50.', title + 'the resulting validation tip is correct');
  casper.clickLabel('Cancel', 'span');
});

// The validation message is cleared
casper.then(function ()
{
  'use strict';
  var validateMessage = casper.fetchText('.validate-tips');
  var title = 'The validation message is cleared: ';

  casper.echo(title);
  casper.click('#create-project');
  casper.test.assertEqual(validateMessage, 'All fields are required.', title + 'the starting validation tip is correct');
});

// Creating a project: 
casper.then(function ()
{
  'use strict';
  var projectNameValue, newRow;
  var origTotal = totalProjects(casper);
  var title = 'Creating a project: ';

  casper.echo(title);
  casper.click('#create-project');
  casper.fill('form',
  {
    'project-name': projectName,
    'project-description': 'testing'
  }, false);
  casper.clickLabel('Add project', 'span');
  casper.waitWhileVisible('#loading', function ()
  {
    var total = totalProjects(casper);
    var project = getProjectIdByName(casper, projectName);
    casper.echo(title + 'project id is ' + project);
    casper.test.assertNotVisible('input#project-name', title + 'the project creation dialog is closed');
    casper.test.assertEquals(total, origTotal + 1, title + 'there is one new project');
    casper.test.assert(project !== false, title + 'the project is the one we created');
    configure(project, casper);
    deleteProject(total, project, casper);
  });
});

casper.setHttpAuth('tester', 'tester');

casper.run();
