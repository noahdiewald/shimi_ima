var casper = require('casper').create();

var projectName = (function ()
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

var getProjectIdByName = function (c, name)
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

var getDeleteButton = function (c, name)
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

// Creating a project: 
casper.then(function ()
{
  'use strict';
  var validateMessage, projectNameValue, newRow;
  var origTotal = totalProjects(casper);
  var title = 'Creating a project: ';

  casper.echo(title);
  casper.click('#create-project');
  casper.fill('form',
  {
    'project-name': projectName,
    'project-description': 'testing'
  }, false);

  validateMessage = casper.fetchText('.validate-tips');

  casper.test.assertEqual(validateMessage, 'All fields are required.', title + 'the starting validation tip is correct');
  casper.clickLabel('Add project', 'span');
  casper.wait(1000, function ()
  {
    casper.test.assertNotVisible('input#project-name', title + 'the project creation dialog is closed');
    casper.test.assertEquals(totalProjects(casper), origTotal + 1, title + 'there is one new project');
    casper.test.assert(getProjectIdByName(casper, projectName) !== false, title + 'the project is the one we created');
  });
});

// Deleting a project
casper.then(function ()
{
  'use strict';
  var origTotal = totalProjects(casper);
  var title = 'Deleting a project: ';

  casper.echo(title);
  casper.setFilter('page.confirm', function()
  {
    return true;
  });
  // Set in previous step by getRowByName
  casper.click('#' + getProjectIdByName(casper, projectName));
  casper.wait(1000, function ()
  {
    casper.test.assertEquals(totalProjects(casper), origTotal - 1, title + 'there is one less project');
    casper.test.assertNot(getProjectIdByName(casper, projectName), title + 'the correct project was deleted');
  });
});

casper.setHttpAuth('tester', 'tester');

casper.run(function ()
{
  'use strict';
  this.test.renderResults(true);
});
