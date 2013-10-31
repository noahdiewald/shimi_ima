// # The project manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with projects.

// Variable Definitions

var form = require('../form.js');
var ajax = require('../ajax.js');
var templates = require('templates.js');
var init;

// Internal functions

// Delete the project with the given ID.
var deleteProject = function (id)
{
  'use strict';

  if (window.confirm('Are you sure? This is permanent.'))
  {
    ajax.del('/projects/' + id, init);
  }
};

// Exported functions

// Add a project.
var add = function ()
{
  'use strict';

  var projectName = $('#project-name');
  var projectDescription = $('#project-description');
  var tips = $('.validate-tips');
  var allFields = $([]).add(projectName).add(projectDescription);

  var dialog = $('#add-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Add project': function ()
      {
        allFields.removeClass('ui-state-error');
        $('.validation-error-message').remove();

        var checkResult = form.checkLength(projectName, 'project name', 1, 50, tips);

        if (checkResult)
        {
          var data = {
            name: projectName.val(),
            description: projectDescription.val()
          };

          ajax.post('projects/index', data, init);

          $(this).dialog('close');
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      allFields.val('').removeClass('ui-state-error');
    }
  });

  return dialog;
};

// Add a project.
var del = function (target)
{
  'use strict';

  var id = target.getAttribute('id');

  deleteProject(id);

  return true;
};

// Initialize the interface.
init = function ()
{
  'use strict';

  var url = '/projects/index';

  ajax.get(url, function (req)
  {
    var rendering = templates['project-listing'](req.response);

    document.getElementsByTagName('tbody')[0].innerHTML = rendering;
  });
};

exports.add = add;
exports.del = del;
exports.init = init;
