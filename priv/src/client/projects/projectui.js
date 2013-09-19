// # The project manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with projects.

// Variable Definitions

var form = require('../form.js');
var init;

// Internal functions

// Delete the project with the given ID.
var deleteProject = function (id)
{
  'use strict';

  if (window.confirm('Are you sure? This is permanent.'))
  {
    $.ajax(
    {
      type: 'DELETE',
      url: '/projects/' + id,
      dataType: 'json',
      contentType: 'application/json',
      complete: function (req, status)
      {
        if (req.status === 204)
        {
          init();
        }
        else
        {
          window.alert('An error occurred' + req.status);
        }
      }
    });
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
          $.ajax(
          {
            type: 'POST',
            url: 'projects/index',
            dataType: 'json',
            contentType: 'application/json',
            processData: false,
            data: JSON.stringify(
            {
              name: projectName.val(),
              description: projectDescription.val()
            }),
            complete: function (req, status)
            {
              if (req.status === 201)
              {
                init();
              }
              else
              {
                window.alert('An error occurred ' + req.status);
              }
            }
          });
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

  var id = $(target).attr('id');
  deleteProject(id);

  return true;
};

// Initialize the interface.
init = function ()
{
  'use strict';

  var url = '/projects/index';

  $.get(url, function (projects)
  {
    $('tbody').empty();
    $('tbody').html(projects);
  });
};

exports.add = add;
exports.del = del;
exports.init = init;
