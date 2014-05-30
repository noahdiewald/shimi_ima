// # The project manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with projects.

// Variable Definitions

var ajax = require('ajax');
var templates = require('templates');
var init;

// ## Internal functions

// Show a brief validation message.
var updateTips = function (t, tips) {
  'use strict';

  tips.insertAdjacentHTML('beforeend', '<span class="validation-error-message">' + t + '</span>');
  tips.classList.add('ui-state-highlight');
  setTimeout(function () {
    tips.classList.remove('ui-state-highlight', 1500);
  }, 500);

  return tips;
};

// Client side validation of string length.
// TODO: use HTML 5 validation
var checkLength = function (o, n, min, max, tips) {
  'use strict';

  if (o.value.length > max || o.value.length < min) {
    o.classList.add('ui-state-error');
    updateTips('Length of ' + n + ' must be between ' + min + ' and ' + max + '.', tips);
    return false;
  } else {
    return true;
  }
};

// Delete the project with the given ID.
var deleteProject = function (id) {
  'use strict';

  if (window.confirm('Are you sure? This is permanent.')) {
    ajax.del('/projects/' + id, init);
  }
};

// ## Exported functions

// Add a project.
var add = function () {
  'use strict';

  var projectName = document.getElementById('project-name');
  var projectDescription = document.getElementById('project-description');
  var tips = document.getElementsByClassName('validate-tips')[0];
  var validationErrors = document.getElementsByClassName('validation-error-message')[0];
  var allFields = [projectName, projectDescription];
  var checkResult;

  var dialog = $('#add-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Add project': function () {
        Array.prototype.forEach.call(allFields, function (item) {
          item.classList.remove('ui-state-error');
        });

        if (validationErrors) {
          validationErrors.parentNode.removeChild(validationErrors);
        }

        checkResult = checkLength(projectName, 'project name', 1, 50, tips);

        if (checkResult) {
          var data = {
            name: projectName.value,
            description: projectDescription.value
          };

          ajax.post('projects/index', data, init);

          $(this).dialog('close');
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      Array.prototype.forEach.call(allFields, function (item) {
        item.value = '';
        item.classList.remove('ui-state-error');
      });
    }
  });

  return dialog;
};

// Add a project.
var del = function (target) {
  'use strict';

  var id = target.getAttribute('id');

  deleteProject(id);

  return true;
};

// Initialize the interface.
init = function () {
  'use strict';

  var url = '/projects/index';

  ajax.get(url, function (req) {
    var rendering = templates['project-listing'](req.response);

    document.getElementsByTagName('tbody')[0].innerHTML = rendering;
  });
};

exports.add = add;
exports.del = del;
exports.init = init;
