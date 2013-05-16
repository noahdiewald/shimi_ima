shimi.projectui = (function () {
  'use strict';

  var mod = {};

  var deleteProject = function (id) {
    if (window.confirm('Are you sure? This is permanent.')) {
      $.ajax({
        type: 'DELETE',
        url: '/projects/' + id,
        dataType: 'json',
        contentType: 'application/json',
        complete: function (req, status) {
          if (req.status === 204) {
            mod.init();
          } else {
            window.alert('An error occurred' + req.status);
          }
        }
      });
    }
  };

  mod.add = function () {
    var projectName = $('#project-name');
    var projectDescription = $('#project-description');
    var tips = $('.validate-tips');
    var allFields = $([]).add(projectName).add(projectDescription);

    var dialog = $('#add-dialog').dialog({
      autoOpen: false,
      modal: true,
      buttons: {
        'Add project': function () {
          allFields.removeClass('ui-state-error');

          var checkResult = shimi.form.checkLength(projectName, 'project name', 1, 50, tips);

          if (checkResult) {
            $.ajax({
              type: 'POST',
              url: 'projects/index',
              dataType: 'json',
              contentType: 'application/json',
              processData: false,
              data: JSON.stringify({
                name: projectName.val(),
                description: projectDescription.val()
              }),
              complete: function (req, status) {
                if (req.status === 201) {
                  mod.init();
                } else {
                  window.alert('An error occurred' + req.status);
                }
              }
            });
            $(this).dialog('close');
          }
        },
        'Cancel': function () {
          $(this).dialog('close');
        }
      },
      close: function () {
        allFields.val('').removeClass('ui-state-error');
      }
    });

    return dialog;
  };

  mod.del = function (target) {
    var id = $(target).attr('id');
    deleteProject(id);

    return mod;
  };

  mod.init = function () {
    var url = '/projects/index';

    $.get(url, function (projects) {
      $('tbody').empty();
      $('tbody').html(projects);
    });
  };

  return mod;
})();