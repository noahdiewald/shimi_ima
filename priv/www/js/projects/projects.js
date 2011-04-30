function addProjectDialog() {
  var projectName = $("#project-name");
  var projectDescription = $("#project-description");
  var tips = $(".validate-tips");
  var allFields = $([]).add(projectName).add(projectDescription);
  
  var dialog = $("#add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add project": function() {
        allFields.removeClass('ui-state-error');
        
        checkResult = checkLength(projectName, "project name", 1, 50, tips);
        
        if (checkResult) {
          $.ajax({
            type: "POST", 
            url: "/projects/index",
            dataType: "json",
            contentType: "application/json",
            processData: false,
            data: JSON.stringify({name: projectName.val(), description: projectDescription.val()}),
            complete: function(req, status) {
              if (req.status == 201) {
                populateProjectsTable();
              } else {
                alert("An error occurred" + req.status);
              }
            }
          });
          $(this).dialog("close");
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      allFields.val('').removeClass('ui-state-error');
    }
  });
  
  return dialog;
}

function deleteProject(id) {
  if (confirm("Are you sure? This is permanent.")) {
    $.ajax({
      type: "DELETE", 
      url: "/projects/" + id,
      dataType: "json",
      contentType: "application/json",
      complete: function(req, status) {
        if (req.status == 204) {
          populateProjectsTable();
        } else {
          alert("An error occurred" + req.status);
        }
      }
    });
  }
}

function populateProjectsTable() {
  var url = "/projects/index";
  
  $.get(url, function(projects) {
    $('tbody').empty();
    $('tbody').html(projects);
    $('.configure-button').button({
      icons: {primary: "ui-icon-wrench"}
    });
    $('.delete-button').button({
      icons: {primary: "ui-icon-trash"}
    }).click(function(e) {
      id = $(e.target).attr("id");
      deleteProject(id);
      $('#delete-dialog').dialog("open");
    });
  });
}

$(function () {
  populateProjectsTable();
  
  $("#create-project").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    addProjectDialog().dialog("open");
  });
  
});