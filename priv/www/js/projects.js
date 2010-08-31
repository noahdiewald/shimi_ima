$(function () {
  var projectName = $("#project-name"),
    projectDescription = $("#project-description"),
    tips = $(".validate-tips"),
    allFields = $([]).add(projectName).add(projectDescription);
  
  function populateProjectsTable() {
    $.getJSON("project", function(data) {
      $("#projects-table > tbody").empty();
      data.renderings.forEach(function(rendering) {
        $("#projects-table").append(rendering);
        $(".delete-button").button().click(function() {
          toDelete = $(this).attr("id");
          $("#delete-dialog").dialog("open");
        });
      });
    });
  }
  
  populateProjectsTable();
  
  $("#add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add project": function() {
        allFields.removeClass('ui-state-error');
        
        checkResult = checkLength(projectName, "project name", 1, 50, tips);
        
        if (checkResult) {
          $.ajax({
            type: "POST", 
            url: "project",
            dataType: "json",
            contentType: "application/json",
            processData: false,
            data: JSON.stringify({name: projectName.val(), description: projectDescription.val()}),
            complete: function(req, status) {
              if (req.status == 204) {
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
      },
    },
    close: function() {
      allFields.val('').removeClass('ui-state-error');
    }
  });
  
  $("#delete-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Delete project": function() {
        $.ajax({
          type: "DELETE", 
          url: "project/" + toDelete,
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
        $(this).dialog("close");
      },
      Cancel: function() {
        $(this).dialog("close");
      },
    },
  });
  
  $("#create-project").button().click(function() {
    $("#add-dialog").dialog("open");
  });
  
});