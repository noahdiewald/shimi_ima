$(function () {
  var projectName = $("#project-name"),
    projectDescription = $("#project-description"),
    tips = $(".validate-tips"),
    allFields = $([]).add(projectName).add(projectDescription);
  
  function populateProjectsTable() {
    // I use a bogus query string to ensure that the browser
    // won't replace the html version with the json verson
    // from this resource. There is probably a better way.
    $.getJSON("projects?this=null", function(data) {
      $("#projects-table > tbody").empty();
      data.renderings.forEach(function(rendering) {
        $("#projects-table").append(rendering);
        $(".configure-button").button({
          icons: {primary: "ui-icon-wrench"}
        });
        $(".delete-button").button({
          icons: {primary: "ui-icon-trash"}
        }).click(function() {
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
            url: "projects",
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
          url: "projects/" + toDelete,
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
  
  $("#create-project").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#add-dialog").dialog("open");
  });
  
});