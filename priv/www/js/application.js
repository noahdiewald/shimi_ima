$(function () {
  // Date Picker
  $(".date").datepicker();
  
  // Sortable
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      lis.forEach(function(li) {$(li).text($(li).text() + " " + x);});
    }
  });
  
  // Project Dialogs
  var projectName = $("#project-name"),
    projectDescription = $("#project-description"),
    tips = $(".validate-tips"),
    allFields = $([]).add(projectName).add(projectDescription);
    
  function updateTips(t) {
    tips.text(t).addClass('ui-state-highlight');
    setTimeout(function() {
      tips.removeClass('ui-state-highlight', 1500);
    }, 500);
  }
  
  function checkLength(o,n,min,max) {
    if ( o.val().length > max || o.val().length < min ) {
      o.addClass('ui-state-error');
      updateTips("Length of " + n + " must be between " + min + " and " + max + ".");
      return false;
    } else {
      return true;
    }
  }
  
  function checkRegexp(o,regexp,n) {
    if ( !( regexp.test( o.val() ) ) ) {
      o.addClass('ui-state-error');
      updateTips(n);
      return false;
    } else {
      return true;
    }
  }
  
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
        var bValid = true;
        allFields.removeClass('ui-state-error');
        
        bValid = bValid && checkLength(projectName, "project name", 1, 50);
        
        if (bValid) {
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
      Cancel: function() {
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