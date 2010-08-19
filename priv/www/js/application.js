$(function () {
  // Date Picker
  $(".date").datepicker();
  
  // Sortable
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      for (var x=0; x < lis.length; x++) {
        $(lis[x]).text($(lis[x]).text() + " " + x);
      }
      //alert($(lis[0]).text());
      //alert($(this).sortable('toArray').toString());
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
  
  $("#dialog-form").dialog({
    autoOpen: false,
    height: 300,
    width: 350,
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
              var resp = $.httpData(req, "json");
              if (req.status == 204) {
                $("#projects-table").append(
                  "<tr>" +
                    "<td>ok</td>" +
                    "<td>nokay</td>" +
                  "</tr>"
                );
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
  
  $("#create-project").button().click(function() {
    $("#dialog-form").dialog("open");
  });
});