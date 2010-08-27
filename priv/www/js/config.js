$(function () {
  function postConfigDoc(obj, onComplete) {
    $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: onComplete(req, status)
    });
  }
  
  $("#main-tabs").tabs();
  $("#document-type-tabs").tabs();
  $("#character-sequence-tabs").tabs();
  $("#document-type-info").hide();
  $("#character-sequence-info").hide();
  
  $("#document-type-info-toggle").click(function() {
    $("#document-type-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#character-sequence-info-toggle").click(function() {
    $("#character-sequence-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#doc-type-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add": function() {
        
      },
      "Cancel": function() {
        $(this).dialog("close");
      },
    },
    close: function() {
      allFields.val('').removeClass('ui-state-error');
    }
  });
  
  $("#doc-type-add-button").button().click(function() {
    $("#doc-type-add-dialog").dialog("open");
  });
  
  $("#character-sequence-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add": function() {},
      "Cancel": function() {
        $(this).dialog("close");
      },
    },
    close: function() {
      allFields.val('').removeClass('ui-state-error');
    }
  });
  
  $("#character-sequence-add-button").button().click(function() {
    $("#character-sequence-add-dialog").dialog("open");
  });
});