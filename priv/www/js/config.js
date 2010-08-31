$(function () {
  var documentTypeName = $("#document-type-name"),
    documentTypeDescription = $("#document-type-description"),
    tips = $(".validate-tips"),
    allFields = $([]).add(documentTypeName).add(documentTypeDescription);
  
  function postConfigDoc(ajaxUrl, obj) {
    $.ajax({
      type: "POST",
      url: ajaxUrl,
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj)
    });
  }
  
  function populateDocTypeTabs() {
    $.getJSON("config/doctypes", function(data) {
      $("#document-type-tabs-headings").empty();
      $("#document-type-tabs-headings + .ui-tabs-panel").remove();
      $("#document-type-tabs").tabs("destroy");
      data.renderings.forEach(function(rendering) {
        $("#document-type-tabs-headings").append(rendering);
      });
      $("#document-type-tabs").tabs();
    });
  }
  
  $("#document-type-tabs").tabs();
  populateDocTypeTabs();
  
  $("#main-tabs").tabs();
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
      "Add Document Type": function() {
        allFields.removeClass('ui-state-error');
        
        checkResult = checkLength(documentTypeName, "document type name", 1, 50, tips); 
        if (checkResult) {
          obj = {
            "category": "doctype", 
            "description": documentTypeDescription.val(),
            "_id": documentTypeName.val()
          };
          postConfigDoc("config/doctypes", obj);
          populateDocTypeTabs();
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