$(function () {
  var documentTypeName = $("#document-type-name"),
    documentTypeDescription = $("#document-type-description"),
    fieldsetName = $("#fieldset-name-input"),
    fieldsetDescription = $("#fieldset-description-input"),
    fieldsetOrder = $("#fieldset-order-input"),
    fieldsetDoctype = $("#fieldset-doctype-input"),
    fieldsetMultiple = $("#fieldset-multiple-input"),
    tips = $(".validate-tips"),
    allDoctypeFields = $([]).add(documentTypeName).add(documentTypeDescription),
    allFieldsetFields = $([]).add(documentTypeName).add(documentTypeDescription);
  
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
  
  function populateFieldsets(docTypeId) {
    $.getJSON("config/" + docTypeId + "/fieldsets", function(data) {
      var fieldsetContainer = $("#fieldsets-" + docTypeId);
      fieldsetContainer.empty();
      fieldsetContainer.accordion("destroy");
      data.renderings.forEach(function(rendering) {
        fieldsetContainer.append(rendering);
      });
      fieldsetContainer.accordion({
        autoHeight: false,
        collapsible: true
      });
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
      $("#document-type-tabs").tabs(
        {
          load: function(event, ui) {
            populateFieldsets($(ui.panel).children()[0].id);
  
            $(".add-fieldset-button").button().click(function() {
              fieldsetDoctype.val($(this).attr("data-doctype-id")),
              fieldsetOrder.val("0"),
              $("#fieldset-add-dialog").dialog("open");
            });
          }
        }
      );
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
        allDoctypeFields.removeClass('ui-state-error');
        
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
      allDoctypeFields.val('').removeClass('ui-state-error');
    }
  });
  
  $("#doc-type-add-button").button().click(function() {
    $("#doc-type-add-dialog").dialog("open");
  });
  
  $("#fieldset-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Fieldset": function() {
        allFieldsetFields.removeClass('ui-state-error');
        
        checkResult = (checkLength(fieldsetName, "fieldset name", 1, 50, tips)
          && checkLength(fieldsetOrder, "fieldset order", 1, 50, tips)
          && checkRegexp(fieldsetOrder, /[0-9]*/, "fieldset order must be a number", tips));
        
        if (checkResult) {
          obj = {
            "category": "fieldset", 
            "name": fieldsetName.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": fieldsetDoctype.val(),
            "multiple": (fieldsetMultiple.val() == "true")
          };
          postConfigDoc("config/" + fieldsetDoctype.val() + "/fieldsets", obj);
          populateFieldsets(fieldsetDoctype.val());
          $(this).dialog("close");
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      },
    },
    close: function() {
      allFieldsetFields.val('').removeClass('ui-state-error');
    }
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