$(function () {
  var documentTypeName = $("#document-type-name"),
    documentTypeDescription = $("#document-type-description"),
    fieldsetName = $("#fieldset-name-input"),
    fieldsetDescription = $("#fieldset-description-input"),
    fieldsetOrder = $("#fieldset-order-input"),
    fieldsetDoctype = $("#fieldset-doctype-input"),
    fieldsetMultiple = $("#fieldset-multiple-input"),
    fieldName = $("#field-name-input"),
    fieldLabel = $("#field-label-input"),
    fieldSubcategory = $("#field-subcategory-input"),
    fieldDescription = $("#field-description-input"),
    fieldOrder = $("#field-order-input"),
    fieldDoctype = $("#field-doctype-input"),
    fieldFieldset = $("#field-fieldset-input"),
    tips = $(".validate-tips"),
    allDoctypeFields = $([]).add(documentTypeName).add(documentTypeDescription),
    allFieldsetFields = $([]).add(fieldsetName).add(fieldsetOrder);
    allFieldFields = $([]).add(fieldName).add(fieldLabel).add(fieldOrder);
  
  function postConfigDoc(ajaxUrl, obj, completeFun, callContext) {
    $.ajax({
      type: "POST",
      url: ajaxUrl,
      dataType: "json",
      context: callContext,
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function(req, status) {
        if (req.status == 201) {
          completeFun(this);
        }
      }
    });
  }
  
  function populateFields(fieldsetId) {
    $.getJSON("config/" + fieldsetId + "/fields", function(data) {
      var fieldContainer = $("#fields-" + fieldsetId);
      fieldContainer.empty();
      data.renderings.forEach(function(rendering) {
        fieldContainer.append(rendering);
      });
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
      $(".add-field-button").button().click(function() {
        fieldDoctype.val($(this).attr("data-doctype-id"));
        fieldFieldset.val($(this).attr("data-fieldset-id"));
        fieldOrder.val("0");
        $("#field-add-dialog").dialog("open");
      });
      data.rows.forEach(function(element) {
        populateFields(element.value._id);
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
              fieldsetDoctype.val($(this).attr("data-doctype-id"));
              fieldsetOrder.val("0");
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
          var obj = {
            "category": "doctype", 
            "description": documentTypeDescription.val(),
            "_id": documentTypeName.val()
          },
          complete = function(context) {
            populateDocTypeTabs();
            $(context).dialog("close");
          };
          postConfigDoc("config/doctypes", obj, complete, this);
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
          && checkRegexp(fieldsetOrder, /^[0-9]+$/, "fieldset order must be a number", tips));
        
        if (checkResult) {
          var obj = {
            "category": "fieldset", 
            "name": fieldsetName.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": fieldsetDoctype.val(),
            "multiple": (fieldsetMultiple.val() == "true")
          },
          complete = function(context) {
            populateFieldsets(fieldsetDoctype.val());
            $(context).dialog("close");
          };
          postConfigDoc("config/" + fieldsetDoctype.val() + "/fieldsets", obj, complete, this);
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
  
  $("#field-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Field": function() {
        allFieldFields.removeClass('ui-state-error');
        
        checkResult = (
             checkLength(fieldName, "field name", 1, 50, tips)
          && checkLength(fieldLabel, "field label", 1, 50, tips)
          && checkLength(fieldOrder, "field order", 1, 50, tips)
          && checkRegexp(fieldOrder, /^[0-9]+$/, "field order must be a number", tips)
        );
        
        if (checkResult) {
          var obj = {
            "category": "field", 
            "name": fieldName.val(),
            "label": fieldLabel.val(),
            "description": fieldDescription.val(),
            "order": (fieldOrder.val() * 1),
            "doctype": fieldDoctype.val(),
            "fieldset": fieldFieldset.val(),
            "subcategory": fieldSubcategory.val()
          },
          complete = function(context) {
            populateFields(fieldFieldset.val());
            $(context).dialog("close");
          };
          postConfigDoc("config/" + fieldFieldset.val() + "/fields", obj, complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      },
    },
    close: function() {
      allFieldFields.val('').removeClass('ui-state-error');
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