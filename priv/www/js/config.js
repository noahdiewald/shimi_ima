$(function () {
  var doctypeName = $("#doctype-name-input"),
    doctypeDescription = $("#doctype-description-input"),
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
    allDoctypeFields = $([]).add(doctypeName).add(doctypeDescription),
    allFieldsetFields = $([]).add(fieldsetName).add(fieldsetOrder).add(fieldsetDescription);
    allFieldFields = $([]).add(fieldName).add(fieldLabel).add(fieldDescription).add(fieldOrder);
  
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
  
  function populateFields(doctypeId, fieldsetId) {
    var url = "config/doctypes/" + doctypeId + 
              "/fieldsets/" + fieldsetId + 
              "/fields"
          
    $.get(url, function(fields) {
      var fieldContainer = $("#fields-" + fieldsetId);
      fieldContainer.empty();
      fieldContainer.html(fields);
    });
  }
  
  function populateFieldsets(doctypeId) {
    var url = "config/doctypes/" + doctypeId + 
              "/fieldsets"
              
    $.get(url, function(fieldsets) {
      var fieldsetContainer = $("#fieldsets-" + doctypeId);
      
      fieldsetContainer.empty();
      fieldsetContainer.accordion("destroy");
      fieldsetContainer.html(fieldsets);
      
      fieldsetContainer.accordion({
        autoHeight: false,
        collapsible: true
      });
      
      $(".add-field-button").button({
        icons: {primary: "ui-icon-plus"}
      }).click(function() {
        fieldDoctype.val($(this).attr("data-doctype-id"));
        fieldFieldset.val($(this).attr("data-fieldset-id"));
        fieldOrder.val("50");
        $("#field-add-dialog").dialog("open");
      });
      
      $('.accordion-head').each(function(index) {
        var fieldsetId = $(this).attr('data-fieldset-id');
        
        populateFields(doctypeId, fieldsetId);
      });
    });
  }
  
  function populateDoctypeTabs() {
    var url = "config/doctypes";
    
    $.get(url, function(doctypes) {
      $("#doctype-tabs-headings").empty();
      $("#doctype-tabs-headings + .ui-tabs-panel").remove();
      $("#doctype-tabs").tabs("destroy");
      $("#doctype-tabs-headings").html(doctypes);
      
      $("#doctype-tabs").tabs(
        {
          load: function(event, ui) {
            populateFieldsets($(ui.panel).children()[0].id);
  
            $(".add-fieldset-button").button({
              icons: {primary: "ui-icon-plus"}
            }).click(function() {
              fieldsetDoctype.val($(this).attr("data-doctype-id"));
              fieldsetOrder.val("0");
              $("#fieldset-add-dialog").dialog("open");
            });
          }
        }
      );
    });
  }
  
  $("#doctype-tabs").tabs();
  populateDoctypeTabs();
  
  $("#main-tabs").tabs();
  $("#character-sequence-tabs").tabs();
  $("#doctype-info").hide();
  $("#character-sequence-info").hide();
  
  $("#doctype-info-toggle").click(function() {
    $("#doctype-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#character-sequence-info-toggle").click(function() {
    $("#character-sequence-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#doctype-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Document Type": function() {
        allDoctypeFields.removeClass('ui-state-error');
        
        checkResult = checkLength(doctypeName, "document type name", 1, 50, tips); 
        
        if (checkResult) {
          var obj = {
            "category": "doctype", 
            "description": doctypeDescription.val(),
            "_id": doctypeName.val()
          },
          complete = function(context) {
            populateDoctypeTabs();
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
  
  $("#doctype-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#doctype-add-dialog").dialog("open");
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
          postConfigDoc("config/doctypes/" + fieldsetDoctype.val() + "/fieldsets", obj, complete, this);
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
            populateFields(fieldDoctype.val(), fieldFieldset.val());
            $(context).dialog("close");
          };
          postConfigDoc("config/doctypes/" + fieldDoctype.val() + "/fieldsets/" + fieldFieldset.val() + "/fields", obj, complete, this);
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
  
  $("#character-sequence-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#character-sequence-add-dialog").dialog("open");
  });
});