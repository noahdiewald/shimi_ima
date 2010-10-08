// TODO: maybe saying $('input, select, textarea...) could serve
//       as alternative to variable passing.
function clearValues(inputFields) {
  inputFields.each(function(index) {
    inputField = $(this);
    
    if (! inputField.attr('data-retain')) {
      if (inputField.is(':checked')) {
        inputField.attr('checked', false);
      }
      inputField.val('');
    }
  });
  
  return inputFields;
}

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
  var fieldDoctype = $("#field-doctype-input");
  var fieldFieldset = $("#field-fieldset-input");
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
      fieldDoctype.val($(this).attr('data-doctype-id'));
      fieldFieldset.val($(this).attr('data-fieldset-id'));
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
    var fieldsetDoctype = $("#fieldset-doctype-input");

    $("#doctype-tabs-headings").empty();
    $("#doctype-tabs-headings + .ui-tabs-panel").remove();
    $("#doctype-tabs").tabs("destroy");
    $("#doctype-tabs-headings").html(doctypes);
    
    $("#doctype-tabs").tabs(
      {
        load: function(event, ui) {
          doctypeId = $(ui.panel).children()[0].id;
          
          populateFieldsets(doctypeId);

          $(".add-fieldset-button").button({
            icons: {primary: "ui-icon-plus"}
          }).click(function() {
            fieldsetDoctype.val($(this).attr("data-doctype-id"));
            $("#fieldset-add-dialog").dialog("open");
          });
        }
      }
    );
  });
}

$(function () {
  
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
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "doctype", 
            "description": $("#doctype-description-input").val(),
            "_id": $("#doctype-name-input").val()
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
      clearValues($('.input')).removeClass('ui-state-error');
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
        var fieldsetName = $("#fieldset-name-input");
        var fieldsetDescription = $("#fieldset-description-input");
        var fieldsetOrder = $("#fieldset-order-input");
        var fieldsetDoctype = $("#fieldset-doctype-input");
        var fieldsetMultiple = $("#fieldset-multiple-input");
        
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
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
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  $("#field-add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Field": function() {
        var fieldName = $("#field-name-input");
        var fieldLabel = $("#field-label-input");
        var fieldSubcategory = $("#field-subcategory-input");
        var fieldDescription = $("#field-description-input");
        var fieldHead = $("#field-head-input");
        var fieldReversal = $("#field-reversal-input");
        var fieldOrder = $("#field-order-input");
        var fieldDoctype = $("#field-doctype-input");
        var fieldFieldset = $("#field-fieldset-input");
        
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "field", 
            "name": fieldName.val(),
            "label": fieldLabel.val(),
            "head": fieldHead.is(':checked'),
            "reversal": fieldReversal.is(':checked'),
            "order": (fieldOrder.val() * 1),
            "description": fieldDescription.val(),
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
      clearValues($('.input')).removeClass('ui-state-error');
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
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  $("#character-sequence-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#character-sequence-add-dialog").dialog("open");
  });
});