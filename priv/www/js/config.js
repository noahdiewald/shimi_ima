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

function sendConfigDoc(ajaxUrl, obj, method, completeFun, callContext) {
  $.ajax({
    type: method,
    url: ajaxUrl,
    dataType: "json",
    context: callContext,
    contentType: "application/json",
    processData: false,
    data: JSON.stringify(obj),
    complete: function(req, status) {
      if (req.status >= 200) {
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
      $("#field-dialog").dialog("open");
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

          $('#edit-doctype-' + doctypeId + '-button').button({
            icons: {primary: "ui-icon-pencil"}
          }).click(function() {
            var description = $(this).attr("data-doctype-description");
            var name = $(this).attr("data-doctype-id");
            var rev = $(this).attr("data-doctype-rev");
            initDoctypeEditDialog(name, description, rev).dialog("open");
          });

          $('#delete-doctype-' + doctypeId + '-button').button({
            icons: {primary: "ui-icon-trash"}
          }).click(function() {
            var name = $(this).attr("data-doctype-id");
            var rev = $(this).attr("data-doctype-rev");
            var url = "config/doctypes/" + name + "?rev=" + rev
            
            var complete = function() {
              populateDoctypeTabs();
            };
            
            sendConfigDoc(url, {}, 'DELETE', complete, this);
          });
          
          populateFieldsets(doctypeId);

          $(".add-fieldset-button").button({
            icons: {primary: "ui-icon-plus"}
          }).click(function() {
            fieldsetDoctype.val($(this).attr("data-doctype-id"));
            $("#fieldset-dialog").dialog("open");
          });
        }
      }
    );
  });
}

function initTabs() {
  $("#doctype-tabs").tabs();
  populateDoctypeTabs();
  $("#main-tabs").tabs();
  $("#charseq-tabs").tabs();
  
  return true;
}

function initHelpText() {
  $("#doctype-info").hide();
  $("#charseq-info").hide();

  $("#doctype-info-toggle").click(function() {
    $("#doctype-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#charseq-info-toggle").click(function() {
    $("#charseq-info").toggle("blind", {}, 500);
    return false;
  });
  
  return true;
}

function initDoctypeAddDialog() {
  var doctypeName = $("#doctype-name-input");
  doctypeName.attr('disabled', false);
  
  var dialog = $("#doctype-dialog").dialog({
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
          sendConfigDoc("config/doctypes", obj, 'POST', complete, this);
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
  
  return dialog;
}

function initDoctypeEditDialog(name, description, rev) {
  var doctypeName = $("#doctype-name-input");
  var doctypeDescription = $("#doctype-description-input");
  
  doctypeDescription.val(description);
  doctypeName.val(name);
  doctypeName.attr('disabled', true);

  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save Document Type": function() {
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
          var url = "config/doctypes/" + name + "?rev=" + rev
          sendConfigDoc(url, obj, 'PUT', complete, this);
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
  
  return dialog;
}

function initDoctypeAddButton() {
  $("#doctype-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    initDoctypeAddDialog().dialog("open");
  });
  
  return true;
}

function initFieldsetAddDialog() {
  $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Fieldset": function() {
        var fieldsetName = $("#fieldset-name-input");
        var fieldsetLabel = $("#fieldset-label-input");
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
            "label": fieldsetLabel.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": fieldsetDoctype.val(),
            "multiple": (fieldsetMultiple.val() == "true")
          },
          complete = function(context) {
            populateFieldsets(fieldsetDoctype.val());
            $(context).dialog("close");
          };
          sendConfigDoc("config/doctypes/" + fieldsetDoctype.val() + "/fieldsets", obj, 'POST', complete, this);
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
  
  return true;
}

function initFieldAddDialog() {
  var fieldName = $("#field-name-input");
  var fieldLabel = $("#field-label-input");
  var fieldSubcategory = $("#field-subcategory-input");
  var fieldDescription = $("#field-description-input");
  var fieldHead = $("#field-head-input");
  var fieldReversal = $("#field-reversal-input");
  var fieldOrder = $("#field-order-input");
  var fieldDoctype = $("#field-doctype-input");
  var fieldFieldset = $("#field-fieldset-input");
  var fieldDefault = $("#field-default-input");
  var fieldRequired = $("#field-required-input");
  var fieldAllowed = $("#field-allowed-input");
  var fieldSource = $("#field-source-input");
  var fieldMin = $("#field-max-input");
  var fieldMax = $("#field-min-input");
  var fieldRegex = $("#field-regex-input");
  var notDefault = [fieldAllowed, 
                    fieldSource, 
                    fieldMin, 
                    fieldMax, 
                    fieldRegex];
  
  function hideDisable(blank) {
    notDefault.forEach(function(field) {
      field.attr("disabled", "disabled");
      if (blank) field.val('');
      field.parent().hide();
    });
  }
      
  $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Field": function() {
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "field", 
            "name": fieldName.val(),
            "label": fieldLabel.val(),
            "default": fieldDefault.val(),
            "head": fieldHead.is(':checked'),
            "reversal": fieldReversal.is(':checked'),
            "required": fieldRequired.is(':checked'),
            "order": (fieldOrder.val() * 1),
            "allowed": fieldAllowed.val().split(","),
            "source": fieldSource.val(),
            "min": fieldMin.val(),
            "max": fieldMax.val(),
            "regex": fieldRegex.val(),
            "description": fieldDescription.val(),
            "doctype": fieldDoctype.val(),
            "fieldset": fieldFieldset.val(),
            "subcategory": fieldSubcategory.val()
          },
          complete = function(context) {
            populateFields(fieldDoctype.val(), fieldFieldset.val());
            $(context).dialog("close");
          };
          sendConfigDoc("config/doctypes/" + fieldDoctype.val() + "/fieldsets/" + fieldFieldset.val() + "/fields", obj, 'POST', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      },
    },
    close: function() {
      clearValues($('.input')).removeClass('ui-state-error');
      hideDisable();
    }
  });
  
  hideDisable();
  
  fieldSubcategory.change(function() {
    var selected = fieldSubcategory.children('option:selected').val();
    var simpleSelects = ["select", "multiselect"];
    var remoteSelects = ["docselect", "docmultiselect"];
    var rangeable = ["date", "integer", "rational"];
    var matchable = ["text", "textarea"];
    
    if (simpleSelects.indexOf(selected) >= 0) {
      hideDisable(true);
      fieldAllowed.removeAttr("disabled");
      fieldAllowed.parent().show();
    } else if (remoteSelects.indexOf(selected) >= 0) {
      hideDisable(true);
      fieldSource.removeAttr("disabled");
      fieldSource.parent().show();
    } else if (matchable.indexOf(selected) >= 0) {
      hideDisable(true);
      fieldRegex.removeAttr("disabled");
      fieldRegex.parent().show();
    } else if (rangeable.indexOf(selected) >= 0) {
      hideDisable(true);
      fieldMin.removeAttr("disabled");
      fieldMin.parent().show();
      fieldMax.removeAttr("disabled");
      fieldMax.parent().show();
    } else {
      hideDisable(true);
    };
  });
  
  return true;
}

function initCharsecAddDialog() {
  $("#charseq-dialog").dialog({
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
  
  return true;
}

function initCharsecAddButton() {
  $("#charseq-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#charseq-add-dialog").dialog("open");
  });

  return true;
}

$(function () {
  initTabs(); 
  initHelpText();
  $('#doctype-dialog').hide();
  initDoctypeAddButton();
  initFieldsetAddDialog();
  initFieldAddDialog();
  initCharsecAddDialog();
  initCharsecAddButton();
});