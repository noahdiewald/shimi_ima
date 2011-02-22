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
            "/fields";
        
  $.get(url, function(fields) {
    var fieldContainer = $("#fields-" + fieldsetId);
    fieldContainer.empty();
    fieldContainer.html(fields);
    $('.field-row').each(function(field) {
      var id = $(this).attr('data-field-id');
      
      $('#edit-field-' + id + '-button').button({
        icons: {primary: "ui-icon-pencil"},
        text: false
      }).click(function() {
        var bttn = $(this);
        var rev = bttn.attr('data-field-rev');
        var doctype = bttn.attr('data-doctype-id');
        var fieldset = bttn.attr('data-fieldset-id');
        var oldobj = {
          name: bttn.attr('data-field-name'),
          label: bttn.attr('data-field-label'),
          order: bttn.attr('data-field-order'),
          description: bttn.attr('data-field-description'),
          subcategory: bttn.attr('data-field-subcategory'),
          head: bttn.attr('data-field-head'),
          reversal: bttn.attr('data-field-reversal'),
          default: bttn.attr('data-field-default'),
          required: bttn.attr('data-field-required'),
          allowed: bttn.attr('data-field-allowed'),
          source: bttn.attr('data-field-source'),
          max: bttn.attr('data-field-max'),
          min: bttn.attr('data-field-min'),
          regex: bttn.attr('data-field-regex')
        };
        
        initFieldEditDialog(id, fieldset, doctype, oldobj, rev).dialog("open");
      });
  
      $("#delete-field-" + id + "-button").button({
        icons: {primary: "ui-icon-trash"},
        text: false
      }).click(function() {
        var bttn = $(this);
        var rev = bttn.attr('data-field-rev');
        var doctype = bttn.attr('data-doctype-id');
        var fieldset = bttn.attr('data-fieldset-id');
        var url = "config/doctypes/" + doctype + 
                  "/fieldsets/" + fieldset + 
                  "/fields/" + id + "?rev=" + rev;
        var complete = function() {
          populateFields(doctype, fieldset);
        };
          
        if (confirm("Are you sure? This is permanent.")) {
          sendConfigDoc(url, {}, 'DELETE', complete, this);
        }
      });
    });
  });
}

function populateFieldsets(doctypeId) {
  var fieldDoctype = $("#field-doctype-input");
  var fieldFieldset = $("#field-fieldset-input");
  var url = "config/doctypes/" + doctypeId + 
            "/fieldsets";
            
  $.get(url, function(fieldsets) {
    var fieldsetContainer = $("#fieldsets-" + doctypeId);
    
    fieldsetContainer.empty();
    fieldsetContainer.accordion("destroy");
    fieldsetContainer.html(fieldsets);
    
    fieldsetContainer.accordion({
      autoHeight: false,
      collapsible: true,
      active: false
    });
    
    $(".add-field-button").button({
      icons: {primary: "ui-icon-plus"}
    }).click(function() {
      var fieldset = $(this).attr('data-fieldset-id');
      var doctype = $(this).attr('data-doctype-id');
      
      initFieldAddDialog(fieldset, doctype).dialog("open");
    });
    
    $('.accordion-head').each(function(index) {
      var fieldsetId = $(this).attr('data-fieldset-id');

      $("#edit-fieldset-" + fieldsetId + "-button").button({
        icons: {primary: "ui-icon-pencil"}
      }).click(function() {
        var bttn = $(this);
        var rev = bttn.attr('data-fieldset-rev');
        var doctype = bttn.attr('data-doctype-id');
        var oldobj = {
          name: bttn.attr('data-fieldset-name'),
          label: bttn.attr('data-fieldset-label'),
          order: bttn.attr('data-fieldset-order'),
          multiple: bttn.attr('data-fieldset-multiple'),
          collapse: bttn.attr('data-fieldset-collapse'),
          description: bttn.attr('data-fieldset-description') 
        };
        
        initFieldsetEditDialog(fieldsetId, doctype, oldobj, rev).dialog("open");
      });
  
      $("#delete-fieldset-" + fieldsetId + "-button").button({
        icons: {primary: "ui-icon-trash"}
      }).click(function() {
        var bttn = $(this);
        var rev = bttn.attr('data-fieldset-rev');
        var doctype = bttn.attr('data-doctype-id');
        var url = "config/doctypes/" + doctype + 
                  "/fieldsets/" + fieldsetId + "?rev=" + rev;
        var complete = function() {
          populateFieldsets(doctype);
        };
          
        if (confirm("Are you sure? This is permanent.")) {
          sendConfigDoc(url, {}, 'DELETE', complete, this);
        }
      });
      
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
            var url = "config/doctypes/" + name + "?rev=" + rev;
            
            var complete = function() {
              populateDoctypeTabs();
            };
            
            if (confirm("Are you sure? This is permanent.")) {
              sendConfigDoc(url, {}, 'DELETE', complete, this);
            }
          });
          
          populateFieldsets(doctypeId);

          $(".add-fieldset-button").button({
            icons: {primary: "ui-icon-plus"}
          }).click(function() {
            fieldsetDoctype.val($(this).attr("data-doctype-id"));
            initFieldsetAddDialog().dialog("open");
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
      }
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
  var url = "config/doctypes/" + name + "?rev=" + rev;
  
  doctypeDescription.val(description);
  doctypeName.val(name);
  doctypeName.attr('disabled', true);

  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
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
          
          sendConfigDoc(url, obj, 'PUT', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
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
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        var fieldsetName = $("#fieldset-name-input");
        var fieldsetLabel = $("#fieldset-label-input");
        var fieldsetDescription = $("#fieldset-description-input");
        var fieldsetOrder = $("#fieldset-order-input");
        var fieldsetDoctype = $("#fieldset-doctype-input");
        var fieldsetMultiple = $("#fieldset-multiple-input");
        var fieldsetCollapse = $("#fieldset-collapse-input");
        
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
            "multiple": fieldsetMultiple.is(':checked'),
            "collapse": fieldsetCollapse.is(':checked')
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
      }
    },
    close: function() {
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  return dialog;
}

function initFieldsetEditDialog(id, doctype, oldobj, rev) {
  var fieldsetName = $("#fieldset-name-input");
  var fieldsetLabel = $("#fieldset-label-input");
  var fieldsetDescription = $("#fieldset-description-input");
  var fieldsetOrder = $("#fieldset-order-input");
  var fieldsetMultiple = $("#fieldset-multiple-input");
  var fieldsetCollapse = $("#fieldset-collapse-input");
  var url = "config/doctypes/" + doctype + 
            "/fieldsets/" + id + "?rev=" + rev;
            
  fieldsetName.val(oldobj.name);
  fieldsetLabel.val(oldobj.label);
  fieldsetDescription.val(oldobj.description);
  fieldsetOrder.val(oldobj.order);
  if (oldobj.multiple == "true") {
    fieldsetMultiple.attr('checked', true);
  }
  if (oldobj.collapse == "true") {
    fieldsetCollapse.attr('checked', true);
  }
        
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "_id": id, 
            "category": "fieldset", 
            "name": fieldsetName.val(),
            "label": fieldsetLabel.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": doctype,
            "multiple": fieldsetMultiple.is(':checked'),
            "collapse": fieldsetCollapse.is(':checked')
          },
          complete = function(context) {
            populateFieldsets(doctype);
            $(context).dialog("close");
          };
           
          sendConfigDoc(url, obj, 'PUT', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  return dialog;
}

function initFieldAddDialog(fieldset, doctype) {
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
  var fieldMin = $("#field-min-input");
  var fieldMax = $("#field-max-input");
  var fieldRegex = $("#field-regex-input");
  var notDefault = [fieldAllowed, 
                    fieldSource, 
                    fieldMin, 
                    fieldMax, 
                    fieldRegex];
  var url = "config/doctypes/" + doctype + 
            "/fieldsets/" + fieldset + "/fields";

  function hideDisable(blank) {
    notDefault.forEach(function(field) {
      field.attr("disabled", "disabled");
      if (blank) field.val('');
      field.parent().hide();
    });
  }
      
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
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
            "doctype": doctype,
            "fieldset": fieldset,
            "subcategory": fieldSubcategory.val()
          },
          complete = function(context) {
            populateFields(doctype, fieldset);
            $(context).dialog("close");
          };
          sendConfigDoc(url, obj, 'POST', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
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
  
  return dialog;
}

function getFieldElems() {
  var obj = {
    name: $("#field-name-input"),
    label: $("#field-label-input"),
    subcategory: $("#field-subcategory-input"),
    description: $("#field-description-input"),
    head: $("#field-head-input"),
    reversal: $("#field-reversal-input"),
    order: $("#field-order-input"),
    doctype: $("#field-doctype-input"),
    fieldset: $("#field-fieldset-input"),
    default: $("#field-default-input"),
    required: $("#field-required-input"),
    allowed: $("#field-allowed-input"),
    source: $("#field-source-input"),
    min: $("#field-min-input"),
    max: $("#field-max-input"),
    regex: $("#field-regex-input"),
    notDefault: function() {
      return [this.allowed, this.source, this.min, this.max, this.regex];
    }
  };
  
  return obj;
}

function hideDisable(form, blank) {
  form.notDefault().forEach(function(field) {
    field.attr("disabled", "disabled");
    if (blank) field.val('');
    field.parent().hide();
  });
}

function showEnable(form) {
  form.notDefault().forEach(function(field) {
    field.removeAttr("disabled");
    field.parent().show();
  });
}

function copyValues(form, oldobj) {
  for (var name in oldobj) {
    form[name].val(oldobj[name]);
    if (form[name].is('input[type=checkbox]')) {
      if (oldobj[name] == "true") form[name].attr('checked', true);
    }
  }
}

function initFieldEditDialog(id, fieldset, doctype, oldobj, rev) {
  var form = getFieldElems();
  var url = "config/doctypes/" + doctype + 
            "/fieldsets/" + fieldset + 
            "/fields/" + id + "?rev=" + rev;
  
  copyValues(form, oldobj);
  showEnable(form);
  
  if (form.min.val() == '' && form.max.val() == '') {
    form.min.attr("disabled", "disabled");
    form.max.attr("disabled", "disabled");
    form.max.parent().hide();
    form.min.parent().hide();
  }
  
  if (form.regex.val() == '') {
    form.regex.attr("disabled", "disabled");
    form.regex.parent().hide();
  }
  
  if (form.allowed.val() == '') {
    form.allowed.attr("disabled", "disabled");
    form.allowed.parent().hide();
  }
  
  if (form.source.val() == '') {
    form.source.attr("disabled", "disabled");
    form.source.parent().hide();
  }
      
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "field", 
            "name": form.name.val(),
            "label": form.label.val(),
            "default": form.default.val(),
            "head": form.head.is(':checked'),
            "reversal": form.reversal.is(':checked'),
            "required": form.required.is(':checked'),
            "order": (form.order.val() * 1),
            "allowed": form.allowed.val().split(","),
            "source": form.source.val(),
            "min": form.min.val(),
            "max": form.max.val(),
            "regex": form.regex.val(),
            "description": form.description.val(),
            "doctype": doctype,
            "fieldset": fieldset,
            "subcategory": form.subcategory.val()
          },
          complete = function(context) {
            populateFields(doctype, fieldset);
            $(context).dialog("close");
          };
          sendConfigDoc(url, obj, 'PUT', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      clearValues($('.input')).removeClass('ui-state-error');
      hideDisable(form);
    }
  });
  
  form.subcategory.change(function() {
    var selected = form.subcategory.children('option:selected').val();
    var simpleSelects = ["select", "multiselect"];
    var remoteSelects = ["docselect", "docmultiselect"];
    var rangeable = ["date", "integer", "rational"];
    var matchable = ["text", "textarea"];
    
    if (simpleSelects.indexOf(selected) >= 0) {
      hideDisable(form, true);
      form.allowed.removeAttr("disabled");
      form.allowed.parent().show();
    } else if (remoteSelects.indexOf(selected) >= 0) {
      hideDisable(form, true);
      form.source.removeAttr("disabled");
      form.source.parent().show();
    } else if (matchable.indexOf(selected) >= 0) {
      hideDisable(form, true);
      form.regex.removeAttr("disabled");
      form.regex.parent().show();
    } else if (rangeable.indexOf(selected) >= 0) {
      hideDisable(form, true);
      form.min.removeAttr("disabled");
      form.min.parent().show();
      form.max.removeAttr("disabled");
      form.max.parent().show();
    } else {
      hideDisable(form, true);
    };
  });
  
  return dialog;
}

function initCharsecAddDialog() {
  $("#charseq-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add": function() {},
      "Cancel": function() {
        $(this).dialog("close");
      }
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
  $("#fieldset-dialog").hide();
  $("#field-dialog").hide();
  initDoctypeAddButton();
  initCharsecAddDialog();
  initCharsecAddButton();
});