// Depending on the class trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".edit-field-button": function(t) {editFieldButton(t)},
    ".delete-field-button": function(t) {deleteFieldButton(t)},
    ".add-field-button": function(t) {addFieldButton(t)},
    ".edit-fieldset-button": function(t) {editFieldsetButton(t)},
    ".delete-fieldset-button": function(t) {deleteFieldsetButton(t)},
    ".add-fieldset-button": function(t) {addFieldsetButton(t)},
    "h3.accordion-head a": function(t) {accordionHead(t)}
  });

  action(e);
}

// Build a url based on information available on page

function buildUrl(source, prefix) {
  if (prefix) {
    prefix = prefix + "-";
  } else {
    prefix = "";
  }
  
  var url = {
    string: "config/",
    origin: source,
    type: prefix + "url",
    valid_components: ["doctype", "fieldset", "field"],
    send: function(object, method, callback, context) {
      sendConfigDoc(this.string, object, method, callback, context);
      return this;
    },
    put: function(object, callback, context) {
      this.send(object, 'PUT', callback, context);
      return this;
    },
    post: function(object, callback, context) {
      this.send(object, 'POST', callback, context);
      return this;
    },
    delete: function(object, callback, context) {
      this.sendConfigDoc(object, 'DELETE', callback, context);
      return this;
    },
    toString: function() {
      var rev;
      
      this.string.concat(this.valid_components.map(function(item) {
        var plural = item + "s";
        var value;
        
        if (value = getData(prefix + item, source)) {
          return plural + "/" + "value";
        } else if (prefix == item + "-") {
          return plural;
        }
      }).join("/"));
      
      if (rev = getData(prefix + 'rev', source)) {
        this.string.concat("?rev=" + rev);
      }
    }
  };
  
  url.valid_components.forEach(function(item) {
    url[item] = (function() {return getData(prefix + item, source)})();
  });

  return url; 
}

// Returns an object with references to add/edit fields dialog
// field elements with some helper functions. 
// TODO look at how an OO person does this

function fieldElems() {
  var attrs = ["name", "label", "order", "description", "subcategory", 
               "head", "reversal", "default", "required", "allowed", 
               "source", "max", "min", "regex", "doctype", "fieldset",
               "rev", "field"];
               
  return {
    attrs: attrs,
    getFieldElems: function(values) {
      var fieldsObj = {
        notDefault: function() {
          return [this.allowed, this.source, this.min, this.max, this.regex];
        },
        hideDisable: function(blankAll) {
          this.notDefault().forEach(function(field) {
            field.attr("disabled", "disabled");
            if (blankAll) field.val('');
            field.parent().hide();
          });
          return this;
        },
        showEnable: function() {
          this.notDefault().forEach(function(field) {
            field.removeAttr("disabled");
            field.parent().show();
          });
          return this;
        },
        copyValues: function(source) {
          Object.keys(source).forEach(function(field) {
            fieldsObj[field].val(source[field]);
            if (fieldsObj[field].is('input[type=checkbox]')) {
              if (source[field] == "true") fieldsObj[field].attr('checked', true);
            }
          });
          return this;
        },
        hideBlanks: function() {
          this.notDefault().forEach(function(field) {
            if (field.val() == '') {
              field.attr("disabled", "disabled");
              field.parent().hide();
            }
          });
          return this;
        },
        getFieldInputVals: function() {
          var valObj = {
            "category": "field", 
            "name": this.name.val(),
            "label": this.label.val(),
            "default": decodeDefaults(this.subcategory.val(), this.default.val()),
            "head": this.head.is(':checked'),
            "reversal": this.reversal.is(':checked'),
            "required": this.required.is(':checked'),
            "order": this.order.val() * 1,
            "allowed": this.allowed.val().split(","),
            "source": this.source.val(),
            "min": this.min.val(),
            "max": this.max.val(),
            "regex": this.regex.val(),
            "description": this.description.val(),
            "doctype": this.doctype.val(),
            "fieldset": this.fieldset.val(),
            "subcategory": this.subcategory.val()
          }
          return valObj;
        },
        clear: function() {
          clearValues($('#field-dialog .input')).removeClass('ui-state-error');
          this.hideDisable();
          return this;
        }
      };
                   
      attrs.forEach(function(item) {
         fieldsObj[item] = $('#field-' + item + '-input');
      });
      
      if (values) {
        fieldsObj.copyValues(values);
        fieldsObj.showEnable();
        fieldsObj.hideBlanks();
      }
      
      fieldsObj.subcategory.change(function() {
        switch (fieldsObj.subcategory.val()) {
          case "select":
          case "multiselect":
          case "docselect":
          case "docmultiselect":
            fieldsObj.hideDisable(true);
            fieldsObj.source.removeAttr("disabled");
            fieldsObj.source.parent().show();
            break;
          case "text":
          case "textarea":
            fieldsObj.hideDisable(true);
            fieldsObj.regex.removeAttr("disabled");
            fieldsObj.regex.parent().show();
            break;
          case "date":
          case "integer":
          case "rational":
            fieldsObj.hideDisable(true);
            fieldsObj.min.removeAttr("disabled");
            fieldsObj.min.parent().show();
            fieldsObj.max.removeAttr("disabled");
            fieldsObj.max.parent().show();
            break;
          default:
            fieldsObj.hideDisable(true);
        }
      });
      
      return fieldsObj;
    }
  }
}

function getData(value, elem) {
  var dataElem = elem.attr('data-group-id');
  return $('#' + dataElem).attr('data-' + value);
}

// Button that opens a dialog for editing a field

function editFieldButton(button) {
  var url = buildUrl(button, "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getData('field-' + item, button);
  });
  
  initFieldEditDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a field

function deleteFieldButton(button) {
  var answer = confirm("Are you sure? This is permanent.");
  
  if (answer) {
    var rev = getData('field-rev', button);
    var doctype = getData('doctype-id', button);
    var fieldset = getData('fieldset-id', button);
    var field = getData('field-id', button);
    var url = "config/doctypes/" + doctype + 
              "/fieldsets/" + fieldset + 
              "/fields/" + field + "?rev=" + rev;
    var complete = function() {populateFields(doctype, fieldset)};
    
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
}

// Button that opens a dialog for adding a field

function addFieldButton(button) {
  var fieldset = getData('fieldset-id', button);
  var doctype = getData('doctype-id', button);
  
  initFieldAddDialog(fieldset, doctype).dialog("open");
}

// Button that opens a dialog for editing a fieldset

function editFieldsetButton(button) {
  var rev = getData('fieldset-rev', button);
  var doctype = getData('doctype-id', button);
  var fieldset = getData('fieldset-id', button);
  var oldobj = {
    name: getData('fieldset-name', button),
    label: getData('fieldset-label', button),
    order: getData('fieldset-order', button),
    multiple: getData('fieldset-multiple', button),
    collapse: getData('fieldset-collapse', button),
    description: getData('fieldset-description', button) 
  };
  
  initFieldsetEditDialog(fieldset, doctype, oldobj, rev).dialog("open");
}

// Button that opens a dialog for deleting a fieldset

function deleteFieldsetButton(button) {
  var rev = getData('fieldset-rev', button);
  var doctype = getData('doctype-id', button);
  var fieldset = getData('fieldset-id', button);
  var url = "config/doctypes/" + doctype + 
"/fieldsets/" + fieldset + "?rev=" + rev;
  var complete = function() {
    populateFieldsets(doctype);
  };
    
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
}

// Button that opens a dialog for adding a fieldset

function addFielsetButton(button) {
  fieldsetDoctype.val(getData('doctype-id', button));
  initFieldsetAddDialog().dialog("open");
}

function editDoctypeButton(button) {
  var description = getData('doctype-description', button);
  var name = getData('doctype-id', button);
  var rev = getData('doctype-rev', button);
  initDoctypeEditDialog(name, description, rev).dialog("open");
}

function deleteDoctypeButton(button) {
  var name = getData('doctype-id', button);
  var rev = getData('doctype-rev', button);
  var url = "config/doctypes/" + name + "?rev=" + rev;
  var complete = function() {populateDoctypeTabs()};
  
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
}

function addDoctypeButton(button) {
}

// Action for click event on accordion head

function accordionHead(head) {
  var doctype = getData('fieldset-doctype', head.parent('h3'));
  var fieldset = getData('fieldset-fieldset', head.parent('h3'));
  populateFields(doctype, fieldset);
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
  
  return false;
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

function encodeDefaults(subcategory, defaults) {
  switch (subcategory) {
    case "docmultiselect":
    case "multiselect":
      return defaults.join(", ");
    default:
      return defaults;
  }
}

function decodeDefaults(subcategory, defaults) {
  switch (subcategory) {
    case "docmultiselect":
    case "multiselect":
      return defaults.split(",").map(function (item) {return item.trim()});
    default:
      return defaults;
  }
}

function setSubcategoryChangeAction(f) {
  f.subcategory.change(function(e) {
    switch ($(e.target).val()) {
      case "select":
      case "multiselect":
      case "docselect":
      case "docmultiselect":
        f.hideDisable(true);
        f.source.removeAttr("disabled");
        f.source.parent().show();
        break;
      case "text":
      case "textarea":
        f.hideDisable(true);
        f.regex.removeAttr("disabled");
        f.regex.parent().show();
        break;
      case "date":
      case "integer":
      case "rational":
        f.hideDisable(true);
        f.min.removeAttr("disabled");
        f.min.parent().show();
        f.max.removeAttr("disabled");
        f.max.parent().show();
        break;
      default:
        f.hideDisable(true);
    }
  });
}

function initFieldAddDialog(url) {
  var f = fieldElems().getFieldElems();
   
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        var obj = f.getFieldInputVals();
        complete = function(context) {
          populateFields(url.doctype, url.fieldset);
          $(context).dialog("close");
        };
        f.post(obj, complete, this);
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  f.hideDisable();
  
  return dialog;
}

function copyValues(form, oldobj) {
  for (var name in oldobj) {
    form[name].val(oldobj[name]);
    if (form[name].is('input[type=checkbox]')) {
      if (oldobj[name] == "true") form[name].attr('checked', true);
    }
  }
}

function initFieldEditDialog(url, oldobj) {
  var f = fieldElems().getFieldElems(oldobj);
   
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getFieldInputVals();
        var complete = function(context) {
          populateFields(url.doctype, fieldset);
          $(context).dialog("close");
        };
        url.put(obj, complete, this);
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
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

function populateFields(doctypeId, fieldsetId) {
  var url = "config/doctypes/" + doctypeId + 
            "/fieldsets/" + fieldsetId + 
            "/fields";
        
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
  });
}

function populateDoctypeTabs() {
  var url = "config/doctypes";
  
  $.get(url, function(doctypes) {
    var fieldsetDoctype = $("#fieldset-doctype-input");
    var loadFun = function(event, ui) {
      doctype = $(ui.panel).children()[0].id;
      populateFieldsets(doctype);
    };
    
    $("#doctype-tabs-headings").empty();
    $("#doctype-tabs-headings + .ui-tabs-panel").remove();
    $("#doctype-tabs").tabs("destroy");
    $("#doctype-tabs-headings").html(doctypes);
    $("#doctype-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
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

$(function () {
  $('body').click(function(e) {clickDispatch(e)});
  initTabs(); 
  initHelpText();
  $('#doctype-dialog').hide();
  $("#fieldset-dialog").hide();
  $("#field-dialog").hide();
  initDoctypeAddButton();
  initCharsecAddDialog();
  initCharsecAddButton();
});