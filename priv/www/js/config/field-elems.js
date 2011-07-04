// Returns an object with references to add/edit fields dialog
// field elements with helper functions. 

function fieldElems() {
  var fElems = {};
  
  fElems.attrs = ["name", "label", "order", "description", "subcategory", 
                  "head", "reversal", "default", "required", "allowed", 
                  "source", "max", "min", "regex", "doctype", "fieldset",
                  "rev", "field"];
               
  fElems.get = function(values) {
    var fObj = {};
    
    fObj.attrs = fElems.attrs;
    
    fObj.notDefault = function() {
      return [fObj.allowed, fObj.source, fObj.min, fObj.max, fObj.regex];
    };
    
    fObj.hideDisable = function(blankAll) {
      fObj.notDefault().forEach(function(field) {
        field.attr("disabled", "disabled");
        if (blankAll) field.val('');
        field.parent().hide();
      });
      return fObj;
    };
    
    fObj.showEnable = function() {
      fObj.notDefault().forEach(function(field) {
        field.removeAttr("disabled");
        field.parent().show();
      });
      return fObj;
    };
    
    fObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]')) {
          if (source[field] == "true") fObj[field].attr('checked', true);
        }
      });
      return fObj;
    };
    
    fObj.hideBlanks = function() {
      fObj.notDefault().forEach(function(field) {
        if (field.val() == '') {
          field.attr("disabled", "disabled");
          field.parent().hide();
        }
      });
      return fObj;
    };
    
    fObj.getFieldInputVals = function() {
      var valObj = {
        "category": "field", 
        "name": fObj.name.val(),
        "label": fObj.label.val(),
        "default": fObj.decodeDefaults(fObj.subcategory.val(), fObj.default.val()),
        "head": fObj.head.is(':checked'),
        "reversal": fObj.reversal.is(':checked'),
        "required": fObj.required.is(':checked'),
        "order": fObj.order.val() * 1,
        "allowed": fObj.allowed.val().split(","),
        "source": fObj.decodeSource(fObj.subcategory.val(), fObj.source.val()),
        "min": stringToNumber(fObj.min.val()),
        "max": stringToNumber(fObj.max.val()),
        "regex": fObj.regex.val(),
        "description": fObj.description.val(),
        "doctype": fObj.doctype.val(),
        "fieldset": fObj.fieldset.val(),
        "subcategory": fObj.subcategory.val()
      }
      return valObj;
    };
    
    fObj.clear = function() {
      clearValues($('#field-dialog .input')).removeClass('ui-state-error');
      fObj.hideDisable();
      return fObj;
    };
    
    fObj.decodeSource = function(subcategory, source) {
      switch (subcategory) {
        case "file":
          return source.split("/");
        default:
          return defaults;
      }
    };
    
    fObj.decodeDefaults = function(subcategory, defaults) {
      switch (subcategory) {
        case "docmultiselect":
        case "multiselect":
          return defaults.split(",").map(function (item) {return item.trim()});
        case "file":
          return defaults.split("/");
        default:
          return defaults;
      }
    };
                   
    fObj.attrs.forEach(function(item) {
      fObj[item] = $('#field-' + item + '-input');
    });
    
    fObj.copyValues(values);
    fObj.showEnable();
    fObj.hideBlanks();
      
    fObj.subcategory.change(function() {
      switch (fObj.subcategory.val()) {
        case "select":
        case "multiselect":
          fObj.hideDisable(true);
          break;
        case "docselect":
        case "docmultiselect":
        case "file":
          fObj.hideDisable(true);
          fObj.source.removeAttr("disabled");
          fObj.source.parent().show();
          break;
        case "text":
        case "textarea":
          fObj.hideDisable(true);
          fObj.regex.removeAttr("disabled");
          fObj.regex.parent().show();
          break;
        case "date":
        case "integer":
        case "rational":
          fObj.hideDisable(true);
          fObj.min.removeAttr("disabled");
          fObj.min.parent().show();
          fObj.max.removeAttr("disabled");
          fObj.max.parent().show();
          break;
        default:
          fObj.hideDisable(true);
      }
    });
      
    return fObj;
  };
  
  return fElems;
}

