// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets.
 
function fieldsetsToObject(root) {
  var obj = {fieldsets:[]};
  
  root.find('fieldset').each(function(i, fieldset) {
    fieldset = $(fieldset);
    
    var fields;
    
    var fsObj = {
      id: fsInfo("fieldset", fieldset),
      multiple: fsInfo("multiple", fieldset) == "true",
      collapse: fsInfo("collapse", fieldset) == "true",
      name: fsInfo("name", fieldset),
      label: fsInfo("label", fieldset),
      order: fsInfo("order", fieldset) * 1
    };

    fields = fsContainer(fsObj.id).children('.fields');
    
    if (!fsObj.multiple) {
      $.extend(fsObj, fieldsToObject(fields.first()));
    } else {
      fsObj.multifields = [];
      
      fields.each(function(j, field) {
        field = $(field);
        
        fsObj.multifields[j] = fieldsToObject(field, j);
      });
    }
    
    obj.fieldsets[i] = fsObj;
  });
  
  return obj;
}

// Convert field values to an object that can be converted to JSON

function fieldsToObject(fields, index) {
  fields = fields.children('.field-container').children('.field');
  var obj = {fields:[]};
  
  fields.each(function(i, field) {
    field = $(field);
    
    obj.fields[i] = {
      id: fInfo("field", field),
      name: fInfo("name", field),
      label: fInfo("label", field),
      head: fInfo("head", field) == "true",
      reversal: fInfo("reversal", field) == "true",
      required: fInfo("required", field) == "true",
      min: stringToNumber(fInfo("min", field)),
      max: stringToNumber(fInfo("max", field)),
      instance: fInfo("instance", field),
      regex: fInfo("regex", field),
      order: fInfo("order", field) * 1,
      subcategory: fInfo("subcategory", field),
      value: getFieldValue(field)
    };
    
    if (index >= 0) {
      obj.fields[i].index = index;
    }
  });
  
  return obj;
}

// Get the correct value for a boolean that can be null
function getOpenboolean(value) {
  switch (value) {
    case "true":
      value = true;
      break;
    case "false":
      value = false;
      break;
    default:
      value = null;
  }
  
  return value
}

// Get a number from a string. Blanks are returned as an empty string.

function getNumber(value) {
  if (isBlank(value)) {
    value = '';
  } else if (!isNaN(value)) {
    value = value * 1;
  }
  
  return value;
}

// Items in multiple select lists are URL encoded

function getMultiple(value) {
  if (value) {
    value = value.map(function(v) {return getEncoded(v)});
  } else {
    value = null;
  }
  
  return value;
}

// Items in select lists are URL encoded

function getEncoded(value) {
  return decodeURIComponent(value.replace(/\+/g," "));
}

// Get the value from a field using the subcategory to ensure
// that the value has the correct type and is properly formatted.

function getFieldValue(field) {
  var value;
  
  switch (fInfo("subcategory", field)) {
    case "boolean":
      value = field.is('input:checkbox:checked');
      break;
    case "openboolean":
      value = getOpenboolean(field.val())
      break;
    case "integer":
    case "rational":
      value = getNumber(field.val());
      break;
    case "multiselect":
    case "docmultiselect":
      value = getMultiple(field.val());
      break;
    case "select":
    case "docselect":
      value = getEncoded(field.val());
      break;
    default:
      value = field.val();
  }
  
  return value;
}
