// # Fieldsets (and fields)
//
// *Implicit depends:* DOM, JQuery
//
// Dealing with fields and fieldsets.

// Variable Definitions

var path = require('../path.js').path;
var store = require('../store.js').store;
var utils = require('../utils.js');
var editui = require('./editui.js');

// Internal functions

// Get the container for a fieldset with `id`.
var fsContainer = function (id)
{
  return $('#container-' + id);
};

// Get the doctype path.
var dpath = function (source, category)
{
  var url = path(source, category);
  url.doctype = false;
  return url;
};

// If the item referred to by `key` is in session storage perform the
// `success` action with the stored items as the argument, otherwise,
// get the item from the server and perform the `otherwise` action with
// the retrieved item as an argument.
var ifStoredElse = function (key, success, otherwise)
{
  var item = null;

  item = sessionStorage.getItem(key);

  if (item)
  {
    success(item);
  }
  else
  {
    $.get(key, otherwise);
  }
};

// Convert field values to an object that can be converted to JSON
var fieldsToObject = function (fields, index)
{
  fields = fields.children('.field-container').children('.field');
  var obj = {
    fields: []
  };

  fields.each(function (i, field)
  {
    field = $(field);
    var s = store(field);
    var value = getFieldValue(field);
    var instance = s.f('instance');

    obj.fields[i] = {
      id: s.f('field'),
      name: s.f('name'),
      label: s.f('label'),
      head: s.f('head') === 'true',
      reversal: s.f('reversal') === 'true',
      required: s.f('required') === 'true',
      min: dateOrNumber(s.f('subcategory'), s.f('min')),
      max: dateOrNumber(s.f('subcategory'), s.f('max')),
      instance: instance,
      charseq: s.f('charseq'),
      regex: s.f('regex'),
      order: s.f('order') * 1,
      subcategory: s.f('subcategory'),
      value: value
    };

    if (index >= 0)
    {
      obj.fields[i].index = index;
    }
  });

  return obj;
};

// `min` and `max` are either dates or numbers. Provide the correct
// value or the correct type depending on the subcategory of the field.
var dateOrNumber = function (subcategory, fieldvalue)
{
  if (subcategory === 'date')
  {
    return fieldvalue;
  }
  else
  {
    return utils.stringToNumber(fieldvalue);
  }
};

// Get the correct value for a boolean that can be null
var getOpenboolean = function (value)
{
  switch (value)
  {
  case 'true':
    value = true;
    break;
  case 'false':
    value = false;
    break;
  default:
    value = null;
  }

  return value;
};

// Get a number from a string. Blanks are returned as an empty string.
var getNumber = function (value)
{
  if (utils.isBlank(value))
  {
    value = '';
  }
  else if (!isNaN(value))
  {
    value = value * 1;
  }

  return value;
};

// Items in multiple select lists are URL encoded
var getMultiple = function (value)
{
  if (value)
  {
    value = value.map(function (v)
    {
      return getEncoded(v);
    });
  }
  else
  {
    value = null;
  }

  return value;
};

// Items in select lists are URL encoded
var getEncoded = function (value)
{
  return window.decodeURIComponent(value.replace(/\+/g, ' '));
};

// Get the value from a field using the subcategory to ensure
// that the value has the correct type and is properly formatted.
var getFieldValue = function (field)
{
  var value;

  switch (store(field).f('subcategory'))
  {
  case 'boolean':
    value = field.is('input:checkbox:checked');
    break;
  case 'openboolean':
    value = getOpenboolean(field.val());
    break;
  case 'integer':
  case 'rational':
    value = getNumber(field.val());
    break;
  case 'multiselect':
  case 'docmultiselect':
    value = getMultiple(field.val());
    break;
  case 'select':
  case 'docselect':
    value = getEncoded(field.val());
    break;
  default:
    value = field.val();
  }

  return value;
};

// Basic initialization of fields.
var initFields = function (container, callback, addInstances)
{
  var url = dpath(container, 'field');
  var section = container.children('.fields').last();
  var prependIt = function (data)
  {
    if (addInstances)
    {
      section.attr('id', 'last-added');
    }
    section.prepend(data);
    if (callback)
    {
      callback(section);
    }

    editui.afterFreshRefresh(addInstances);
  };
  var storeIt = function (data)
  {
    sessionStorage.setItem(url, data);
    prependIt(data);
  };

  ifStoredElse(url.toString(), prependIt, storeIt);

  return true;
};

// Initialize and fill multifieldsets.
var fillMultiFieldsets = function (vfieldset)
{
  vfieldset = $(vfieldset);
  var id = store(vfieldset).fs('fieldset');
  var container = $('#container-' + id);
  var url = dpath(vfieldset, 'fieldset');

  container.html('');

  vfieldset.find('.multifield').each(function (i, multifield)
  {
    mod.initFieldset(container, function (fieldset)
    {
      fillFields($(multifield), fieldset);
    });
  });
};

// Initialize and fill normal fieldsets.
var fillNormalFieldsets = function (vfieldset)
{
  fillFields($(vfieldset));
};

// Fill the fields with values taken from the view pane.
var fillFields = function (container, context)
{
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').show();

  container.find('.field-view').each(function (i, field)
  {
    var valueJson = $(field).attr('data-field-value');
    var id = $(field).attr('data-field-field');
    var instance = $(field).attr('data-field-instance');
    var value;

    if (valueJson)
    {
      value = JSON.parse(valueJson);
    }

    if (!context)
    {
      context = $('body');
    }

    // TODO: There is still a mismatch in template systems and
    // conventions that means that I cannot simply set the values
    // directly. There are different rules for escaping, etc.
    setFieldValue(context.find('.field[data-field-field=' + id + ']'), value, instance);
  });
};

// Properly set the value of the field.
var setFieldValue = function (field, value, instance)
{
  if (field.is('input.boolean'))
  {
    field.prop('checked', value);
  }
  else if (value && field.is('select.open-boolean'))
  {
    field.val(value.toString());
  }
  else if (value && field.is('select.multiselect'))
  {
    value = value.map(function (x)
    {
      return encodeURIComponent(x).replace(/[!'()]/g, window.escape).replace(/\*/g, '%2A');
    });
    field.val(value);
  }
  else if (value && field.is('select.select'))
  {
    value = encodeURIComponent(value).replace(/[!'()]/g, window.escape).replace(/\*/g, '%2A');
    field.val(value);
  }
  else if (value && (field.is('input.text') || field.is('select.file')))
  {
    field.val(decodeURIComponent(value.replace(/\+/g, ' ')));
  }
  else
  {
    field.val(value);
  }

  field.attr('data-field-instance', instance);
};

// Exported functions

// Initialize a fieldset.
var initFieldset = function (fieldset, callback, addInstances)
{
  var url = dpath($(fieldset), 'fieldset').toString();
  var id = store($(fieldset)).fs('fieldset');
  var container = $('#container-' + id);
  var appendIt = function (data)
  {
    container.append(data);
    initFields(container, callback, addInstances);
  };
  var storeIt = function (data)
  {
    sessionStorage.setItem(url, data);
    appendIt(data);
  };

  ifStoredElse(url.toString(), appendIt, storeIt);

  return false;
};

// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets.
var fieldsetsToObject = function (root)
{
  var obj = {
    fieldsets: []
  };

  root.find('fieldset').each(function (i, fieldset)
  {
    fieldset = $(fieldset);
    var s = store(fieldset);

    var fields;

    var fsObj = {
      id: s.fs('fieldset'),
      multiple: s.fs('multiple') === 'true',
      collapse: s.fs('collapse') === 'true',
      name: s.fs('name'),
      label: s.fs('label'),
      order: s.fs('order') * 1
    };

    fields = fsContainer(fsObj.id).children('.fields');

    if (!fsObj.multiple)
    {
      $.extend(fsObj, fieldsToObject(fields.first()));
    }
    else
    {
      fsObj.multifields = [];

      fields.each(function (j, field)
      {
        field = $(field);

        fsObj.multifields[j] = fieldsToObject(field, j);
      });
    }

    obj.fieldsets[i] = fsObj;
  });

  return obj;
};

// Initialize fieldsets
var initFieldsets = function ()
{
  $('fieldset').each(function (i, fieldset)
  {
    var fs = store($(fieldset));

    if (fs.fs('multiple') === 'false')
    {
      mod.initFieldset(fieldset, false);
    }
  });

  return true;
};

// Remove a multifieldset. This is done after the remove button is
// pressed.
var removeFieldset = function (target)
{
  target.parent().remove();
};

// Fill the fieldset with values from the view pane.
var fillFieldsets = function ()
{
  $('.fieldset-view').each(function (i, fieldset)
  {
    if (store($(fieldset)).fs('multiple') === 'true')
    {
      fillMultiFieldsets(fieldset);
    }
    else
    {
      fillNormalFieldsets(fieldset);
    }
  });

  editui.afterEditRefresh();

  return true;
};

exports(initFieldset);
exports(fieldsetsToObject);
exports(initFieldsets);
exports(removeFieldset);
