shimi.fieldsets = (function ()
{
  'use strict';

  var mod = {};
  var store = shimi.store;
  var utils = shimi.utils();

  var fsContainer = function (id)
  {
    return $('#container-' + id);
  };

  var dpath = function (source, category)
  {
    var url = shimi.path(source, category);
    url.doctype = false;
    return url;
  };

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

      shimi.editui.afterFreshRefresh(addInstances);
    };
    var storeIt = function (data)
    {
      sessionStorage.setItem(url, data);
      prependIt(data);
    };

    ifStoredElse(url.toString(), prependIt, storeIt);

    return true;
  };

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

  var fillNormalFieldsets = function (vfieldset)
  {
    fillFields($(vfieldset));
  };

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

  mod.initFieldset = function (fieldset, callback, addInstances)
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
  mod.fieldsetsToObject = function (root)
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

  mod.initFieldsets = function ()
  {
    $('fieldset').each(function (i, fieldset)
    {
      var fs = store($(fieldset));

      if (fs.fs('multiple') === 'false')
      {
        mod.initFieldset(fieldset, false);
      }
    });

    return mod;
  };

  mod.removeFieldset = function (target)
  {
    target.parent().remove();
  };

  mod.fillFieldsets = function ()
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

    shimi.editui.afterEditRefresh();

    return mod;
  };

  return mod;
})();
