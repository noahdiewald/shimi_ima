// # Working with elements of a field manipulation HTML form
//
// *Implicit depends:* DOM, JQuery

// Variable Definitions

var form = require('../form.js');
var utils = require('../utils.js');

// Exported functions

// Returns an object with references to add/edit fields dialog
// field elements with helper functions.
var fieldElems = (function ()
{
  'use strict';

  var mod = {};

  mod.attrs = ['name', 'label', 'order', 'description', 'subcategory', 'head', 'reversal', 'default', 'required', 'allowed', 'source', 'max', 'min', 'regex', 'doctype', 'fieldset', 'charseq', 'rev', 'field'];

  mod.get = function (values)
  {
    var fObj = {};

    fObj.attrs = mod.attrs;

    // These are fields that only some field subcategories use.
    // Below you'll see them being disabled and reenabled depending on the
    // chosen subcategory.
    fObj.notDefault = function ()
    {
      return [fObj.charseq, fObj.allowed, fObj.source, fObj.min, fObj.max, fObj.regex];
    };

    fObj.disable = function ()
    {
      fObj.notDefault().forEach(function (field)
      {
        field.attr('disabled', 'disabled');
      });
      return fObj;
    };

    fObj.clearDisabled = function ()
    {
      fObj.notDefault().forEach(function (field)
      {
        if (field.attr('disabled'))
        {
          field.val('');
        }
      });
      return fObj;
    };

    fObj.copyValues = function (source)
    {
      Object.keys(source).forEach(function (field)
      {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]'))
        {
          if (source[field] === 'true')
          {
            fObj[field].prop('checked', true);
          }
        }
      });
      return fObj;
    };

    fObj.getFieldInputVals = function ()
    {
      var valObj = {
        'category': 'field',
        'name': fObj.name.val(),
        'label': fObj.label.val(),
        'default': fObj.decodeDefaults(fObj.subcategory.val(), fObj['default'].val()),
        'head': fObj.head.is(':checked'),
        'reversal': fObj.reversal.is(':checked'),
        'required': fObj.required.is(':checked'),
        'order': fObj.order.val() * 1,
        'allowed': fObj.allowed.val().split(',').trimAll(),
        'source': fObj.decodeSource(fObj.subcategory.val(), fObj.source.val()),
        'min': fObj.decodeBound(fObj.subcategory.val(), fObj.min.val()),
        'max': fObj.decodeBound(fObj.subcategory.val(), fObj.max.val()),
        'regex': fObj.regex.val(),
        'description': fObj.description.val(),
        'charseq': fObj.charseq.val(),
        'doctype': fObj.doctype.val(),
        'fieldset': fObj.fieldset.val(),
        'subcategory': fObj.subcategory.val()
      };
      return valObj;
    };

    fObj.clear = function ()
    {
      form.clear($('#field-dialog .input')).removeClass('ui-state-error');
      fObj.disable();
      return fObj;
    };

    fObj.decodeBound = function (subcategory, bound)
    {
      if (subcategory === 'date')
      {
        return bound;
      }
      else
      {
        return utils.stringToNumber(bound);
      }
    };

    fObj.decodeSource = function (subcategory, source)
    {
      if (subcategory === 'file')
      {
        return source.split('/').trimAll();
      }
      else
      {
        return source;
      }
    };

    fObj.decodeDefaults = function (subcategory, defaults)
    {
      switch (subcategory)
      {
      case 'docmultiselect':
      case 'multiselect':
        return defaults.split(',').trimAll();
      case 'file':
        return defaults.split('/').trimAll();
      default:
        return defaults;
      }
    };

    fObj.displayFields = function (subcategory)
    {
      switch (subcategory)
      {
      case 'select':
      case 'multiselect':
        fObj.disable();
        fObj.allowed.removeAttr('disabled');
        break;
      case 'docselect':
      case 'docmultiselect':
      case 'file':
        fObj.disable();
        fObj.source.removeAttr('disabled');
        break;
      case 'text':
      case 'textarea':
        fObj.disable();
        fObj.charseq.removeAttr('disabled');
        fObj.regex.removeAttr('disabled');
        break;
      case 'date':
      case 'integer':
      case 'rational':
        fObj.disable();
        fObj.min.removeAttr('disabled');
        fObj.max.removeAttr('disabled');
        break;
      default:
        fObj.disable();
      }
    };

    fObj.attrs.forEach(function (item)
    {
      fObj[item] = $('#field-' + item + '-input');
    });

    fObj.copyValues(values);
    fObj.displayFields(fObj.subcategory.val());

    fObj.subcategory.change(function ()
    {
      fObj.displayFields(fObj.subcategory.val());
    });

    return fObj;
  };

  return mod;
})();

exports.fieldElems = fieldElems;
