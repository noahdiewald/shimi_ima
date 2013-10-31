// # HTML Form Helpers
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// The are slightly specialized toward form elements using JQueryUI in
// some way.

// ## Variable Definitions

var clear;

// ## Internal Functions

// Show a brief validation message.
var updateTips = function (t, tips)
{
  'use strict';

  tips.append('<span class="validation-error-message">' + t + '</span>').addClass('ui-state-highlight');
  setTimeout(function ()
  {
    tips.removeClass('ui-state-highlight', 1500);
  }, 500);

  return true;
};

// ## Exported Functions

// Generic element toggler. The idea being that a clicked or otherwise
// 'stimulated' element has a `data-target` attribute with a value the
// ID of an element to be toggled.
var toggle = function (t)
{
  'use strict';

  var toggleElem;
  var target = $(t);

  if (target.attr('data-target'))
  {
    toggleElem = $('#' + target.attr('data-target'));
    toggleElem.toggle();
  }

  return true;
};

// Generic dialog canceling code
var cancelDialog = function (t)
{
  'use strict';

  var target = $(t);
  var toggleElem;
  var elemId;

  if (target.attr('data-target'))
  {
    elemId = '#' + target.attr('data-target');
    toggleElem = $(elemId);
    toggleElem.hide();
    clear(undefined, toggleElem.find('form'));
  }

  return true;
};

// Generic dialog form clearing code
clear = function (inputFields, form)
{
  'use strict';

  if (inputFields === undefined)
  {
    inputFields = $(form).find('input, select, textarea');
  }
  inputFields.each(function (index, elem)
  {
    var inputField = $(elem);

    if (!inputField.attr('data-retain'))
    {
      if (inputField.is(':checked'))
      {
        inputField.attr('checked', false);
      }
      inputField.val('');
    }
  });
  return inputFields;
};

// ### Validation

// Client side validation of string length.
//
// NOTE: Used only by [`projectui.js`](./projects/projectui.html)
var checkLength = function (o, n, min, max, tips)
{
  'use strict';

  if (o.val().length > max || o.val().length < min)
  {
    o.addClass('ui-state-error');
    updateTips('Length of ' + n + ' must be between ' + min + ' and ' + max + '.', tips);
    return false;
  }
  else
  {
    return true;
  }
};

// ### Form element manipulation

// Init JqueryUI datepicker widgets
var initDateFields = function ()
{
  'use strict';

  $('.date').datepicker(
  {
    dateFormat: 'yy-mm-dd'
  });

  return true;
};

// Fill select options from a URL using Ajax
var fillOptionsFromUrl = function (url, selectElement, callback)
{
  'use strict';

  $.get(url, function (options)
  {
    selectElement.html(options);
    if (callback)
    {
      callback();
    }
  });

  return false;
};

exports.toggle = toggle;
exports.cancelDialog = cancelDialog;
exports.clear = clear;
exports.checkLength = checkLength;
exports.initDateFields = initDateFields;
exports.fillOptionsFromUrl = fillOptionsFromUrl;
