// # HTML Form Helpers
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// The are slightly specialized toward form elements using JQueryUI in
// some way.

// ## Variable Definitions

var flash = require('./flash.js');
var clear;

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

// Perform an Ajax action with a URL, object to be translated to JSON,
// an HTTP method, a function to be run on completion and the calling
// context.
var send = function (ajaxUrl, obj, method, completeFun, callContext)
{
  'use strict';

  var dataObj;

  if (obj)
  {
    dataObj = JSON.stringify(obj);
  }

  $.ajax(
  {
    type: method,
    url: ajaxUrl,
    dataType: 'json',
    context: callContext,
    contentType: 'application/json',
    processData: false,
    data: dataObj,
    complete: function (req, status)
    {
      if (req.status >= 200 && req.status < 300)
      {
        completeFun(this, req);
      }
      else if (req.status === 500)
      {
        flash.error('Unknown Server Error', 'Please report that you received ' + 'this message');
      }
      else if (req.status >= 400)
      {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;

        flash.error(title, body.fieldname + ' ' + body.message);
      }
    }
  });

  return true;
};

// ### Validation

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

// Client side validation of string length.
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

// Client side validation using a regex match.
var checkRegexp = function (o, regexp, n, tips)
{
  'use strict';

  if (!(regexp.test(o.val())))
  {
    o.addClass('ui-state-error');
    updateTips(n, tips);
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
exports.send = send;
exports.updateTips = updateTips;
exports.checkLength = checkLength;
exports.checkRegexp = checkRegexp;
exports.initDateFields = initDateFields;
exports.fillOptionsFromUrl = fillOptionsFromUrl;
