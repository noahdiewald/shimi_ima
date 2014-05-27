// # HTML Form Helpers
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Some form helpers.
// TODO: find non-JQueryUI implementations. Only the date picker needs
// JQuery or JQueryUI.

// ## Variable Definitions

var ajax = require('ajax');
var clear;

// ## Internal Functions

// Show a brief validation message.
var updateTips = function (t, tips) {
  'use strict';

  tips.insertAdjacentHTML('beforeend', '<span class="validation-error-message">' + t + '</span>');
  tips.classList.add('ui-state-highlight');
  setTimeout(function () {
    tips.classList.remove('ui-state-highlight', 1500);
  }, 500);

  return tips;
};

// ## Exported Functions

// Generic element toggler.
var toggle = function (target) {
  'use strict';

  var toggleElem = document.getElementById(target.dataset.target);

  toggleElem.classList.toggle('hidden');

  return target;
};

// Generic dialog canceling code
var cancelDialog = function (target) {
  'use strict';

  var toggleElem = document.getElementById(target.dataset.target);

  toggleElem.classList.add('hidden');
  clear(undefined, toggleElem.querySelector('form'));

  return target;
};

// Generic dialog form clearing code
clear = function (inputFields, form) {
  'use strict';

  if (inputFields === undefined) {
    inputFields = form.querySelectorAll('input, select, textarea');
  }

  Array.prototype.forEach.call(inputFields, function (elem) {
    if (!elem.dataset.retain) {
      if (elem.checked) {
        elem.checked = false;
      }
      elem.value = '';
    }
  });

  return inputFields;
};

// ### Validation

// Client side validation of string length.
//
// NOTE: Used only by [`projectui.js`](./projects/projectui.html)
var checkLength = function (o, n, min, max, tips) {
  'use strict';

  if (o.value.length > max || o.value.length < min) {
    o.addClass('ui-state-error');
    updateTips('Length of ' + n + ' must be between ' + min + ' and ' + max + '.', tips);
    return false;
  } else {
    return true;
  }
};

// ### Form element manipulation

// Init JqueryUI datepicker widgets
var initDateFields = function () {
  'use strict';

  $('input[type="date"]').datepicker({
    dateFormat: 'yy-mm-dd'
  });

  return true;
};

// Fill select options from a URL using Ajax
var fillOptionsFromUrl = function (url, selectElement, callback) {
  'use strict';

  ajax.get(url, function (req) {
    selectElement.innerHTML = templates['options'](req.response);
    if (callback) {
      callback();
    }
  });

  return selectElement;
};

exports.toggle = toggle;
exports.cancelDialog = cancelDialog;
exports.clear = clear;
exports.checkLength = checkLength;
exports.initDateFields = initDateFields;
exports.fillOptionsFromUrl = fillOptionsFromUrl;
