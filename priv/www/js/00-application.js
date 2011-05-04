// Helpers for treating HTML elements like key value storage

// Tail call optimization

Function.prototype.r = function() {return [this, arguments]};

Function.prototype.t = function() {
  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape) {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
}

var identity = function(x) {return x}

// Functions to treat html elements like key value stores
// TODO may have been better to call it getValue

function getData(key, elem) {
  var getData1 = function(key, curr, id) {
    var dataElem = curr.attr('data-group-id');
    var store = $('#' + dataElem);
    var value = store.attr('data-' + key);
    var nextElem = store.attr('data-group-id');
    
    if (value === undefined && 
        nextElem !== undefined && 
        dataElem !== nextElem) {
      return getData1.r(key, store, id);
    }
    
    return id.r(value);
  };
  
  return getData1.t(key, elem, identity);
}

function putData(key, value, elem) {
  var dataElem = elem.attr('data-group-id');
  $('#' + dataElem).attr('data-' + key, value);
}

// Object.keys compatibility from MDC

if(!Object.keys) Object.keys = function(o){
  if (o !== Object(o))
    throw new TypeError('Object.keys called on non-object');
  var ret=[],p;
  for(p in o) if(Object.prototype.hasOwnProperty.call(o,p)) ret.push(p);
  return ret;
}

// Reduce compatibility from MDC

if (!Array.prototype.reduce)
{
  Array.prototype.reduce = function(fun /*, initialValue */)
  {
    "use strict";

    if (this === void 0 || this === null)
      throw new TypeError();

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function")
      throw new TypeError();

    // no value to return if no initial value and an empty array
    if (len == 0 && arguments.length == 1)
      throw new TypeError();

    var k = 0;
    var accumulator;
    if (arguments.length >= 2)
    {
      accumulator = arguments[1];
    }
    else
    {
      do
      {
        if (k in t)
        {
          accumulator = t[k++];
          break;
        }

        // if array contains no values, no initial value to return
        if (++k >= len)
          throw new TypeError();
      }
      while (true);
    }

    while (k < len)
    {
      if (k in t)
        accumulator = fun.call(undefined, accumulator, t[k], k, t);
      k++;
    }

    return accumulator;
  };
}
// A predicate function to detect blank strings
      
function isBlank(value) {
  return (((/^\s*$/).test(value)) || 
    (value === null) || 
    (value === undefined) ||
    (typeof value === 'number' && isNaN(value)) ||
    (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
};

// functions added to String

String.prototype.isBlank = function() {
  return ((/^\s*$/).test(this) && ! (/\S/).test(this) && ! (this == null));
};

String.prototype.trim = function() {
  return this.replace(/^\s+/,'').replace(/\s+$/,'');
};

// safer(ish) string to number. The difference is that in this app
// I am using '' if the string isn't a valid number.

function stringToNumber(string) {
  if (typeof string === 'string' && 
      !isNaN(string) && 
      string !== '') {
    return string * 1;
  } else {
    return '';
  }
}

// Event dispatch

function dispatcher(patterns) {
  var d = function(e) {
    var target = $(e.target);
    
    Object.keys(patterns).forEach(function(pattern) {
      if (target.is(pattern)) {
        var action = patterns[pattern];
        action(target);
      }
    });  
  };
  
  return d;
}

// Dialog form helpers

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
      if (req.status >= 200 && req.status < 300) {
        completeFun(this, req);
      } else if (req.status >= 400 && req.status < 500) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
        
        flashError(title, body.fieldname + " " + body.message);
      }
    }
  });
}

// Validation
  
function updateTips(t, tips) {
  tips.text(t).addClass('ui-state-highlight');
  setTimeout(function() {
    tips.removeClass('ui-state-highlight', 1500);
  }, 500);
}

function checkLength(o, n, min, max, tips) {
  if ( o.val().length > max || o.val().length < min ) {
    o.addClass('ui-state-error');
    updateTips("Length of " + n + " must be between " + min + " and " + max + ".", tips);
    return false;
  } else {
    return true;
  }
}

function checkRegexp(o, regexp, n, tips) {
  if ( !( regexp.test( o.val() ) ) ) {
    o.addClass('ui-state-error');
    updateTips(n, tips);
    return false;
  } else {
    return true;
  }
}

// Date Picker

function initDateFields() {
  $(".date").datepicker({dateFormat: "yy-mm-dd"});
}

// Display notifications

function flashError(title, body) {
  $('#notifications-main .ui-state-error .notification-summary').text(title + ": ");
  $('#notifications-main .ui-state-error .notification-message').text(body);
  $('#notifications-main .ui-state-error').fadeIn().delay(7000).fadeOut('slow');
}

function flashHighlight(title, body) {
  $('#notifications-main .ui-state-highlight .notification-summary').text(title + ": ");
  $('#notifications-main .ui-state-highlight .notification-message').text(body);
  $('#notifications-main .ui-state-highlight').fadeIn('slow').delay(7000).fadeOut('slow');
}
 
$(function () {
  $('.notification').hide();
  
  $('#loading').hide().ajaxStart(function() {
    $(this).show();
  }).ajaxStop(function() {
    $(this).hide();
  });
  
  // Buttons
  
  $(".remove-button").button({
    icons: {primary: "ui-icon-minus"},
    text: false
  }).click(function() {
    $(this).parent().remove();
  });
  
  $(".help-button").button({
    icons: {primary: "ui-icon-help"},
    text: false
  });
  
  $(".link-button").button({
    icons: {primary: "ui-icon-link"}
  });
  
  $(".edit-button").button({
    icons: {primary: "ui-icon-pencil"}
  });
  
  $(".create-continue-button").button({
    icons: {
      primary: "ui-icon-disk",
      secondary: "ui-icon-arrowthick-1-e"
    }
  });
  
  initDateFields();
  
  // Sortable
  
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      lis.forEach(function(li) {$(li).text($(li).text() + " " + x);});
    }
  });
});