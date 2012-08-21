/*
 h1. Application Wide Utility Functions
 
 The sections of this application are usually divided into separate
 files that do not share code. Much of the code that is used by more
 than one section and hasn't been moved to its own file can be found
 here.
 
 h2. Data Attribute Key Value Stores
 
 There are two functions provided for getting values from keys
 embedded in HTML elements.
  
  @getValue(key, elem)@
  
  This funtion takes a key that corresponds to the name of the data
  attribute without the data- prefix. It also takes a jQuery
  object. The assumption is that the jQuery object will hold only one
  element but it may work either way. The element is expected to have
  an attribute data-group-id with a value that is the id of the
  element actually holding the data.
 
 Example:

 <pre> 
   <div
     id="someid"
     data-fieldset-fieldset="fsid"
     data-fieldset-doctype="did"></div>
     
   <div
    id="thisid"
    data-group-id="someid">
    
   getValue("fieldset-doctype", $(thisid)) == "did";
 </pre> 
   
 The following also works:

 <pre> 
   <div
     id="someid2"
     data-fieldset-fieldset="fsid"
     data-fieldset-doctype="did"></div>
   
   <div
     id="someid"
     data-group-id="someid2"
     data-fieldset-fieldset="fsid"></div>
     
   <div
    id="thisid"
    data-group-id="someid"></div>
    
   getValue("fieldset-doctype", $(thisid)) == "did";
 </pre> 
   
  @putValue(key, value, elem)@
  
  This function will set an attribute at the target with a name
  corresponding to key and a value of value.

 == Click Dispatcher
 
 Each section of the application calls this function with an object
 composed of keys of CSS patterns of elements which should have click
 event actions bound to them and values of functions that will be
 called if a click event occurs and the key pattern matches.

  @dispatcher(patterns)@
  
  *More to come*
  
 == Compatability Functions
 
 @Object.keys@ and @array.reduce@ compatibility functions from MDC
 are included.
 
 == Helpers
 
  @isBlank(value)@
  
  This tests an object of several types for the concept of
  'blankness'. For a string this means only whitespace, for
  instance. See the code below for details. It is idiosyncratic but
  matches what I want, usually.
  
  @String.isBlank()@
  
  A more limited version of the above. It will probably be removed.
  
  @String.trim()@
  
  A string trim method.
  
  @stringToNumber(string)@
  
  Safer(ish) string to number. In this app I am using '' if the string
  isn't a valid number. It saves effort when putting values in form
  fields.
  
*/

function getValue(key, elem) {
  var getValue1 = function(key, elem, id) {
    var gid = elem.attr('data-group-id');
    var store = $('#' + gid);
    var val = store.attr('data-' + key);
    var next = store.attr('data-group-id');
    
    if (val === undefined &&  next !== undefined &&  gid !== next) {
      return getValue1.r(key, store, id);
    }
    
    return id.r(val);
  };
  
  return getValue1.t(key, elem, identity);
}

function putValue(key, value, elem) {
  var dataElem = elem.attr('data-group-id');
  $('#' + dataElem).attr('data-' + key, value);
}

// A predicate function to detect blankness
      
function isBlank(value) {
  return (((/^\s*$/).test(value)) || 
    (value === null) || 
    (value === undefined) ||
    (typeof value === 'number' && isNaN(value)) ||
    (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
};

// functions added to String

// Simple 'parser' for quoted values

String.prototype.parseQuoted = function(quoteChar) {
  var quoteChar = (quoteChar || "'");
  var outArray = [];
  var inArray = this.split('');
  var inQuote = false;
  var quoteCount = 0;
  var currCell = [];
  
  var onQuote = function() {
    if (inQuote && (quoteCount % 2 === 0)) {
      ++quoteCount;
    } else if (inQuote && (quoteCount % 2 === 1)) {
      ++quoteCount;
      currCell.push("'");
    } else if (!inQuote) {
      inQuote = true;
    }
  };
  
  var outQuote = function() {
    outArray.push(currCell.join(''));
    currCell = [];
    quoteCount = 0;
    inQuote = false;
  };
  
  inArray.forEach(function(item) {
    if (/'/.test(item)) {
      onQuote();
    } else if (quoteCount % 2 === 1) {
      outQuote();
    } else if (quoteCount % 2 === 0) {
      quoteCount = 0;
      if (inQuote) {
        currCell.push(item);
      } else if (/\S/.test(item)) {
        return false;
      }
    }
  });
  
  outArray.push(currCell.join(''));
  
  return outArray;
};

String.prototype.isBlank = function() {
  return ((/^\s*$/).test(this) && ! (/\S/).test(this) && ! (this === null));
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

// functions added to Array

Array.prototype.trimAll = function() {
  return this.map(function (i) {
                    return i.trim();
                  }).filter(function (i) {
                              return !i.match(/^$/);
                            });
};

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
    var inputField = $(this);
    
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

var flash = function(flasher, title, body) {
  var fadeout = function() {
    flasher.fadeOut();
  };
  flasher.find('.notification-summary').text(title + ": ");
  flasher.find('.notification-message').text(body);
  var timeout = setTimeout(fadeout, 7000);
  flasher.fadeIn();
  flasher.find('.close').click(function () {
                                     clearTimeout(timeout);
                                     flasher.hide();
                                   });
};

var flashError = function(title, body) {
  flash($('#notifications-main .ui-state-error'), title, body);
};

var flashHighlight = function(title, body) {
  flash($('#notifications-main .ui-state-highlight'), title, body);
};


var fillOptionsFromUrl = function(url, selectElement, callback) {
  $.get(url, function(options) {
    selectElement.html(options);
    if (callback) callback();
  });
  
  return false;
};

var validID = function(id) {
  return !!id.match(/^[a-f0-9]{32}$/);
};

// General UI Stuff

var panelToggle = function() {
  var toggler = function(e) {
    var panel;
    
    if ($(e.target).attr('data-panel')) {
      panel = $('#' + $(e.target).attr('data-panel'));
    } else {
      panel = $(e.target).closest('.panel');
    }
    panel.toggle();
  };

  $('#panel-toggle li')
    .live("click", function(e) {toggler(e);});
  $('.panel > h2')
    .live("dblclick", function(e) {toggler(e);});
};
 
$(function () {
    $('.notification').hide();
  
    $('#loading').hide()
      .ajaxStart(function() {
                   $(this).show();
                 })
      .ajaxStop(function() {
                  $(this).hide();
                });

    panelToggle();    
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