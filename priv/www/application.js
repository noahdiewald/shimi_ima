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
  var dataObj;

  if (obj) {
    dataObj = JSON.stringify(obj);
  }

  $.ajax({
    type: method,
    url: ajaxUrl,
    dataType: "json",
    context: callContext,
    contentType: "application/json",
    processData: false,
    data: dataObj,
    complete: function(req, status) {
      if (req.status >= 200 && req.status < 300) {
        completeFun(this, req);
      } else if (req.status == 500) {
        flashError("Unknown Server Error", "Please report that you received " +
                   "this message");
      } else if (req.status >= 400) {
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

var uiToggle = function() {
  var toggler = function(e) {
    var toggleElem;

    if ($(e.target).attr('data-target')) {
      toggleElem = $('#' + $(e.target).attr('data-target'));
      toggleElem.toggle();
    }
  };

  $('.toggler').live("click", function(e) {toggler(e);});
};

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
    uiToggle();    
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
/*
 * h1. Path helper
 * 
 * h2. Creating the object
 * 
 * This function returns an object with various helpers for URL
 * path operations. In this application a common pattern in paths is
 * doctypes/<doctypeid>/fieldsets/<fiedsetid>/fields/<fieldid>. The path
 * function below will take a source, which is a jQuery object, such as
 * $('#some-id'), which has an attribute named 'data-group-id' having a
 * value of the id of an element that stores data relevant to the current
 * context as HTML data attributes, in particular the ids of doctypes,
 * fieldsets and/or fields. The category is one of 'field', 'fieldset'
 * or 'doctype'. The section argument is a section of the application,
 * such as 'config' that will be prefixed to the path.
 * 
 * Example:
 *
 * <pre> 
 *   <div
 *     id="someid"
 *     data-fieldset-fieldset="fsid"
 *     data-fieldset-doctype="did"></div>
 *     
 *   <div
 *    id="thisid"
 *    data-group-id="someid">
 *     
 *   mypath = path($('#thisid'), "fieldset");
 *   mypath.toString() == "doctypes/did/fieldsets/fsid";
 *   
 *   mypath = path($('#thisid'), "fieldset", "config");
 *   mypath.toString() == "config/doctypes/did/fieldsets/fsid";
 *   
 *   mypath = path($('#thisid'), "fieldset");
 *   mypath.fieldset = false; // unsets the fielset id
 *   mypath.toString() == "doctypes/did/fieldsets"; // all fieldsets
 * </pre> 
 *   
 * Note that the category matches the x of data-x in someid. Different
 * values may be held for doctype or field in the same element. Sometimes
 * this leads to repetition of information and a better scheme may be
 * forthcoming. The positive side is that information about different
 * paths may be held in the same location.
 *
 * h3. CouchDB Revision Numbers
 * 
 * Above, a revision could have been added to someid as 'data-fieldset-rev'.
 * 
 * h3. More Information
 * 
 * For more information on how data attributes are used in this application,
 * see getValue in the application.js file.
 * 
 * h2. Manipulating the object
 * 
 * Also note that setting certain path elements to false (or undefined)
 * will exclude their ids from the end result. Setting the element to a
 * different id would cause the path to be altered appropriately. This
 * allows one to cleanly manipulate the paths without performing string
 * manipulation.
 * 
 * h2. PUT, POST and DELETE using the object
 * 
 * There are also helpers for using the path the work with the resource it points to.
 * 
 *  Example:
 * 
 * <pre> 
 *   mypath = path($('#thisid'), "fieldset");
 *   mypath.put(object, callback, context);
 *   mypath.post(object, callback, context);
 *   mypath.delete(callback, context);
 * </pre>
 *   
 * Object is an Javascript object that can be encoded as JSON, callback
 * will be run on success and context provides information the environment
 * from which the method was called, usually 'this' is supplied. (Context
 * may no longer be an option in the future).
 * 
 * The object will be sent to the path that would be returned by the
 * toString method using the method implied by the above method's names.
 * 
 * h3. Error handlers
 * 
 * Within the context of this application it is assumed that fairly standard
 * things will be done with error responces so they are left alone.
*/

function path(source, category, section) {
  var path = {};
  
  if (category) {
    var prefix = category + "-";
  } else {
    var prefix = "";
  }
  
  if (section) {
    path.string = section + "/";
  } else {
    path.string = "";
  }
  
  path.category = category;
  path.origin = source;
  path.type = prefix + "path";
  path.valid_components = ["doctype", "fieldset", "field"];
  
  path.valid_components.forEach(
    function(item) {
      path[item] = (function() {
                      var value = getValue(prefix + item, path.origin);
                      return value;
                    })();
    });
  
  path.rev = getValue(prefix + 'rev', path.origin);

  path.doctype = getValue(prefix + 'doctype', path.origin);
  
  path.send = function(object, method, callback, context) {
    sendConfigDoc(path.toString(), object, method, callback, context);
    return path;
  };
  
  path.put = function(object, callback, context) {
    path.send(object, 'PUT', callback, context);
    return path;
  };
    
  path.post = function(object, callback, context) {
    path.send(object, 'POST', callback, context);
    return path;
  };
  
  path.delete = function(callback, context) {
    path.send({}, 'DELETE', callback, context);
    return path;
  };
    
  path.toString = function() {
    var rev;
      
    var pathString = 
      path.string.concat(
        path.valid_components.map(
          function(item) {
            var plural = item + "s";
            var value = path[item];
            var retval = null;
            
            if (value) {
              retval = plural + "/" + value;
            } else if (item == path.category) {
              retval = plural;
            }
            
            return retval;
          }).filter(
            function(item) {
              return (typeof item == "string" && !item.isBlank());
            }).join("/"));
      
    if (path.rev) {
      pathString = pathString.concat("?rev=" + path.rev);
    }
    
    return pathString;
  };
  
  return path; 
}


/*
 * jQuery Hotkeys Plugin
 * Copyright 2010, John Resig
 * Modified by Noah Diewald
 * Dual licensed under the MIT or GPL Version 2 licenses.
 *
 * Based upon the plugin by Tzury Bar Yochay:
 * http://github.com/tzuryby/hotkeys
 *
 * Original idea by:
 * Binny V A, http://www.openjs.com/scripts/events/keyboard_shortcuts/
*/

(function(jQuery){	
   jQuery.hotkeys = {
     version: "0.8",
     
     specialKeys: {
       8: "backspace", 9: "tab", 13: "return", 16: "shift", 17: "ctrl", 18: "alt", 19: "pause",
       20: "capslock", 27: "esc", 32: "space", 33: "pageup", 34: "pagedown", 35: "end", 36: "home",
       37: "left", 38: "up", 39: "right", 40: "down", 45: "insert", 46: "del", 
       96: "0", 97: "1", 98: "2", 99: "3", 100: "4", 101: "5", 102: "6", 103: "7",
       104: "8", 105: "9", 106: "*", 107: "+", 109: "-", 110: ".", 111 : "/", 
       112: "f1", 113: "f2", 114: "f3", 115: "f4", 116: "f5", 117: "f6", 118: "f7", 119: "f8", 
       120: "f9", 121: "f10", 122: "f11", 123: "f12", 144: "numlock", 145: "scroll", 191: "/", 224: "meta"
     },
	
     shiftNums: {
       "`": "~", "1": "!", "2": "@", "3": "#", "4": "$", "5": "%", "6": "^", "7": "&", 
       "8": "*", "9": "(", "0": ")", "-": "_", "=": "+", ";": ": ", "'": "\"", ",": "<", 
       ".": ">",  "/": "?",  "\\": "|"
     }
   };

   function keyHandler( handleObj ) {
     // Only care when a possible input has been specified
     if ( typeof handleObj.data !== "string" ) {
       return;
     }
		
     var origHandler = handleObj.handler,
     keys = handleObj.data.toLowerCase().split(" ");
     
     handleObj.handler = function( event ) {
       // Don't fire in text-accepting inputs that we didn't directly bind to
       // MODIFIED FROM ORIGINAL
       //if ( this !== event.target && (/textarea|select/i.test( event.target.nodeName ) ||
       //      event.target.type === "text") ) {
       //	return;
       //}
       
       // Keypress represents characters, not special keys
       var special = event.type !== "keypress" && jQuery.hotkeys.specialKeys[ event.which ],
       character = String.fromCharCode( event.which ).toLowerCase(),
       key, modif = "", possible = {};

       // check combinations (alt|ctrl|shift+anything)
       if ( event.altKey && special !== "alt" ) {
         modif += "alt+";
       }
       
       if ( event.ctrlKey && special !== "ctrl" ) {
         modif += "ctrl+";
       }
			
       // TODO: Need to make sure this works consistently across platforms
       if ( event.metaKey && !event.ctrlKey && special !== "meta" ) {
         modif += "meta+";
       }

       if ( event.shiftKey && special !== "shift" ) {
         modif += "shift+";
       }

       if ( special ) {
         possible[ modif + special ] = true;
         
       } else {
         possible[ modif + character ] = true;
         possible[ modif + jQuery.hotkeys.shiftNums[ character ] ] = true;

         // "$" can be triggered as "Shift+4" or "Shift+$" or just "$"
         if ( modif === "shift+" ) {
           possible[ jQuery.hotkeys.shiftNums[ character ] ] = true;
         }
       }

       for ( var i = 0, l = keys.length; i < l; i++ ) {
         if ( possible[ keys[i] ] ) {
           return origHandler.apply( this, arguments );
         }
       }
     };
   }

   jQuery.each([ "keydown", "keyup", "keypress" ], function() {
                 jQuery.event.special[ this ] = { add: keyHandler };
               });
   
 })( jQuery );
/*
 Simple plugin for manipulating input.
*/

(function( $ ){

  $.fn.inputDisable = function() {
    this.val('');
    this.attr('disabled', 'disabled');
    this.addClass('ui-state-disabled');
    return this;
  };

  $.fn.inputEnable = function() {
    this.removeAttr('disabled');
    this.removeClass('ui-state-disabled');
    return this;
  };
  
})( jQuery );

/*
 Tail call optimization taken from Spencer Tipping's Javascript in Ten
 Minutes.

 For more information see:
 https://github.com/spencertipping/js-in-ten-minutes
*/

var identity = function(x) {return x};

Function.prototype.r = function() {return [this, arguments];};

Function.prototype.t = function() {
  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape) {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
};

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

// Dialog for manipulating doctypes

function charseqDialog(values) {
  var f = charseqElems().get(values);
  
  var dialog = $("#charseq-dialog").dialog({
    width: 650,
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function(context) {
          populateCharseqTabs();
          $(context).dialog("close");
        };
        
        if (values.rev && (!values.rev.isBlank())) {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }
        
        sendConfigDoc(url, obj, method, complete, this);
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  return dialog;
}

/*
 * Copyright 2011 University of Wisconsin Madison Board of Regents.
 * 
 * This file is part of dictionary_maker.
 * 
 * dictionary_maker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 * 
 * dictionary_maker is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.
 * 
 * Author Noah Diewald <noah@diewald.me>
 * 
 * About this file
 * 
 * A charaseq is a collection of information used in definining properies
 * of a script, including some phonological information and information
 * used for collation of items written in the script.
*/

var charseqElems = function() {
  var cElems = {};
  
  cElems.attrs = ["description", "characters", "name", "sort_ignore", "locale", "tailoring", "vowels", "consonants", "ietf_tag", "iso639_tag", "charseq", "rev"];
  
  cElems.get = function(values) {
    var cObj = {};
    
    cObj.attrs = cElems.attrs;
    
    cObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        cObj[field].val(source[field]);
      });
      return cObj;
    };
    
    cObj.getCharseqInputVals = function() {
      var valObj = {
        "category": "charseq",
        "description": cObj.description.val(),
        "characters": cObj.parse(cObj.characters.val()),
        "name": cObj.name.val(),
        "sort_ignore": cObj.parse(cObj.sort_ignore.val()),
        "locale": cObj.locale.val(),
        "tailoring": cObj.tailoring.val(),
        "vowels": cObj.parse(cObj.vowels.val()),
        "consonants": cObj.parse(cObj.consonants.val()),
        "ietf_tag": cObj.ietf_tag.val(),
        "iso639_tag": cObj.iso639_tag.val(),
        "_id": (cObj.charseq.val() || undefined),
        "rev": (cObj.rev.val() || undefined)
      }
      return valObj;
    };
    
    cObj.parse = function(val) {
      if (val && !val.isBlank()) {
        return JSON.parse(val);
      } else {
        return [];
      }
    };
    
    cObj.clear = function() {
      clearValues($('#charseq-dialog .input')).removeClass('ui-state-error');
      return cObj;
    };
                   
    cObj.attrs.forEach(function(item) {
      cObj[item] = $('#charseq-' + item + '-input');
    });
    
    cObj.copyValues(values);
      
    return cObj;
  };
  
  return cElems;
};
// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".edit-field-button span": function(t) {editFieldButton(t.parent('a'))},
    ".delete-field-button span": function(t) {deleteFieldButton(t.parent('a'))},
    ".add-field-button span": function(t) {addFieldButton(t.parent('a'))},
    ".edit-fieldset-button span": function(t) {editFieldsetButton(t.parent('a'))},
    ".delete-fieldset-button span": function(t) {deleteFieldsetButton(t.parent('a'))},
    ".add-fieldset-button span": function(t) {addFieldsetButton(t.parent('a'))},
    ".delete-doctype-button span": function(t) {deleteDoctypeButton(t.parent('a'))},
    ".edit-doctype-button span": function(t) {editDoctypeButton(t.parent('a'))},
    ".touch-doctype-button span": function(t) {touchDoctypeButton(t.parent('a'))},
    "#doctype-add-button span": function(t) {addDoctypeButton(t.parent('a'))},
    ".delete-charseq-button span": function(t) {deleteCharseqButton(t.parent('a'))},
    ".edit-charseq-button span": function(t) {editCharseqButton(t.parent('a'))},
    "#charseq-add-button span": function(t) {addCharseqButton(t.parent('a'))},
    "#maintenance-upgrade-button span": function(t) {upgradeButton(t.parent('a'))}
  });

  action(e);
}


// Button that opens a dialog for adding a charsec

var addCharsec = function(button) {
  $("#charseq-add-dialog").dialog("open");
};

// Button that opens a dialog for adding a doctype

var addDoctype = function(target) {
  initDoctypeAddDialog().dialog("open");
};

// Button that opens a dialog for editing a field

var editFieldButton = function(target) {
  var url = cpath(target, "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
  var charseqUrl = "config/charseqs?as=options";
  
  $.get(charseqUrl, function(charseqs) {
    $("#field-charseq-input").html(charseqs);
    attrs.forEach(function(item) {
      oldobj[item] = getValue('field-' + item, target);
    });
    fieldDialog(url, oldobj).dialog("open");
  });
};

// Button that opens a dialog for deleting a field

var deleteFieldButton = function(target) {
  var answer = confirm("Are you sure? This is permanent.");
  
  if (answer) {
    var url = cpath(target, "field");
    var complete = function() {
      url.field = false;
      url.rev = false;
      
      populateFields(url);
    };
    url.delete(complete, this);
  }
};

// Button that opens a dialog for adding a field

var addFieldButton = function(target) {
  var url = cpath(target, "field");
  var charseqUrl = "config/charseqs?as=options";
  
  $.get(charseqUrl, function(charseqs) {
    $("#field-charseq-input").html(charseqs);
    fieldDialog(url, {fieldset: url.fieldset, doctype: url.doctype}).dialog("open");
  });
};

// Button that opens a dialog for editing a fieldset

var editFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  var oldobj = {};
  var attrs = fieldsetElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('fieldset-' + item, target);
  });
  
  fieldsetDialog(url, oldobj).dialog("open");
};

// Button that opens a dialog for deleting a fieldset

var deleteFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  
  var complete = function() {
    url.fieldset = false;
    url.rev = false;
    populateFieldsets(url);
  };
    
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
};

// Button that opens a dialog for adding a fieldset

var addFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  fieldsetDialog(url, {doctype: url.doctype}).dialog("open");
};

var editCharseqButton = function(target) {
  var oldobj = {};
  var attrs = charseqElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('charseq-' + item, target);
  });
  charseqDialog(oldobj).dialog("open");
};

var deleteCharseqButton = function(target) {
  var id = getValue('charseq-charseq', target);
  var rev = getValue('charseq-rev', target);
  var url = 'config/charseqs/' + id + '?rev=' + rev;
  var complete = function() {
    populateCharseqTabs();
  };
  
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
};

var editDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  var oldobj = {};
  var attrs = doctypeElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('doctype-' + item, target);
  });
  doctypeDialog(url, oldobj).dialog("open");
};

var touchDoctypeButton = function(target) {
  var docid = getValue("doctype-doctype", target);
  $.post("config/doctypes/" + docid + "/touch");
  alert("Touch In Progress");
};

var upgradeButton = function(target) {
  $.post("config/upgrade");
  alert("Upgrade In Progress");
};

var deleteDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  var complete = function() {
    url.doctype = false;
    url.rev = false;
    populateDoctypeTabs();
  };
  
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
};

var addDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  doctypeDialog(url, {}).dialog("open");
};

var addCharseqButton = function(target) {
  charseqDialog({}).dialog("open");
};


// hide common options for path

function cpath(source, category) {
  return path(source, category, "config");
}

// Populate the listing of fields

function populateFields(path) {
  path.field = false;
  
  $.get(path.toString(), function(fields) {
    var fieldContainer = $("#fields-" + path.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
    $('.link-button').button();
  });
}

// Populate the listing of fieldsets

function populateFieldsets(url) {
  $.get(url.toString(), function(fieldsets) {
    var fieldsetContainer = $("#fieldsets-" + url.doctype);
    
    fieldsetContainer.empty();
    fieldsetContainer.accordion("destroy");
    fieldsetContainer.html(fieldsets);
    $('.link-button').button();
    
    fieldsetContainer.accordion({
      autoHeight: false,
      collapsible: true,
      active: false
    });
  });
}

// populate the tabs listing the doctypes

function populateDoctypeTabs() {
  var url = "config/doctypes";
  
  $.get(url, function(doctypes) {
    var fieldsetDoctype = $("#fieldset-doctype-input");
    
    $("#doctype-tabs-headings").empty();
    $("#doctype-tabs-headings + .ui-tabs-panel").remove();
    $("#doctype-tabs").tabs("destroy");
    $("#doctype-tabs-headings").html(doctypes);
    $('.link-button').button();
    
    var loadFun = function(event, ui) {
      var source = $(ui.panel).children('div[data-fieldset-doctype]');
      var fieldsetsPath = path(source, "fieldset", "config");
      populateFieldsets(fieldsetsPath);
    };
    
    $("#doctype-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
  });
}

// populate the tabs listing the charseqs

function populateCharseqTabs() {
  var url = "config/charseqs";
  
  $.get(url, function(charseqs) {
    $("#charseq-tabs-headings").empty();
    $("#charseq-tabs-headings + .ui-tabs-panel").remove();
    $("#charseq-tabs").tabs("destroy");
    $("#charseq-tabs-headings").html(charseqs);
    
    var loadFun = function(event, ui) {
      $('.link-button').button();
    };
    
    $("#charseq-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
  });
}

// Initialize the tabs on the config page

function initTabs() {
  $("#doctype-tabs").tabs();
  populateDoctypeTabs();
  $("#main-tabs").tabs();
  $("#charseq-tabs").tabs();
  populateCharseqTabs();
  
  return true;
}

// Hide the help text and set toggle on click events
// TODO use the click dispatcher

function initHelpText() {
  $("#doctype-info").hide();
  $("#charseq-info").hide();

  $("#doctype-info-toggle").click(function() {
    $("#doctype-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#charseq-info-toggle").click(function() {
    $("#charseq-info").toggle("blind", {}, 500);
    return false;
  });
  
  return true;
}

// Code to be run on page load

$(function () {
  $('body').click(function(e) {clickDispatch(e)});
  initTabs(); 
  initHelpText();
  $('.link-button').button();
  $('.simple-tabs').tabs();
});
// Dialog for manipulating doctypes

function doctypeDialog(url, values) {
  var f = doctypeElems().get(values);
  
  if (values.rev && !values.rev.isBlank()) {
    f.doctype.attr('disabled', 'disabled');
  }  
  
  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getDoctypeInputVals();
        var complete = function(context) {
          populateDoctypeTabs();
          $(context).dialog("close");
        };
        
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.doctype;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  return dialog;
}

// Returns an object with references to add/edit doctype dialog
// field elements with helper functions. 

function doctypeElems() {
  var fElems = {};
  
  fElems.attrs = ["description", "doctype", "rev"];
               
  fElems.get = function(values) {
    var fObj = {};
    
    fObj.attrs = fElems.attrs;
    
    fObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        fObj[field].val(source[field]);
      });
      return fObj;
    };
    
    fObj.getDoctypeInputVals = function() {
      var valObj = {
        "category": "doctype",
        "description": fObj.description.val(),
        "_id": fObj.doctype.val()
      };
      return valObj;
    };
    
    fObj.clear = function() {
      clearValues($('#doctype-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };
                   
    fObj.attrs.forEach(function(item) {
      fObj[item] = $('#doctype-' + item + '-input');
    });
    
    fObj.copyValues(values);
      
    return fObj;
  };
  
  return fElems;
}


// Dialog for manipulating fields

function fieldDialog(url, values) {
  var f = fieldElems().get(values);
  
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.clearDisabled().getFieldInputVals();
        var complete = function(context) {
          populateFields(url);
          $(context).dialog("close");
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  return dialog;
}

// Returns an object with references to add/edit fields dialog
// field elements with helper functions. 

function fieldElems() {
  var fElems = {};
  
  fElems.attrs = ["name", "label", "order", "description", "subcategory", 
                  "head", "reversal", "default", "required", "allowed", 
                  "source", "max", "min", "regex", "doctype", "fieldset",
                  "charseq", "rev", "field"];
               
  fElems.get = function(values) {
    var fObj = {};
    
    fObj.attrs = fElems.attrs;
    
    // These are fields that only some field subcategories use.
    // Below you'll see them being disabled and reenabled depending on the
    // chosen subcategory.
    fObj.notDefault = function() {
      return [fObj.charseq, fObj.allowed, fObj.source, fObj.min, fObj.max, fObj.regex];
    };
    
    fObj.disable = function() {
      fObj.notDefault().forEach(function(field) {
        field.attr("disabled", "disabled");
      });
      return fObj;
    };
    
    fObj.clearDisabled = function() {
      fObj.notDefault().forEach(function(field) {
        if (field.attr("disabled")) field.val("");
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
        "allowed": fObj.allowed.val().split(",").trimAll(),
        "source": fObj.decodeSource(fObj.subcategory.val(), fObj.source.val()),
        "min": fObj.decodeBound(fObj.subcategory.val(), fObj.min.val()),
        "max": fObj.decodeBound(fObj.subcategory.val(), fObj.max.val()),
        "regex": fObj.regex.val(),
        "description": fObj.description.val(),
        "charseq": fObj.charseq.val(),
        "doctype": fObj.doctype.val(),
        "fieldset": fObj.fieldset.val(),
        "subcategory": fObj.subcategory.val()
      };
      return valObj;
    };
    
    fObj.clear = function() {
      clearValues($('#field-dialog .input')).removeClass('ui-state-error');
      fObj.disable();
      return fObj;
    };
    
    fObj.decodeBound = function(subcategory, bound) {
      switch (subcategory) {
        case "date":
          return bound;
        default:
          return stringToNumber(fObj.min.val());
      }
    };
    
    fObj.decodeSource = function(subcategory, source) {
      switch (subcategory) {
        case "file":
          return source.split("/").trimAll();
        default:
          return source;
      }
    };
    
    fObj.decodeDefaults = function(subcategory, defaults) {
      switch (subcategory) {
        case "docmultiselect":
        case "multiselect":
          return defaults.split(",").trimAll();
        case "file":
          return defaults.split("/").trimAll();
        default:
          return defaults;
      }
    };
    
    fObj.displayFields = function(subcategory) {
      switch (subcategory) {
        case "select":
        case "multiselect":
          fObj.disable();
          fObj.allowed.removeAttr("disabled");
          break;
        case "docselect":
        case "docmultiselect":
        case "file":
          fObj.disable();
          fObj.source.removeAttr("disabled");
          break;
        case "text":
        case "textarea":
          fObj.disable();
          fObj.charseq.removeAttr("disabled");
          fObj.regex.removeAttr("disabled");
          break;
        case "date":
        case "integer":
        case "rational":
          fObj.disable();
          fObj.min.removeAttr("disabled");
          fObj.max.removeAttr("disabled");
          break;
        default:
          fObj.disable();
      }
    };
    
    fObj.attrs.forEach(function(item) {
      fObj[item] = $('#field-' + item + '-input');
    });
    
    fObj.copyValues(values);
    fObj.displayFields(fObj.subcategory.val());
      
    fObj.subcategory.change(function () { fObj.displayFields(fObj.subcategory.val()) });
      
    return fObj;
  };
  
  return fElems;
}


function fieldsetDialog(url, values) {
  var f = fieldsetElems().get(values);
  
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getFieldsetInputVals();
        var complete = function(context) {
          url.fieldset = false;
          url.rev = false;
          
          populateFieldsets(url);
          $(context).dialog("close");
        }
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.fieldset;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  return dialog;
}

// Returns an object with references to add/edit fieldset dialog
// field elements with helper functions. 

function fieldsetElems() {
  var fElems = {};
  
  fElems.attrs = ["name", "label", "order", "description", 
                  "doctype", "rev", "multiple", "collapse",
                  "fieldset"];
               
  fElems.get = function(values) {
    var fObj = {};
    
    fObj.attrs = fElems.attrs;
    
    fObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]')) {
          if (source[field] == "true") fObj[field].attr('checked', true);
        }
      });
      return fObj;
    };
    
    fObj.getFieldsetInputVals = function() {
      var valObj = {
        "category": "fieldset", 
        "name": fObj.name.val(),
        "label": fObj.label.val(),
        "order": fObj.order.val() * 1,
        "description": fObj.description.val(),
        "doctype": fObj.doctype.val(),
        "multiple": fObj.multiple.is(':checked'),
        "collapse": fObj.collapse.is(':checked')
      }
      return valObj;
    };
    
    fObj.clear = function() {
      clearValues($('#fieldset-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };
                   
    fObj.attrs.forEach(function(item) {
      fObj[item] = $('#fieldset-' + item + '-input');
    });
    
    fObj.copyValues(values);
      
    return fObj;
  };
  
  return fElems;
}


// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher(
    {
      ".add-button span": function(t) {initFieldset(t.parent());},
      ".remove-button span": function(t) {removeFieldset(t.parent());},
      "#save-document-button span": function(t) {saveDoc(t.parent());},
      "#create-document-button span": function(t) {createDoc(t.parent());},
      "#clear-document-button span": function(t) {clearDoc(t.parent());},
      "#document-edit-button span": function(t) {editDoc(t.parent());},
      "#document-delete-button span": function(t) {deleteDoc(t.parent());},
      "#document-restore-button span": function(t) {restoreDoc(t.parent());},
      "#document-view-tree > ul > li > b": function(t) {collapseToggle(t);},
      ".revision-link": function(t) {fetchRevision(t);},
      ".expander": function(t) {toggleTextarea(t);},
      "label span": function(t) {showHelpDialog(t);}
    });

  action(e);
}

// These are the target functions called in the click dispatch area.
// Some are defined elsewhere for organizational reasons.

function removeFieldset(target) {
  target.parent().remove();
}

function saveDoc(target) {
  var saveButton = target;

  if (saveButton.hasClass('oldrev')) {
    if (!confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
      return false;
    }
  }
  
  var root = $('#edit-document-form');
  var document = dInfo("document", saveButton);
  var rev = dInfo("rev", saveButton);
  var url = "./documents/" + document + "?rev=" + rev;
  var obj = {
    doctype: dInfo("doctype", saveButton),
    description: dInfo("description", saveButton)
  };
  
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton.button('disable');
  $.extend(obj, fieldsetsToObject(root));
  
  $.ajax({
           type: "PUT",
           url: url,
           dataType: "json",
           contentType: "application/json",
           processData: false,
           data: JSON.stringify(obj),
           complete: function(req, status) {
             if (req.status == 204 || req.status == 200) {
               var title = "Success";
               var body = "Your document was saved.";
               getDocument(document, true);
               getIndex();
               flashHighlight(title, body);
               saveButton.removeClass('oldrev');
               saveButton.button('enable');
             } else if (req.status == 403) {
               setInvalidError(req);
               saveButton.button('enable');
             } else if (req.status == 409) {
               var body = JSON.parse(req.responseText);
               var title = req.statusText;
               
               flashError(title, body.message);
               saveButton.button('enable');
             }
           }
         });
}

function createDoc(target) {
  var createButton = target;
  var root = $('#edit-document-form');
  var obj = {
    doctype: dInfo("doctype", createButton),
    description: dInfo("description", createButton)
  };
  
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  createButton.button('disable');
  $.extend(obj, fieldsetsToObject(root));
  
  var postUrl = $.ajax({
    type: "POST",
    dataType: "json",
    contentType: "application/json",
    processData: false,
    data: JSON.stringify(obj),
    complete: function(req, status) {
      if (req.status == 201) {
        var title = "Success";
        var body = "Your document was created.";
        var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);
        
        $('#save-document-button').hide();
        $('#save-document-button').attr('disabled','true');
        $('.fields').remove();
        initFieldsets();
        getDocument(documentId);
        getIndex();
        flashHighlight(title, body);
        createButton.button('enable');
      } else if (req.status == 403) {
        setInvalidError(req);
        createButton.button('enable');
      }
    }
  });
}

function clearDoc(target) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').hide();
  $('#save-document-button').attr('disabled','disabled');
  $('.fields').remove();
  initFieldsets();
}

function editDoc(target) {
  resetFields();
  if ($('#document-view-tree').hasClass('oldrev')) {
    $('#save-document-button').addClass('oldrev');
  } else {
    $('#save-document-button').removeClass('oldrev');
  }
  fillFieldsets();
}

function deleteDoc(target) {
  if (confirm("Are you sure?")) {
    var document = dInfo("document", target);
    var rev = dInfo("rev", target);
    
    deleteDocument(document, rev);
  }
}

function restoreDoc(target) {
  if (confirm("Are you sure?")) {
    var document = dInfo("document", target);
    var rev = dInfo("rev", target);
    
    restoreDocument(document, rev);
  }
}

function collapseToggle(target) {
  target.parent('li').toggleClass('collapsed');
}

function showHelpDialog(target) {
  if (target.is('.label-text')) {
    target = target.parent('label').find('.ui-icon-help');
  }
  
  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));
}

function toggleTextarea(target) {
  var textarea = $('#' + target.attr('data-group-id'));
  
  textarea.toggleClass('expanded');
  target.toggleClass('expanded');
}

var fetchRevision = function(target) {
  var id = dInfo("document", target);
  var rev = dInfo("rev", target);
  var oldrev = target.attr("data-document-oldrev");

  if (rev != oldrev) {
    $('#document-view-tree').addClass('oldrev');
  } else {
    $('#document-view-tree').removeClass('oldrev');
  }

  getRevision(id, oldrev);
};
// Helper for building the url to access a fieldset for a document
function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
    "/doctypes/" + doctype +
    "/fieldsets/" + fieldset;
}

function fsContainer(id) {
  return $("#container-" + id);
}

function dpath(source, category) {
  var url = path(source, category);
  url.doctype = false;
  return url;
}

function fsInfo(key, elem) {
  return getValue("fieldset-" + key, elem);
}

function fInfo(key, elem) {
  return getValue("field-" + key, elem);
}

function dInfo(key, elem) {
  return getValue("document-" + key, elem);
}

function loadHash(urlHash) {
  if (urlHash) {
    getDocument(urlHash);
  }
  return false;
}

var searchAllFieldsSwitch = function() {
  $('#search-all-fields-switch a')
    .live("click", function() {
            searches.clearSearchVals();
          });
};

var searchFieldItems = function() {
  $('.search-field-item')
    .live("click", function(e) {
            searches.removeSearchField(e);
          });
};

var fieldViews = function() {
  $('.search-result-field-id a, .field-view b, .field-container label span')
    .live('dblclick', function(e) {
            searches.addSearchField(e);
          });
};

var searchIndex = function() {
  $('#index-index-input-label')
    .live('dblclick', function(e) {
            searches.addSearchIndex(e);
          });  
};

var excludeCheck = function() {
  $('#document-search-exclude')
    .live("change", function(e) {
            searches.toggleExclusion(e);
          });
};

var loadDocument = function(docid) {
  $("#document-view").html("<em>Loading...</em>");
  clearDoc();
  getDocument(docid);
};

var jumpForm = function() {
  $('#view-jump-id')
    .live("keydown", 
          function(e) {
            if (e.which === 13) {
              var docid = $('#view-jump-id').val();
              loadDocument(docid);
            }
            return true;
          });  
};

var documentLinks = function() {
  // Allows the document for the listed item to be displayed
  // in the correct pane on click.
  $('.view-document-link')
    .live("click", 
          function () {
            loadDocument(this.hash.slice(1));
          });
};

var searchForm = function() {
  searches.clearSearchVals(true);
  searches.loadSearchVals();
  searchAllFieldsSwitch();
  searchFieldItems();
  fieldViews();
  excludeCheck();
  searchIndex();
  $('#document-search-term')
    .live("keydown",
          function(e) {
            if (e.which === 13) {
              searches.getSearch();
              return false;
            }
            return true;
          });
};

$(
  function () {
    var getIndexTimer;

    $('body').click(function(e) {clickDispatch(e);});
    
    documentLinks();
    fillQueryOptions();
    getIndex();
    jumpForm();
    searchForm();
    initEdit();

    $('#index-filter-form input').keyup(
      function() {
        clearTimeout(getIndexTimer);
        getIndexTimer = setTimeout(function () {getIndex();}, 500);
      });
  
    $('#index-filter-form select').change(
      function() {
        getIndex();
      });
  
    loadHash($(location)[0].hash.split("#")[1]);
  });

// Initialize the form in the edit pane

function initEdit() {
  var url = "documents/edit";
  
  $.get(url, function(documentEditHtml) {

    $('#document-edit').html(documentEditHtml);
    $('#edit-tabs').tabs();
    setKeyboardEvents();
    initFieldsets();
    initEditButtons();
  });
  
  return true;
}

// This function initializes all the keyboard events for the
// edit pane
function setKeyboardEvents() {
  var inputable = 'input, select';
  var t = $('#edit-tabs');
  
  var selectInput = function() {
    var cur = t.find('.ui-tabs-selected a').attr('href');
    $(cur).find(inputable + ", textarea").first().focus();
  };
  
  $(document).bind('keydown', 'Alt+p', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected != 0) {
      t.tabs('select', selected - 1);
      selectInput();
    } else {
      t.tabs('select', totaltabs - 1);
      selectInput();
    }
    
    return false;
  });
  
  $(document).bind('keydown', 'Alt+n', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected < totaltabs - 1) {
      t.tabs('select', selected + 1);
      selectInput();
    } else {
      t.tabs('select', 0);
      selectInput();
    }
    
    return false;
  });
}

// Functions for initializing buttons

// Initialize buttons for form in right column
function initEditButtons() {
  initAddButton();
  initSaveButton();
  initCreateButton();
  initClearButton();
  return true;
}

// Initialize the button that will add additional fieldsets to
// a multiple fieldset 
function initAddButton() {
  $(".add-button").button({icons: {primary: "ui-icon-plus"}});
  
  return true;
}

// Initialize the button that removes a fieldset from a multiple fieldset
function initRemoveButton() {  
  return $(".remove-button").button({icons: {primary: "ui-icon-minus"}});
}

// Initialize the save button that is used for already existing documents.
// Hide the button until the form has been filled by an already existing
// document's values. See the after functions above.
function initSaveButton() {
  $('#save-document-button').button({ icons: {primary: "ui-icon-disk"}});
  $('#save-document-button').hide();
  return $('#save-document-button').attr('disabled', 'disabled');
}

// When a form validation error has occurred, this will cause it to
// be displayed
function setInvalidError(req) {
  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = $('[data-field-instance=' + body.instance + ']');
  var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');
  
  invalidTab.addClass('ui-state-error');
  invalid.addClass('ui-state-error');
  
  flashError(title, body.fieldname + " " + body.message);
}

// Initialize the create as new button used to create a new document based
// on filled in values.
function initCreateButton() {
  return $("#create-document-button").button({icons: {primary: "ui-icon-document"}});
}

// Initialize the clear button that will rebuild the form fresh.
function initClearButton() {
  return $('#clear-document-button').button({icons: {primary: "ui-icon-refresh"}});
}

// Functions to run after the form is refreshed or filled.

// This is run after the form is refreshed with new values inserted.
function afterEditRefresh() {
  var saveBttn = $('#save-document-button');
  var editBttn = $('#document-edit-button');
  var sharedAttrs = ['data-document-id', 'data-document-rev'];
  
  sharedAttrs.forEach(function(elem) {
    saveBttn.attr(elem, editBttn.attr(elem));
  });
  
  saveBttn.show();
  
  afterRefresh();
  
  return true;
}

// This is run after the form is cleared or created empty
function afterFreshRefresh() {
  initRemoveButton();
  afterRefresh();
  return true;
}

// This should be run after either afterFreshRefresh() or afterEditRefresh()
function afterRefresh() {
  initDateFields();
  initInstances();
  return true;
}

var initInstances = function() {
  var text = ['0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f',
              '0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f'];  
  var makeInstance = function() {
    return text.map(function() {
                      return text[Math.floor(Math.random() * text.length)];
                    }).join('');
  };

  $("[data-field-instance]").each(
    function(index, item) {
      var newInstance = makeInstance();
      $(item).first().attr('data-field-instance', newInstance);
      $(item).first().attr('data-group-id', newInstance);
      $(item).first().attr('id', newInstance);
      $(item).first().next('.expander')
        .attr('data-group-id', newInstance);
      $(item).first().next().next('.expander')
        .attr('data-group-id', newInstance);
    });

  return true;
};

function resetFields() {
  $('.field').each(function(index) {
    var field = $(this);
    var thedefault = field.attr('data-field-default');
    
    if (thedefault && thedefault != '') {
      if (field.is('select.multiselect')) {
        field.val(thedefault.split(","));
      } else if (field.is('input.boolean')) {
        field.attr('checked', thedefault == true);
      } else {
        field.val(thedefault);
      }
    } else {
      field.val('');
      field.removeAttr('checked');
    }
  });
}

function fillFieldsets() {
  $('.fieldset-view').each(function(i, fieldset) {
    if (fsInfo("multiple", $(fieldset)) == "true") {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });
  
  afterEditRefresh();
}

function fillMultiFieldsets(vfieldset) {
  vfieldset = $(vfieldset);
  var id = fsInfo("fieldset", vfieldset);
  var container = $('#container-' + id);
  var url = dpath(vfieldset, "fieldset");
  
  container.html('');
  
  vfieldset.find('.multifield').each(function(i, multifield) {
    initFieldset(container, function(fieldset) {
      fillFields($(multifield), fieldset);
    });
  });
}

function fillNormalFieldsets(vfieldset) {
  fillFields($(vfieldset));
}

function fillFields(container, context) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').removeAttr('disabled');
  
  container.find('.field-view').each(function(i, field) {
    var value = $(field).attr('data-field-value');
    var id = $(field).attr('data-field-field');
    
    if (!context) context = $('body');
    
    setFieldValue(context.find('.field[data-field-field=' + id + ']'), value);
  });
}

function setFieldValue(field, value) {
  if (field.is('input.boolean')) {
    field.attr("checked", value == "true");
  } else if (value && field.is('select.multiselect')) {
    field.val(value.split(","));
  } else if (value && (field.is('input.text') || field.is('select.file')))  {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else if (field.is('textarea.textarea')) {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else {
    field.val(value);
  }
}

var ifStoredElse = function(key, success, otherwise) {
  var item = null;

  item = localStorage.getItem(key);

  if (item) {
    success(item);
  } else {
    $.get(key, otherwise);
  }
};

var initFieldsets = function() {
  var reload;
  var container = $("#create-document-button");
  var doctype = dInfo("doctype", container);
  var versionKey = doctype + "_version";
  var oldVersion = localStorage.getItem(versionKey);
  var curVersion = dInfo("version", container);

  reload = oldVersion != curVersion;
  localStorage.setItem(versionKey, curVersion);

  $('fieldset').each(function(i, fieldset) {
    initFieldset(fieldset, false, reload);
  });
  
  return true;
}

var initFieldset = function(fieldset, callback, reload) {
  var url = dpath($(fieldset), "fieldset").toString();
  var id = fsInfo("fieldset", $(fieldset));
  var container = $('#container-' + id);
  var appendIt = function(data) {
    container.append(data);
    initFields(container, callback, reload);
  };
  var storeIt = function(data) {
    localStorage.setItem(url, data);
    appendIt(data);
  };

  if (reload) {
    localStorage.removeItem(url);
  }

  ifStoredElse(url.toString(), appendIt, storeIt);

  return false;
}

var initFields = function(container, callback, reload) {
  var url = dpath(container, "field");
  var section = container.children('.fields').last();
  var prependIt = function(data) {
    section.prepend(data);
    if (callback) {
      callback(section);
    }
    
    afterFreshRefresh();
  };
  var storeIt = function(data) {
    localStorage.setItem(url, data);
    prependIt(data);
  };

  if (reload) {
    localStorage.removeItem(url);
  }

  ifStoredElse(url.toString(), prependIt, storeIt);
    
  return true;
}

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
      min: dateOrNumber(fInfo("subcategory", field), fInfo("min", field)),
      max: dateOrNumber(fInfo("subcategory", field), fInfo("max", field)),
      instance: fInfo("instance", field),
      charseq: fInfo("charseq", field),
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

function dateOrNumber(subcategory, fieldvalue) {
  switch (subcategory) {
    case "date":
      return fieldvalue;
    default:
      return stringToNumber(fieldvalue);
  }
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

// Get the index that is displayed in the index pane. 
// startkey and startid map directly to the same concepts in
// couchdb view queries. The prevkeys and previds are used to
// hold information that will allow the user to page backward
// through the listing. They are arrays of keys and ids corresponding
// to previous page's startkeys and ids.
//
// There are a number of values that this function depends on
// that are taken from the HTML. These include the value for
// the limit and the nextkey and nextid for paging forward. Also
// the current key and id are taken from the html when needed to
// add to the prevkeys and previds. The startkey may be a user
// input value so a more reliable startkey and startid are needed.

function getIndex(startkey, startid, prevkeys, previds) {
  var url = "documents/index?";
  var index = $('#index-index-input').val();
  var limit = $('#index-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!prevkeys) {
    var supplied_val = $('#index-filter').val();
    var json_encoded = JSON.stringify(supplied_val);
    startkey = btoa(unescape(encodeURIComponent(json_encoded)));
    prevkeys = [];
    previds = [];
  }
  
  if (startkey) {
    url = url + '&startkey=' + escape(atob(startkey));
    
    if (startid) {
      url = url + '&startkey_docid=' + startid;
    }
  }
  
  // The user supplied limit will need a plus one so that we can
  // get the start key for the next page from the server.
  if (limit) {
    url = url + '&limit=' + (limit + 1);
  } else {
    // Ten is the default and I don't let people leave it blank
    // because the list could be huge.
    $('#index-limit').val(25);
    url = url + '&limit=26';
  }
  
  if (index) {
    url = url + '&index=' + index;
  }
  
  $.get(url, function(documentIndexHtml) 
        {
          $('#document-index #index-listing').html(documentIndexHtml);
          
          $('#previous-index-page').button(
            {
              icons: {primary:'ui-icon-circle-arrow-w'} 
            }).click(function() 
                     {
                       getIndex(prevkeys.pop(), 
                                previds.pop(), 
                                prevkeys, 
                                previds);
                     });
    
          // Collect the values needed for paging from the HTML
          $('#next-index-page').button(
            {
              icons: {secondary:'ui-icon-circle-arrow-e'}
            }).click(function() 
                     {
                       var nextkey = $(this).attr('data-startkey');
                       var nextid = $(this).attr('data-startid');
                       var prevkey = 
                         $('#first-index-element').attr('data-first-key');
                       var previd = 
                         $('#first-index-element').attr('data-first-id');
                       prevkeys.push(prevkey);
                       previds.push(previd);
      
                       getIndex(nextkey, nextid, prevkeys, previds);
                     });
    
          // Disable the previous button if we're at the beginning
          if (prevkeys.length == 0) {
            $('#previous-index-page').button("disable");
          }
    
          // Disable the next button if we're at the end
          if ($('#next-index-page').attr('data-last-page')) {
            $('#next-index-page').button("disable");
          }
  
          $('nav.pager').buttonset();
    
        });
}

function fillQueryOptions() {
  var url = "/projects/project-" + $('#container').attr('data-project-id') +
        "/indexes?as=options";
        
  $.get(url, function(data) {
    $('#index-index-input').html(data);
  });
}

var searches = {
  getSearch: function() {
    var query = $('#document-search-term').val();
    var url = "documents/search?q=" + encodeURIComponent(query);
    var field = $('#document-search-field').val();
    var exclude = $('#document-search-exclude').is(':checked');
    var index = $('#document-search-index').val();
    var lookup = searches.fieldLookup();

    if (index) {
      url = url + "&index=" + index; 
    } else {
      if (field) {
        url = url + "&field=" + field;
      }
      if (exclude) {
        url = url + "&exclude=true";
      }
    }

    $('#search-listing').hide();

    $.get(url, function(searchResults) {
            $('#search-listing').html(searchResults);
            $('.search-result-field-id')
              .each(function(index, item) {
                      var label = lookup[$(item).attr('data-field-field')];
                      var target = $(item).children('a').first();
                      target.html(label);
                      target.attr('data-search-label', label);
                    });
            $('.search-results th')
              .each(function(index, item) {
                      var itemText = $.trim($(item).children('a').html());
                      var re = new RegExp("(" + query + ")", "g");
                      var newText = 
                        itemText.replace(re, 
                                         "<span class='highlight'>$1</span>");
                      $(item).children('a').html(newText);
                    });
            $('#search-listing').show();
          });
  },

  fieldLookup: function() {
    var lookup = {};
    
    $('fieldset').each(
      function(index, fset) {
        var fsLabel = $(fset).attr('data-fieldset-label');
        $(fset).find('.field-container').each(
          function(index, item) {
            var id = $(item).attr('data-field-field');
            var label = $(item).find('.label-text').first().text();
            lookup[id] = fsLabel + ": " + label;
          });
      });
    return lookup;
  },

  lookup: function(item) {
    var stored = localStorage.getItem(item);
    if (stored === "" || stored === "null") {
      return null;
    } else {
      return stored;
    }
  },

  excludedVal: function() {
    var exclude = $('#document-search-exclude').is(':checked');

    if (!exclude) {
      return null;
    } else {
      return exclude;
    }
  },

  toggleExclusion: function(e) {
    var exclude = searches.excludedVal();
    var excludeLabel = $('#search-exclude-label');

    if (exclude) {
      excludeLabel.show();
    } else {
      excludeLabel.hide();
    }

    localStorage.setItem("searchExclude", exclude);
  },

  clearSearchVals: function(initial) {
    $('#document-search-field').val(null);
    $('#document-search-index').val(null);
    $('#document-search-exclude:checked').click();
    $('.search-optional, #search-exclude-label').hide();
    $('.search-field-item').remove();
    $('#search-index-label').empty();
    if (!initial) {
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
    }
  },

  loadSearchVals: function() {
    var index = searches.lookup("searchIndex");
    var fieldids = searches.lookup("searchFields");

    if (index !== null) {
      $('#document-search-index').val(index);
      $('#search-index-label').html(localStorage.getItem("searchIndexLabel"));
      $('.search-optional').show();
      $('#document-search-exclude').parent('div').hide();
    } else if (fieldids !== null) {
      $('#document-search-field').val(fieldids);
      $('#search-field-label').html(localStorage.getItem("searchLabels"));

      if (searches.lookup("searchExclude") != searches.excludedVal()) {
        $('#document-search-exclude').click();
      }

      $('.search-optional').show();
    }
  },

  updateSearchVals: function(fieldids, labels, exclude, index) {
    if (index) {
      localStorage.setItem("searchIndex", index);
      localStorage.setItem("searchIndexLabel", labels);
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
    } else {
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
      localStorage.setItem("searchLabels", labels);
      localStorage.setItem("searchFields", fieldids);
      localStorage.setItem("searchExclude", exclude);
    }
  },

  removeSearchField: function(e) {
    var item = $(e.target);
    var value = item.attr('data-index');
    var searchField = $('#document-search-field');
    var currentVal = searchField.val();
    var valDecoded = JSON.parse(currentVal);
    var exclude = searches.excludedVal();
    var newVal = null;

    if (valDecoded.length === 1) {
      searches.clearSearchVals();
    } else {
      var index = valDecoded.indexOf(value);
      
      if (index >= 0) {
        valDecoded.splice(index, 1);
        newVal = JSON.stringify(valDecoded);
        searchField.val(JSON.stringify(valDecoded));
      }

      item.remove();
    }
    
    searches.updateSearchVals(newVal, $('#search-field-label').html(), exclude);
  },

  addSearchIndex: function(e) {
    var indexVal = $('#index-index-input').val();
    var indexLabel = $('option[value=' + indexVal + ']').text();

    if (validID(indexVal)) {
      $('#search-all-fields-switch').show();
      $('#search-field-label').hide();
      $('#search-exclude-label').empty();
      $('#document-search-field').val(null);
      $('#document-search-exclude').parent('div').hide();
      $('#search-index-label').html(indexLabel).show();
      $('#document-search-index').val(indexVal);
      searches.updateSearchVals(null, indexLabel, null, indexVal);
    }
  },

  addSearchField: function(e) {
    var fieldid = $(e.target).closest('[data-field-field]')
      .attr('data-field-field');

    if (validID(fieldid)) {
      var fieldLabel = searches.fieldLookup()[fieldid];
      var searchField = $('#document-search-field');
      var currentVal = searchField.val();
      var searchLabel = $('#search-field-label');
      var exclude = searches.excludedVal();
      var newDecoded;
      var newVal = null;
      var newAnchor = '<a href="#" data-index="' + fieldid + 
        '" class="search-field-item" title="click to remove">' + 
        fieldLabel + '</a>';

      var setSearchVals = function(value) {
        if (searchLabel.html()) {
          searchLabel.children().last().after(newAnchor);
        } else {
          searchLabel.html(newAnchor);
        }
        
        newVal = JSON.stringify(value);
        searchField.val(newVal);
        searches.updateSearchVals(newVal, searchLabel.html(), exclude);
      };

      if (currentVal !== '') {
        var valDecoded = JSON.parse(currentVal);
        if (valDecoded.indexOf(fieldid) < 0) {
          newDecoded = valDecoded.concat(fieldid);
          setSearchVals(newDecoded);
        }
      } else {
        newDecoded = [fieldid];      
        setSearchVals(newDecoded);
      }

      $('.search-optional').show();
    }
  }
};

// Functions for building view via ajax calls. Functions preceded by "get"
// are generally pulling in data. Functions preceded by "init" are generally
// generating parts of a form.

var formatTimestamps = function() {
  $('.timestamp').each(
    function(i, item) {
      var newDate = (new Date($(item).text())).toLocaleString();
      if (newDate !== "Invalid Date") {
        $(item).text(newDate);
      }
    });
};

// Get a document revision and display it in the middle column
var getRevision = function(id, rev) {
  var url = "documents/" + id + "/" + rev;
  var dvt = $('#document-view-tree');

  dvt.hide();

  $.get(url, function(documentHtml) {
          dvt.html(documentHtml);
          formatTimestamps();
          dvt.show();
        });
};

// Get a document and display it in the middle column
function getDocument(id, runAfterEditRefresh) {
  var url = "documents/" + id;
  
  $('#document-view').fadeTo('slow', 1);
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);

    formatTimestamps();

    if (runAfterEditRefresh) afterEditRefresh();
    
    var restoreButton = $('#document-restore-button');
    var editButton = $('#document-edit-button');
    var deleteButton = $('#document-delete-button');
   
    editButton.button({icons: {primary: 'ui-icon-pencil'}});
    deleteButton.button({icons: {primary: 'ui-icon-trash'}});
    restoreButton.button({icons: {primary: 'ui-icon-refresh'}});
    
    if (dInfo("deleted", restoreButton) === "true") {
      editButton.hide();
      deleteButton.hide();
    } else {
      restoreButton.hide();
    }
  });
}

function restoreDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  var restoreButton = $('#document-restore-button');
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 200) {
        var title = "Success";
        var body = "Your document was restored.";

        getDocument(docid, function() {getIndex();});
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Document was erased and cannot be restored.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });
}

function deleteDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  var restoreButton = $('#document-restore-button');
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 200) {
        var title = "Success";
        var body = "Your document was deleted.";
        var response = JSON.parse(req.responseText);
        
        putValue("document-rev", response.rev, restoreButton);
        
        $('#document-delete-button').hide();
        $('#document-edit-button').hide();
        restoreButton.show();
        $('#document-view h2').text("Deleted Document");
        $('#document-view').fadeTo('slow', 0.5);
        
        getIndex();
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Document appears to have been deleted already.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });
}

var getDirListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_dirs/" + path, function (data) {
    $('#file-paths').html(data);
    $('.dir').click(function (e) {
      var newpath = $(e.target).attr('data-path');
      
      refreshListings(newpath);
    });
    $('#up-dir').button().click(function () {
      var newpath = path.split("/");
      newpath.pop();
      newpath = newpath.join("/");
      
      refreshListings(newpath);
    });
    $('#root-dir').button().click(function () {
      refreshListings();
    });
  });
};

var getFileListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_files/" + path, function (data) {
    $('#file-listing').html(data);
    refreshButtons(path);
  });
};

var refreshListings = function (path) {
  getDirListing(path);
  getFileListing(path);
};

var refreshButtons = function (path) {
  refreshEditButton();
  refreshDeleteButton();
};

var refreshEditButton = function (path) {
    $('.edit-file-button').button({
      icons: {primary: 'ui-icon-pencil'}
    }).click(function (e) {
      var target = $(e.target).parent('a');
      var fileId = target.attr('data-file-id');
      var url = "file_manager/" + fileId;
      
      $.getJSON(url, function (obj) {
        pathEditDialog(obj, path).dialog('open');
      });
    });
};

var refreshDeleteButton = function (path) {
    $('.delete-file-button').button({
      icons: {primary: 'ui-icon-trash'}
    }).click(function(e) {
      var target = $(e.target).parent('a');
      var fileId = target.attr('data-file-id');
      var fileRev = target.attr('data-file-rev');
      var url = "file_manager/" + fileId + "?rev=" + fileRev;
      var complete = function () {
        refreshListings(path);
        flashHighlight("Success", "File Deleted");
      };
      
      sendConfigDoc(url, null, 'DELETE', complete, target);
    });
};

var pathEditDialog = function (obj, path) {
  var pathInput = $('#file-path-input');
  
  if (obj.path) {
    pathInput.val(obj.path.join("/"));
  } else {
    pathInput.val('');
  }
  
  var dialog = $('#edit-path-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Move": function () {
        var url = "file_manager/" + obj._id + "?rev=" + obj._rev;
        var complete = function () {
          refreshListings(path);
          flashHighlight("Success", "File Moved");
        };
        
        obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split("/");
        sendConfigDoc(url, obj, 'PUT', complete, dialog);
        $(this).dialog("close");
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    }
  });
  
  return dialog;
};

$(function () {
  refreshListings();
  
  $('#file-upload-target').load(function() {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function () {
      if (encoded) if (encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {message: false};
      }
    };
    
    if (obj()) if (obj().message && obj().status === "error") {
      flashError("Error", obj().message);
      refreshListings();
    } else if (obj().message) {
      flashHighlight("Success", obj().message);
      refreshListings();
    }
  });
});
var initIndexBuilderDialog = function(indexDoctype) {
  var builderOr = $("#builder-or-input");
  var builderParen = $("#builder-paren-input");
  var builderNegate = $("#builder-negate-input");
  var builderOperator = $("#builder-operator-input").inputDisable();
  var builderArgument = $("#builder-argument-input").inputDisable();
  var builderFieldset = $("#builder-fieldset-input").inputDisable();
  var builderField = $("#builder-field-input").inputDisable();
  var notBlank = [builderOperator, builderFieldset, builderField];
  var fieldset_url = 'doctypes/' + indexDoctype + '/fieldsets';
  var condition_url = 'indexes/condition';
  
  $('.ui-helper-reset div').show();

  var appendCondition = function(builderRow) {
    var tableBody = $('#index-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();
    initConditionRemoveButtons(tableBody);
    
    return false;
  };
    
  fillOptionsFromUrl(fieldset_url, builderFieldset, 
                     function () {builderFieldset.inputEnable();});
  
  builderOr.change(function() {
                     if (builderOr.is(':checked')) {
                       $('#builder-conditions').hide();
                       $('#builder-parens').hide();
                     } else {
                       $('#builder-conditions').show();
                       $('#builder-parens').show();
                     }
                   });
  
  builderParen.change(function() {
                        if (builderParen.val()) {
                          $('#builder-or').hide();
                          $('#builder-conditions').hide();
                        } else {
                          $('#builder-or').show();
                          $('#builder-conditions').show();
                        }
                      });
  
  var fieldsetEvents = function () {
    setIndexFieldsetEvents(indexDoctype, builderFieldset, builderField, 
                           function () {
                             builderOperator.inputDisable();
                             builderField.inputDisable();
                             builderArgument.inputDisable();
                             
                             return function () {
                               builderField.inputEnable();
                             };
                           });
  };
  
  var fieldEvents = function () {
    setIndexFieldEvents(indexDoctype, builderFieldset, builderField, 
                        function () {
                          builderOperator.inputDisable();
                          builderArgument.inputDisable();
                          
                          return function () {
                            builderOperator.inputEnable();
                          };
                        });
  };
  
  var operatorEvents = function () {
    setIndexOperatorEvents(builderArgument, builderOperator, builderField, 
                           function () {
                             builderArgument.inputDisable();
                             
                             return function () {
                               builderArgument.inputEnable();
                             };
                           });
  };
  
  var dialog = $("#index-builder-dialog")
    .dialog({
              autoOpen: false,
              modal: true,
              buttons: {
                "Create": function() {
                  $('.input').removeClass('ui-state-error');
        
                  // place holder for client side validation
                  var checkResult = true;
        
                  if (!builderOr.is(':checked') && !builderParen.val()) {
                    notBlank.forEach(function(item) {
                                       if (item.val().isBlank()) {
                                         item.addClass('ui-state-error');
                                         checkResult = false;
                                       } else {
                                         item.removeClass('ui-state-error');
                                       }
                                     });
                  }
        
                  if (checkResult) {
                    if (builderOr.is(':checked')) {
                      $.get(condition_url, {"is_or": true}, 
                            function(data) {appendCondition(data);});
                    } else if (builderParen.val()) {
                      $.get(condition_url, {
                              "is_or": false,
                              "parens": builderParen.val(),
                              "negate": false
                            },function(data) {appendCondition(data);});
                    } else {
                      $.get(condition_url, {
                              "is_or": false,
                              "parens": false,
                              "negate": builderNegate.is(':checked'),
                              "fieldset": builderFieldset.val(),
                              "field": builderField.val(),
                              "operator": builderOperator.val(),
                              "argument": builderArgument.val()
                            }, function(data) {appendCondition(data);});
                    }
          
                    $(this).dialog("close");
                  }
                },
                "Cancel": function() {
                  $(this).dialog("close");
                }
              },
              close: function() {
                $('#builder-conditions').show();
                builderFieldset.unbind('change');
                builderField.unbind('change');
                builderOperator.unbind('change');
                clearValues($('.input')).removeClass('ui-state-error');
              }
            });
  
  fieldsetEvents();
  fieldEvents();
  operatorEvents();
  
  return dialog;
};

var initIndexNewButton = function() {
  $('#new-index-button').button(
    {
      icons: {primary: "ui-icon-plus"}
    }).click(function() {
               initIndexNewDialog().dialog("open");
             });
  
  return false;
};

var initConditionRemoveButtons = function(tableBody) {
  tableBody.find('.remove-condition-button').button(
    {
      icons: {primary: "ui-icon-minus"}
    }).click(function(e) {
               $(e.target).closest('tr').remove();
             });
  
  return false;
};

var initIndexDeleteButton = function(button, buttonData) {
  button.button(
    {icons: {primary: "ui-icon-trash"}
    }).click(function (e) {
               var bData = buttonData();
               
               if (!bData.length < 1) {
                 var deleteButton = $(e.target);
                 var indexId = bData.attr('data-index-id');
                 var indexRev = bData.attr('data-index-rev');
                 var completeMessage = "Your index has been deleted.";
                 var completeFunction = function() {
                   $('#index-conditions').empty();
                   initIndexIndex();
                 };
                 
                 if (confirm("Are you sure?")) {
                   deleteIndex(indexId, indexRev, completeMessage, 
                               completeFunction);
                 }
               } else {
                 flashHighlight("Info", "No index has been chosen to delete.");
               }
             });
  
  return false;
};

var initIndexAddConditionButton = function(button, buttonData) {
  button.button({
                  icons: {primary: "ui-icon-plus"}
                }).click(function (e) {
                           var bData = buttonData();
                           
                           if (!bData.length < 1) {
                             initIndexBuilderDialog(
                               bData.attr('data-index-doctype')).dialog("open");
                           } else {
                             flashHighlight("Info", 
                                            "You must choose an index first.");
                           }
                         });

  return false;
};

var initReplaceButton = function(button, buttonData) {
  button.button({
                  icons: {primary: "ui-icon-shuffle"}
                }).click(function (e) {
                           var bData = buttonData();
                           
                           if (!bData.length < 1) {
                             initReplaceDialog().dialog("open");
                           } else {
                             flashHighlight("Info", 
                                            "You must choose an index first.");
                           }
                         });

  return false;
};

var initIndexEditButtons = function(buttonData) {
  initIndexSaveButton($('#save-index-button'), buttonData);
  initIndexDeleteButton($('#delete-index-button'), buttonData);
  initIndexAddConditionButton($('#add-index-condition-button'), buttonData);
  initReplaceButton($('#replace-button'), buttonData);
  
  return false;
};
 
var initIndexSaveButton = function(button, buttonData) {
  var completeFunction;
  var bData;
  
  button.button(
    {
      icons: {primary: "ui-icon-document"}
    }).click(function (e) {
               bData = buttonData();
    
               if (!bData.length < 1) {
                 completeFunction = function() {
                   getIndexEdit(bData.attr('data-index-id'));
                   flashHighlight("Success", "Your index has been saved.");
                 };
      
                 saveIndex(bData, completeFunction);
               } else {
                 flashHighlight("Info", "No index has been chosen to save.");
               }
             });
};

// Add events to a fieldset. In this case a change event that will populate
// the select options of a following field input element.  indexDoctype may
// be either a string corresponding to the doctype or an input field with
// the doctype as a value. The indexFieldset and indexField should both
// be select elements.

function setIndexDoctypeEvents(indexDoctype, indexFieldset, callback) {
  indexDoctype
    .change(function() {
              var url = 'doctypes/' + 
                indexDoctype.val() + '/fieldsets';
              var callback2;
              
              if (callback) callback2 = callback();
              
              fillOptionsFromUrl(url, indexFieldset, callback2);
            });
  
  return false;
}

function setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, 
                                callback) {
  indexFieldset
    .change(function() {
              var callback2;

              if (!(typeof indexDoctype == "string")) {
                indexDoctype = indexDoctype.val();
              }
              
              if (indexFieldset.val()) {
                var url = 'doctypes/' + indexDoctype + 
                  '/fieldsets/' + indexFieldset.val() + '/fields?as=options';
    
                if (callback) callback2 = callback();
    
                fillOptionsFromUrl(url, indexField, callback2);
              }
            });
  
  return false;
}

function setIndexFieldEvents(indexDoctype, indexFieldset, indexField, 
                             callback) {
  indexField
    .change(function() {
              var fieldId = indexField.val();
              var fieldsetId = indexFieldset.val();
              var callback2;
    
              if (callback) callback2 = callback();
    
              if (!(fieldId.isBlank())) {
                getFieldDoc(fieldId, fieldsetId, indexDoctype, function(data) {
                              alterOperatorField(data, fieldId, callback2);
                            });
              }
            });
  
  return false;
}

function setIndexOperatorEvents(argumentField, operatorField, fieldField, 
                                callback) {
  operatorField
    .change(function() {
              var callback2;
              
              if (callback) callback2 = callback();
              
              alterArgumentField(argumentField, operatorField, fieldField, 
                                 callback2);
            });
  
  return false;
}

var getFieldDoc = function(fieldId, fieldsetId, doctypeId, callback) {
  var fieldDoc = getDoc(fieldId);
  var url = 'doctypes/' + doctypeId + 
    '/fieldsets/' + fieldsetId + 
    '/fields/' + fieldId + '?format=json';
            
  if (fieldDoc) {
    if (callback) {
      callback(fieldDoc);
    }
    return fieldDoc;
  } else {
    $.ajax({
             url: url,
             async: false,
             dataType: 'json',
             success: function(data) {
               putDoc(data);
               if (callback) {
                 callback(getDoc(fieldId));
               }
             }
           });
          
    return getDoc(fieldId);
  }
};

var fillOptionsFromUrl = function(url, selectElement, callback) {
  $.get(url, function(options) {
          selectElement.html(options);
          if (callback) callback();
        });
  
  return false;
};

var alterOperatorField = function(fieldDoc, fieldId, callback) {
  disableOperatorOptions(fieldDoc);
  callback();
  
  return false;
};

var disableOperatorOptions = function(fieldDoc) {
  var options = $('#builder-operator-input');
  
  switch (fieldDoc.subcategory) {
  case "select":
  case "docselect":
  case "text":
  case "textarea":
    disableOptions(options, ["member", "true"]);
    break;
  case "integer":
  case "rational":
  case "date":
    disableOptions(options, ["member", "true", "match"]);
    break;
  case "boolean":
  case "openboolean":
    disableOptions(options, ["equal", "greater", "less", "member", "match"]);
    break;
  case "multiselect":
  case "docmultiselect":
    disableOptions(options, ["equal", "greater", "less", "true", "match"]);
    break;
  }
  
  return false;
};

var disableOptions = function(options, disables) {
  options.children().show();
  
  disables.forEach(function(item) {
                     options.children('option:contains(' + item + ')').hide();
                   });
  
  return false;
};

var alterArgumentField = 
  function(argumentField, operatorField, fieldField, callback) {
    var fieldDoc = function () {return getDoc(fieldField.val());};

    callback();
  
    argumentField.removeAttr('disabled').datepicker('destroy');
    argumentField.removeAttr('disabled').autocomplete('destroy');
  
    var dateOrText = function(argumentField, fdoc) {
      if (fdoc.subcategory == 'date') {
        argumentField.removeAttr('disabled');
        argumentField.datepicker({dateFormat: "yy-mm-dd"});
      } else {
        argumentField.removeAttr('disabled');
        argumentField.autocomplete({source: fdoc.allowed});
      }
    
      return false;
    };

    var fdoc = fieldDoc();
  
    if (fdoc) {
      switch (operatorField.val()) {
      case "true":
      case "isDefined":
      case "blank":
        argumentField.attr('disabled', 'disabled').val("");
        break;
      case "equal":
      case "member":
      case "greater":
      case "less":
      case "hasExactly":
      case "hasGreater":
      case "hasLess":
        dateOrText(argumentField, fdoc);
        break;
      }
    
    }
  };

var fixArgumentType = function(argument, subcategory, operator) {
  switch (subcategory) {
  case "integer":
  case "rational":
    argument = argument * 1;
    break;
  }

  switch (operator) {
  case "hasExactly":
  case "hasGreater":
  case "hasLess":
    argument = Math.floor(argument * 1);
    break;
  }
  
  return argument;
};

var getIndexConditions = function(doctypeId, rows) {
  var conditions = rows.map(
    function(index, row) {
      row = $(row);
      var is_or = row.find('td.or-condition').attr('data-value') == "true";
      var paren = row.find('td.paren-condition').attr('data-value');
      var condition;
    
      if (is_or) {
        condition = {"is_or": true, "parens": false};
      } else if (paren) {
        condition = {"is_or": false, "parens": paren};
      } else {
        var fieldId = row.find('td.field-condition').attr('data-value');
        var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
        var argument = row.find('td.argument-condition').attr('data-value');
        var fieldDoc = getFieldDoc(fieldId, fieldsetId, doctypeId);
        var negate = 
          row.find('td.negate-condition').attr('data-value') == "true";
        var operator = row.find('td.operator-condition').attr('data-value');

        argument = fixArgumentType(argument, fieldDoc.subcategory, operator);
      
        condition = {
          "is_or": false,
          "parens": false,
          "negate": negate,
          "fieldset": fieldsetId,
          "field": fieldId,
          "operator": operator,
          "argument": argument
        };
      }
    
      return condition;
    }).toArray();
  
  return conditions;
};

var saveIndex = function(buttonData, completeFunction) {
  var indexId = buttonData.attr('data-index-id');
  var indexRev = buttonData.attr('data-index-rev');
  var url = "indexes/" + indexId + "?rev=" + indexRev;
  var doctype = buttonData.attr('data-index-doctype');
  
  var obj = {
    "_id": indexId,
    "category": "index",
    "doctype": doctype,
    "show_deleted": buttonData.attr('data-index-show_deleted') === "true",
    "fields": JSON.parse(buttonData.attr('data-index-fields')),
    "fields_label": JSON.parse(buttonData.attr('data-index-fields_label')),
    "name": buttonData.attr('data-index-name'),
    "conditions": getIndexConditions(doctype, $('#index-conditions-listing tbody tr'))
  };
  
  if (buttonData.attr('data-index-replace_function')) {
    obj.replace_function = buttonData.attr('data-index-replace_function');
  }

  sendConfigDoc(url, obj, 'PUT', completeFunction, this);

  return false;  
};

var deleteIndex = 
  function(indexId, indexRev, completeMessage, completeFunction) {
    var url = "indexes/" + indexId + "?rev=" + indexRev;
  
    $.ajax(
      {
        type: "DELETE",
        url: url,
        dataType: "json",
        contentType: "application/json",
        complete: function(req, status) {
          if (req.status == 204) {
            var title = "Success";
            var body = completeMessage;
        
            completeFunction();
        
            flashHighlight(title, body);
          } else if (req.status == 409) {
            var body = JSON.parse(req.responseText);
            var title = req.statusText;
          
            flashError(title, body.message);
          } else if (req.status == 404) {
            var body = "Index appears to have been deleted already.";
            var title = req.statusText;
            
            flashError(title, body);
          }
        }
      });

    return false;  
  };

var getIndexEdit = function(indexId) {
  var url = "indexes/" + indexId;
  var target = $('#index-conditions');
  
  $.get(url, function(indexData) {
          target.html(indexData);
          // TODO don't repeat this code. It is also in initIndexBuilderDialog
          var tableBody = $('#index-conditions-listing tbody');
          tableBody.sortable();
          initConditionRemoveButtons(tableBody);
          getIndexView();
        });
  
  return false;
};

var initIndexIndex = function() {
  var url = "indexes";
  var target = $('#index-index-listing');
  
  $.get(url, function(index) {
          target.html(index);
          target.click(function(e) {
                         getIndexEdit($(e.target).attr('data-index-id'));
                       });
        });
};

var getIndexView = function(startkey, startid, prevkeys, previds) {
  var state = {
    startkey: startkey,
    startid: startid,
    prevkeys: prevkeys,
    previds: previds
  };
  var indexInfo = $('#index-editing-data');
  var indexId = indexInfo.attr('data-index-id');
  var url = "indexes/" + indexId + "/view?";
  var limit = $('#index-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!state.prevkeys) {
    var supplied_val = $('#index-filter').val();
    var json_encoded = JSON.stringify(supplied_val);
    state.startkey = btoa(unescape(encodeURIComponent(json_encoded)));
    state.prevkeys = [];
    state.previds = [];
  }
  
  if (state.startkey) {
    url = url + '&startkey=' + escape(atob(state.startkey));
    
    if (state.startid) {
      url = url + '&startkey_docid=' + state.startid;
    }
  }
  
  // The user supplied limit will need a plus one so that we can
  // get the start key for the next page from the server.
  if (limit) {
    url = url + '&limit=' + (limit + 1);
  } else {
    // Ten is the default and I don't let people leave it blank
    // because the list could be huge.
    $('#index-limit').val(10);
    url = url + '&limit=11';
  }

  sendConfigDoc(url, false, 'GET', 
                function(context, req) 
                {fillIndexPreview(req, state);}, this);  
};
  
var fillIndexPreview = function(req, state) {
  $('#index-list-view').html(req.responseText);
  
  $('#previous-page').button(
    {
      icons: {primary:'ui-icon-circle-arrow-w'} 
    }).click(function() 
             {
               getIndexView(state.prevkeys.pop(), 
                            state.previds.pop(), 
                            state.prevkeys, 
                            state.previds);
             });
    
  // Collect the values needed for paging from the HTML
  $('#next-page').button(
    {
      icons: {secondary:'ui-icon-circle-arrow-e'}
    }).click(function() 
             {
               var nextkey = $(this).attr('data-startkey');
               var nextid = $(this).attr('data-startid');
               var prevkey = 
                 $('#first-index-element').attr('data-first-key');
               var previd = 
                 $('#first-index-element').attr('data-first-id');
               state.prevkeys.push(prevkey);
               state.previds.push(previd);
                       
               getIndexView(nextkey, nextid, state.prevkeys, state.previds);
             });
    
  // Disable the previous button if we're at the beginning
  if (state.prevkeys.length == 0) {
    $('#previous-page').button("disable");
  }
    
  // Disable the next button if we're at the end
  if ($('#next-page').attr('data-last-page')) {
    $('#next-page').button("disable");
  }
  
  $('nav.pager').buttonset();
  
};

$(function () {
    $('#index-builder-dialog').hide();
    $('#index-new-dialog').hide();
    $('#index-replace-dialog').hide();
    initIndexEditButtons(function () {return $('#index-editing-data');});
    initIndexNewButton();
    $('#button-bar').buttonset();
    initIndexIndex();
    $('#index-filter-form input').keyup(function() {
                                          getIndexView();
                                        });
  });

function initIndexNewDialog() {
  var indexDoctype = $("#index-doctype-input");
  var indexFieldset = $("#index-fieldset-input").inputDisable();
  var indexField = $("#index-field-input").inputDisable();
  var indexName = $("#index-name-input");
  var indexShowDeleted = $("#index-show_deleted-input");

  var doctypeEvents = function() {
    setIndexDoctypeEvents(indexDoctype, indexFieldset, function() {
                            indexFieldset.inputDisable();
                            indexField.inputDisable();
                            
                            return function() {
                              indexFieldset.inputEnable();
                            };
                          });
  };
  
  var fieldsetEvents = function() {
    setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, function() {
                             indexField.inputDisable();
      
                             return function() {
                               indexField.inputEnable();
                             };
                           });
  };
  
  var getLabelForVal = function(val) {
    return $('#index-new-dialog option[value="'+ val + '"]').text();
  };

  var getLabel = function() {
    return [getLabelForVal(indexFieldset.val()), 
            getLabelForVal(indexField.val())].join(":");
  };

  var dialog = $("#index-new-dialog")
    .dialog({
              autoOpen: false,
              modal: true,
              buttons: {
                "Create": function() {
                  $('.input').removeClass('ui-state-error');
        
                  // place holder for client side validation
                  var checkResult = true;
        
                  if (checkResult) {
                    var obj = {
                      "category": "index", 
                      "name": indexName.val(),
                      "show_deleted": indexShowDeleted.is(':checked'),
                      "conditions": [], 
                      "doctype": indexDoctype.val(),
                      "fields_label": [getLabel()],
                      "fields": [indexField.val()]
                    },
                    complete = function(context) {
                      initIndexIndex();
                      $(context).dialog("close");
                    };
                    sendConfigDoc("indexes", obj, 'POST', complete, this);
                  }
                },
                "Cancel": function() {
                  $(this).dialog("close");
                }
              },
              close: function() {
                indexFieldset.unbind('change');
                indexDoctype.unbind('change');
                clearValues($('.input')).removeClass('ui-state-error');
              }
            });
  
  doctypeEvents();
  fieldsetEvents();
  
  return dialog;
}
  

var initReplaceDialog = function() {
  var replaceFunction = $('#index-replace_function-input');
  var indexData = $('#index-editing-data');
  var remove = $('#index-remove_function-input');

  if (indexData.attr('data-index-replace_function')) {
    replaceFunction
      .val(indexData.attr('data-index-replace_function'));
  } else {
    clearValues(replaceFunction).removeClass('ui-state-error');
  }

  var dialog = $("#index-replace-dialog")
    .dialog({
              autoOpen: false,
              modal: true,
              buttons: {
                "Save": function() {
                  $('.input').removeClass('ui-state-error');
        
                  // place holder for client side validation
                  var checkResult = true;

                  if (!remove.is(':checked')) {
                    if (replaceFunction.val().isBlank()) {
                      replaceFunction.addClass('ui-state-error');
                    } else {
                      replaceFunction.removeClass('ui-state-error');
                    }
        
                    if (checkResult) {
                      indexData
                        .attr('data-index-replace_function', 
                              replaceFunction.val());
                      $('#replace-function-message')
                        .text("This index has a replacement function.");
                    }
                  } else {
                    indexData.removeAttr('data-index-replace_function');
                    $('#replace-function-message').empty();
                  }

                  $(this).dialog("close");
              },
              "Cancel": function() {
                $(this).dialog("close");
              }
            },
            close: function() {
              clearValues(replaceFunction).removeClass('ui-state-error');
            }
            });

  return dialog;
};

function putDoc(doc) {
  if (!sessionStorage[doc._id]) {
    sessionStorage[doc._id] = JSON.stringify(doc);
  }
  
  return doc._id;
}

function getDoc(docId) {
  var doc = sessionStorage[docId];
  
  if (doc) {
    return JSON.parse(doc);
  } else {
    return null;
  }
}

function addProjectDialog() {
  var projectName = $("#project-name");
  var projectDescription = $("#project-description");
  var tips = $(".validate-tips");
  var allFields = $([]).add(projectName).add(projectDescription);
  
  var dialog = $("#add-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add project": function() {
        allFields.removeClass('ui-state-error');
        
        checkResult = checkLength(projectName, "project name", 1, 50, tips);
        
        if (checkResult) {
          $.ajax({
            type: "POST", 
            url: "projects/index",
            dataType: "json",
            contentType: "application/json",
            processData: false,
            data: JSON.stringify({name: projectName.val(), description: projectDescription.val()}),
            complete: function(req, status) {
              if (req.status == 201) {
                populateProjectsTable();
              } else {
                alert("An error occurred" + req.status);
              }
            }
          });
          $(this).dialog("close");
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      allFields.val('').removeClass('ui-state-error');
    }
  });
  
  return dialog;
}

function deleteProject(id) {
  if (confirm("Are you sure? This is permanent.")) {
    $.ajax({
      type: "DELETE", 
      url: "/projects/" + id,
      dataType: "json",
      contentType: "application/json",
      complete: function(req, status) {
        if (req.status == 204) {
          populateProjectsTable();
        } else {
          alert("An error occurred" + req.status);
        }
      }
    });
  }
}

function populateProjectsTable() {
  var url = "/projects/index";
  
  $.get(url, function(projects) {
    $('tbody').empty();
    $('tbody').html(projects);
    $('.configure-button').button({
      icons: {primary: "ui-icon-wrench"}
    });
    $('.delete-button').button({
      icons: {primary: "ui-icon-trash"}
    }).click(function(e) {
      id = $(e.target).attr("id");
      deleteProject(id);
      $('#delete-dialog').dialog("open");
    });
  });
}

$(function () {
  populateProjectsTable();
  
  $("#create-project").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    addProjectDialog().dialog("open");
  });
  
});