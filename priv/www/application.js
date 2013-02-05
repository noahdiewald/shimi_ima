var shimi = {};

// functions added to String
String.prototype.isBlank = function () {
  return ((/^\s*$/).test(this) && !(/\S/).test(this) && (this !== null));
};

String.prototype.trim = function () {
  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// functions added to Array
Array.prototype.trimAll = function () {
  return this.map(function (i) {
    return i.trim();
  }).filter(function (i) {
    return !i.match(/^$/);
  });
};

// General UI Stuff
shimi.panelToggle = (function () {
  var mod = {};

  mod.toggler = function (target) {
    var panel;

    if ($(target).attr('data-panel')) {
      panel = $('#' + $(target).attr('data-panel'));
    } else {
      panel = $(target).closest('.panel');
    }

    if (panel.css("display") === "none") {
      panel.css("display", "table-cell");
    } else {
      panel.css("display", "none");
    }

    return mod;
  };

  return mod;
})();
shimi.utils = function () {
  var mod = {};

  // safer(ish) string to number. The difference is that in this app
  // I am using '' if the string isn't a valid number.
  mod.stringToNumber = function (string) {
    if (typeof string === 'string' && !isNaN(string) && string !== '') {
      return string * 1;
    } else {
      return '';
    }
  };

  // A predicate function to detect blankness
  mod.isBlank = function (value) {
    return (((/^\s*$/).test(value)) || (value === null) || (value === undefined) || (typeof value === 'number' && isNaN(value)) || (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
  };

  mod.validID = function (id) {
    return !!id.match(/^[a-f0-9]{32}$/);
  };

  /**
   *
   *  Base64 encode / decode
   *  http://www.webtoolkit.info/
   *
   **/

  mod.Base64 = {

    // private property
    _keyStr: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

    // public method for encoding
    encode: function (input) {
      var output = "";
      var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
      var i = 0;

      input = mod.Base64._utf8_encode(input);

      while (i < input.length) {

        chr1 = input.charCodeAt(i++);
        chr2 = input.charCodeAt(i++);
        chr3 = input.charCodeAt(i++);

        enc1 = chr1 >> 2;
        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
        enc4 = chr3 & 63;

        if (isNaN(chr2)) {
          enc3 = enc4 = 64;
        } else if (isNaN(chr3)) {
          enc4 = 64;
        }

        output = output + this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) + this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);

      }

      return output;
    },

    // public method for decoding
    decode: function (input) {
      var output = "";
      var chr1, chr2, chr3;
      var enc1, enc2, enc3, enc4;
      var i = 0;

      input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

      while (i < input.length) {

        enc1 = this._keyStr.indexOf(input.charAt(i++));
        enc2 = this._keyStr.indexOf(input.charAt(i++));
        enc3 = this._keyStr.indexOf(input.charAt(i++));
        enc4 = this._keyStr.indexOf(input.charAt(i++));

        chr1 = (enc1 << 2) | (enc2 >> 4);
        chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
        chr3 = ((enc3 & 3) << 6) | enc4;

        output = output + String.fromCharCode(chr1);

        if (enc3 !== 64) {
          output = output + String.fromCharCode(chr2);
        }
        if (enc4 !== 64) {
          output = output + String.fromCharCode(chr3);
        }

      }

      output = mod.Base64._utf8_decode(output);

      return output;

    },

    // private method for UTF-8 encoding
    _utf8_encode: function (string) {
      string = string.replace(/\r\n/g, "\n");
      var utftext = "";

      for (var n = 0; n < string.length; n++) {

        var c = string.charCodeAt(n);

        if (c < 128) {
          utftext += String.fromCharCode(c);
        }
        else if ((c > 127) && (c < 2048)) {
          utftext += String.fromCharCode((c >> 6) | 192);
          utftext += String.fromCharCode((c & 63) | 128);
        }
        else {
          utftext += String.fromCharCode((c >> 12) | 224);
          utftext += String.fromCharCode(((c >> 6) & 63) | 128);
          utftext += String.fromCharCode((c & 63) | 128);
        }

      }

      return utftext;
    },

    // private method for UTF-8 decoding
    _utf8_decode: function (utftext) {
      var string = "";
      var i = 0;
      var c = 0;
      var c1 = 0;
      var c2 = 0;
      var c3 = 0;

      while (i < utftext.length) {

        c = utftext.charCodeAt(i);

        if (c < 128) {
          string += String.fromCharCode(c);
          i++;
        }
        else if ((c > 191) && (c < 224)) {
          c2 = utftext.charCodeAt(i + 1);
          string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
          i += 2;
        }
        else {
          c2 = utftext.charCodeAt(i + 1);
          c3 = utftext.charCodeAt(i + 2);
          string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
          i += 3;
        }

      }

      return string;
    }

  };


  return mod;

};
shimi.flash = function (title, body) {
  var mod = {};

  var f = function (flasher, title, body) {
    var fadeout = function () {
      flasher.fadeOut();
    };
    flasher.find('.notification-summary').text(title + ": ");
    flasher.find('.notification-message').text(body);
    var timeout = window.setTimeout(fadeout, 7000);
    flasher.fadeIn();
    flasher.find('.close').click(function () {
      window.clearTimeout(timeout);
      flasher.hide();
    });
  };

  mod.error = function () {
    f($('#notifications-main .ui-state-error'), title, body);

    return mod;
  };

  mod.highlight = function () {
    f($('#notifications-main .ui-state-highlight'), title, body);

    return mod;
  };

  return mod;
};
/*
 WARNING: OUT OF DATE
 
 h1. Data Attribute Key Value Stores
 
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
*/

/*
 Tail call optimization taken from Spencer Tipping's Javascript in Ten
 Minutes.

 For more information see:
 https://github.com/spencertipping/js-in-ten-minutes
*/

var identity = function (x) {
  return x;
};

Function.prototype.r = function () {
  return [this, arguments];
};

Function.prototype.t = function () {
  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape) {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
};

shimi.store = function (elem) {
  var mod = {};

  mod.get = function (key) {
    var prelim = elem.attr("data-" + key);

    if (prelim) {
      return prelim;
    }

    var getValue1 = function (key, elem, id) {
      var gid = elem.attr('data-group-id');
      var store = $('#' + gid);
      var val = store.attr('data-' + key);
      var next = store.attr('data-group-id');

      if (val === undefined && next !== undefined && gid !== next) {
        return getValue1.r(key, store, id);
      }

      return id.r(val);
    };

    return getValue1.t(key, elem, identity);
  };

  mod.get64 = function (key) {
    var retval = mod.get(key);
    retval = shimi.utils().Base64.decode(retval.replace(/"/g, '')).replace(/(^"|"$)/g, '');
    return retval;
  };

  mod.put = function (key, value) {
    var dataElem = elem.attr('data-group-id');
    $('#' + dataElem).attr('data-' + key, value);
  };

  mod.fs = function (key) {
    return mod.get("fieldset-" + key);
  };

  mod.f = function (key) {
    return mod.get("field-" + key);
  };

  mod.d = function (key) {
    return mod.get("document-" + key);
  };

  return mod;
};
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
 *   mypath.del(callback, context);
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

shimi.path = function (source, category, section) {
  var mod = {};
  var prefix;

  if (category) {
    prefix = category + "-";
  } else {
    prefix = "";
  }

  if (section) {
    mod.string = section + "/";
  } else {
    mod.string = "";
  }

  mod.category = category;
  mod.origin = source;
  mod.type = prefix + "path";
  mod.valid_components = ["doctype", "fieldset", "field"];
  var s = shimi.store(mod.origin);

  mod.valid_components.forEach(

  function (item) {
    mod[item] = (function () {
      var value = s.get(prefix + item);
      return value;
    })();
  });

  mod.rev = s.get(prefix + 'rev');

  mod.doctype = s.get(prefix + 'doctype');

  mod.send = function (object, method, callback, context) {
    shimi.form.send(mod.toString(), object, method, callback, context);
    return mod;
  };

  mod.put = function (object, callback, context) {
    mod.send(object, 'PUT', callback, context);
    return mod;
  };

  mod.post = function (object, callback, context) {
    mod.send(object, 'POST', callback, context);
    return mod;
  };

  mod.del = function (callback, context) {
    mod.send({}, 'DELETE', callback, context);
    return mod;
  };

  mod.toString = function () {
    var rev;

    var pathString =
    mod.string.concat(
    mod.valid_components.map(

    function (item) {
      var plural = item + "s";
      var value = mod[item];
      var retval = null;

      if (value) {
        retval = plural + "/" + value;
      } else if (item === mod.category) {
        retval = plural;
      }

      return retval;
    }).filter(

    function (item) {
      return (typeof item === "string" && !item.isBlank());
    }).join("/"));

    if (mod.rev) {
      pathString = pathString.concat("?rev=" + mod.rev);
    }

    return pathString;
  };

  return mod;
};
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

(function (jQuery) {
  jQuery.hotkeys = {
    version: "0.8",

    specialKeys: {
      8: "backspace",
      9: "tab",
      13: "return",
      16: "shift",
      17: "ctrl",
      18: "alt",
      19: "pause",
      20: "capslock",
      27: "esc",
      32: "space",
      33: "pageup",
      34: "pagedown",
      35: "end",
      36: "home",
      37: "left",
      38: "up",
      39: "right",
      40: "down",
      45: "insert",
      46: "del",
      96: "0",
      97: "1",
      98: "2",
      99: "3",
      100: "4",
      101: "5",
      102: "6",
      103: "7",
      104: "8",
      105: "9",
      106: "*",
      107: "+",
      109: "-",
      110: ".",
      111: "/",
      112: "f1",
      113: "f2",
      114: "f3",
      115: "f4",
      116: "f5",
      117: "f6",
      118: "f7",
      119: "f8",
      120: "f9",
      121: "f10",
      122: "f11",
      123: "f12",
      144: "numlock",
      145: "scroll",
      191: "/",
      224: "meta"
    },

    shiftNums: {
      "`": "~",
      "1": "!",
      "2": "@",
      "3": "#",
      "4": "$",
      "5": "%",
      "6": "^",
      "7": "&",
      "8": "*",
      "9": "(",
      "0": ")",
      "-": "_",
      "=": "+",
      ";": ": ",
      "'": "\"",
      ",": "<",
      ".": ">",
      "/": "?",
      "\\": "|"
    }
  };

  function keyHandler(handleObj) {
    // Only care when a possible input has been specified
    if (typeof handleObj.data !== "string") {
      return;
    }

    var origHandler = handleObj.handler,
        keys = handleObj.data.toLowerCase().split(" ");

    handleObj.handler = function (event) {
      // Don't fire in text-accepting inputs that we didn't directly bind to
      // MODIFIED FROM ORIGINAL
      //if ( this !== event.target && (/textarea|select/i.test( event.target.nodeName ) ||
      //      event.target.type === "text") ) {
      //	return;
      //}
      // Keypress represents characters, not special keys
      var special = event.type !== "keypress" && jQuery.hotkeys.specialKeys[event.which],
          character = String.fromCharCode(event.which).toLowerCase(),
          key, modif = "",
          possible = {};

      // check combinations (alt|ctrl|shift+anything)
      if (event.altKey && special !== "alt") {
        modif += "alt+";
      }

      if (event.ctrlKey && special !== "ctrl") {
        modif += "ctrl+";
      }

      // TODO: Need to make sure this works consistently across platforms
      if (event.metaKey && !event.ctrlKey && special !== "meta") {
        modif += "meta+";
      }

      if (event.shiftKey && special !== "shift") {
        modif += "shift+";
      }

      if (special) {
        possible[modif + special] = true;

      } else {
        possible[modif + character] = true;
        possible[modif + jQuery.hotkeys.shiftNums[character]] = true;

        // "$" can be triggered as "Shift+4" or "Shift+$" or just "$"
        if (modif === "shift+") {
          possible[jQuery.hotkeys.shiftNums[character]] = true;
        }
      }

      for (var i = 0, l = keys.length; i < l; i++) {
        if (possible[keys[i]]) {
          return origHandler.apply(this, arguments);
        }
      }
    };
  }

  jQuery.each(["keydown", "keyup", "keypress"], function () {
    jQuery.event.special[this] = {
      add: keyHandler
    };
  });

})(jQuery);
/*
 Simple plugin for manipulating input.
*/

(function ($) {

  $.fn.inputDisable = function () {
    this.val('');
    this.attr('disabled', 'disabled');
    this.addClass('ui-state-disabled');
    return this;
  };

  $.fn.inputEnable = function () {
    this.removeAttr('disabled');
    this.removeClass('ui-state-disabled');
    return this;
  };

})(jQuery);
// Object.keys compatibility from MDC
if (!Object.keys) {
  Object.keys = function (o) {
    if (o !== Object(o)) {
      throw new TypeError('Object.keys called on non-object');
    }
    var ret = [],
        p;
    for (p in o) {
      if (Object.prototype.hasOwnProperty.call(o, p)) {
        ret.push(p);
      }
    }
    return ret;
  };
}

// Reduce compatibility from MDC
if (!Array.prototype.reduce) {
  Array.prototype.reduce = function (fun /*, initialValue */ ) {
    "use strict";

    if (this === void 0 || this === null) {
      throw new TypeError();
    }

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function") {
      throw new TypeError();
    }

    // no value to return if no initial value and an empty array
    if (len === 0 && arguments.length === 1) {
      throw new TypeError();
    }

    var k = 0;
    var accumulator;
    if (arguments.length >= 2) {
      accumulator = arguments[1];
    }
    else {
      do {
        if (k in t) {
          accumulator = t[k++];
          break;
        }

        // if array contains no values, no initial value to return
        if (++k >= len) {
          throw new TypeError();
        }
      }
      while (true);
    }

    while (k < len) {
      if (k in t) {
        accumulator = fun.call(undefined, accumulator, t[k], k, t);
      }
      k++;
    }

    return accumulator;
  };
}
/**
 WARNING: OUT OF DATE
 
 == Click Dispatcher
 
 Each section of the application calls this function with an object
 composed of keys of CSS patterns of elements which should have click
 event actions bound to them and values of functions that will be
 called if a click event occurs and the key pattern matches.
 
 @dispatcher(patterns)@
 
 *More to come*
 */


shimi.dispatcher = function (patterns) {
  var d = function (e) {
    var target = $(e.target);

    Object.keys(patterns).forEach(function (pattern) {
      if (target.is(pattern)) {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

shimi.dblclickDispatch = function (e) {
  var searchui = shimi.searchui;

  var action = shimi.dispatcher({
    ".search-result-field-id a": function (t) {
      searchui.addField($(t).parent("h5"));
    },
    ".field-view b": function (t) {
      searchui.addField($(t).parent("li"));
    },
    ".field-container label span": function (t) {
      searchui.addField($(t).parent("label").parent("div"));
    },
    "#index-index-input-label": function () {
      searchui.addIndex();
    },
    ".panel > h2": function (t) {
      shimi.panelToggle.toggler(t);
    }
  });

  action(e);
};

shimi.clickDispatch = function (e) {
  var doctypeTab = shimi.doctypeTab;
  var charseqTab = shimi.charseqTab;
  var editui = shimi.editui;
  var viewui = shimi.viewui;
  var indexiu = shimi.indexiu;
  var setsui = shimi.setsui;
  var searchui = shimi.searchui;
  var fieldsets = shimi.fieldsets;
  var ieditui = shimi.ieditui;
  var form = shimi.form;
  var projectui = shimi.projectui;
  var fm = shimi.fm;

  var action = shimi.dispatcher({
    // Config
    ".edit-field-button": function (t) {
      doctypeTab.editField(t);
    },
    ".delete-field-button": function (t) {
      doctypeTab.deleteField(t);
    },
    ".add-field-button": function (t) {
      doctypeTab.addField(t);
    },
    ".edit-fieldset-button": function (t) {
      doctypeTab.editFieldset(t);
    },
    ".delete-fieldset-button": function (t) {
      doctypeTab.deleteFieldset(t);
    },
    ".add-fieldset-button": function (t) {
      doctypeTab.addFieldset(t);
    },
    ".delete-doctype-button": function (t) {
      doctypeTab.deleteDoctype(t);
    },
    ".edit-doctype-button": function (t) {
      doctypeTab.editDoctype(t);
    },
    ".touch-doctype-button": function (t) {
      doctypeTab.touchDoctype(t);
    },
    "#doctype-add-button": function (t) {
      doctypeTab.addDoctype(t);
    },
    ".delete-charseq-button": function (t) {
      charseqTab.del(t);
    },
    ".edit-charseq-button": function (t) {
      charseqTab.edit(t);
    },
    "#charseq-add-button": function (t) {
      charseqTab.add();
    },
    "#maintenance-upgrade-button": function (t) {
      shimi.upgradeButton(t);
    },

    // Documents
    ".add-button": function (t) {
      fieldsets.initFieldset(t);
    },
    ".remove-button": function (t) {
      fieldsets.removeFieldset(t);
    },
    "#save-document-button": function (t) {
      editui.save();
    },
    "#create-document-button": function (t) {
      editui.create();
    },
    "#clear-document-button": function (t) {
      editui.clear();
    },
    ".expander": function (t) {
      editui.toggleTextarea(t);
    },
    "label span.ui-icon-help": function (t) {
      editui.showHelpDialog(t);
    },
    "#document-edit-button": function (t) {
      viewui.edit(t);
    },
    "#document-delete-button": function (t) {
      viewui.confirmDelete();
    },
    "#document-restore-button": function (t) {
      viewui.confirmRestore();
    },
    "#document-view-tree > ul > li > b": function (t) {
      viewui.collapseToggle(t);
    },
    ".revision-link": function (t) {
      viewui.fetchRevision(t);
    },
    "#search-all-fields-switch a": function () {
      searchui.allFields();
    },
    ".search-field-item": function (t) {
      searchui.removeField(t);
    },
    "#document-search-exclude": function () {
      searchui.toggleExclusion();
    },
    "#document-search-invert": function () {
      searchui.toggleInversion();
    },
    ".select-results": function (t) {
      searchui.toggleSelection(t);
    },
    "#save-search-results a": function () {
      $("#new-set-target-input").val("search");
      $("#new-set-dialog").show();
    },
    "#save-set-results a": function () {
      $("#new-set-target-input").val("sets");
      $("#new-set-dialog").show();
    },
    "#new-set-save-button": function () {
      setsui.saveSelected($("#new-set-target-input").val());
    },
    "#select-all-set-elements": function (t) {
      setsui.toggleSelectAll(t);
    },
    ".view-document-link": function (t) {
      indexiu.load(t);
    },

    // Index Tool
    "#new-index-button": function (t) {
      ieditui.newCond();
    },
    ".remove-condition-button": function (t) {
      ieditui.remCond(t);
    },
    "#delete-index-button": function (t) {
      ieditui.del();
    },
    "#save-index-button": function (t) {
      ieditui.save();
    },
    "#replace-button": function (t) {
      ieditui.replace();
    },
    "#add-index-condition-button": function (t) {
      ieditui.addCond();
    },
    "#index-index-listing ul li a": function (t) {
      ieditui.init(t);
    },

    // Project
    "#create-project": function () {
      projectui.add().dialog("open");
    },
    ".project-delete-button": function (t) {
      projectui.del(t);
    },

    // File Manager
    "#up-dir": function () {
      fm.upDir();
    },
    "#root-dir": function () {
      fm.rootDir();
    },
    ".dir": function (t) {
      fm.goDir(t);
    },
    ".delete-file-button": function (t) {
      fm.deleteFile(t);
    },
    ".edit-file-button": function (t) {
      fm.editFile(t);
    },

    // General
    ".toggler": function (t) {
      form.toggle(t);
    },
    ".cancel-dialog": function (t) {
      form.cancelDialog(t);
    },
    "#panel-toggle li": function (t) {
      shimi.panelToggle.toggler(t);
    }
    //".remove-button": function(t) {$(t).parent().remove();}
  });

  action(e);
};

$(function () {
  $('body').click(function (e) {
    shimi.clickDispatch(e);
  });
  $('body').dblclick(function (e) {
    shimi.dblclickDispatch(e);
  });
});
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
shimi.index = function (args) {
  var mod = {};

  mod.get = function (startkey, startid, prevkeys, previds) {
    var url = args.url + '?';
    var indexId = args.indexId;
    var limitField = $('#index-limit');
    var limit = limitField.val() * 1;
    var target = args.target;
    var filterVal = JSON.stringify($('#index-filter').val());
    var state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!state.pks) {
      state.sk = window.btoa(window.unescape(window.encodeURIComponent(filterVal)));
      state.pks = [];
      state.pids = [];
    }

    if (state.sk) {
      url = url + '&startkey=' + window.escape(window.atob(state.sk));
      if (state.sid) {
        url = url + '&startkey_docid=' + state.sid;
      }
    }

    if (limit) {
      url = url + '&limit=' + (limit + 1);
    } else {
      limitField.val(25);
      url = url + '&limit=26';
    }

    if (indexId) {
      url = url + '&index=' + indexId;
    }

    shimi.form.send(url, false, 'GET', function (context, req) {
      mod.fill(req, state, target);
    }, this);

    return mod;
  };

  mod.fill = function (req, state, target) {
    target.html(req.responseText);

    $('#previous-index-page').click(function () {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    });

    $('#next-index-page').click(function () {
      var nextkey = $('#next-index-page').attr('data-startkey');
      var nextid = $('#next-index-page').attr('data-startid');
      var prevkey =
      $('#first-index-element').attr('data-first-key');
      var previd =
      $('#first-index-element').attr('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    });

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0) {
      $('#previous-index-page').hide();
    }

    // Disable the next button if we're at the end
    if ($('#next-index-page').attr('data-last-page')) {
      $('#next-index-page').hide();
    }

    return mod;
  };

  return mod;
};
shimi.form = (function () {
  var mod = {};

  mod.toggle = function (t) {
    var toggleElem;
    var target = $(t);

    if (target.attr('data-target')) {
      toggleElem = $('#' + target.attr('data-target'));
      toggleElem.toggle();
    }
    return mod;
  };

  mod.cancelDialog = function (t) {
    var target = $(t);
    var toggleElem;

    if (target.attr('data-dialog')) {
      toggleElem = $('#' + target.attr('data-target'));
      toggleElem.hide();
      mod.clear(undefined, toggleElem.find("form"));
    }
    return mod;
  };

  mod.clear = function (inputFields, form) {
    if (inputFields === undefined) {
      inputFields = $(form).find("input, select, textarea");
    }
    inputFields.each(function (index, elem) {
      var inputField = $(elem);

      if (!inputField.attr('data-retain')) {
        if (inputField.is(':checked')) {
          inputField.attr('checked', false);
        }
        inputField.val('');
      }
    });
    return inputFields;
  };

  mod.send = function (ajaxUrl, obj, method, completeFun, callContext) {
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
      complete: function (req, status) {
        if (req.status >= 200 && req.status < 300) {
          completeFun(this, req);
        } else if (req.status === 500) {
          shimi.flash("Unknown Server Error", "Please report that you received " + "this message").error();
        } else if (req.status >= 400) {
          var body = JSON.parse(req.responseText);
          var title = req.statusText;

          shimi.flash(title, body.fieldname + " " + body.message).error();
        }
      }
    });

    return true;
  };

  // Validation
  mod.updateTips = function (t, tips) {
    tips.text(t).addClass('ui-state-highlight');
    setTimeout(function () {
      tips.removeClass('ui-state-highlight', 1500);
    }, 500);

    return true;
  };

  mod.checkLength = function (o, n, min, max, tips) {
    if (o.val().length > max || o.val().length < min) {
      o.addClass('ui-state-error');
      mod.updateTips("Length of " + n + " must be between " + min + " and " + max + ".", tips);
      return false;
    } else {
      return true;
    }
  };

  mod.checkRegexp = function (o, regexp, n, tips) {
    if (!(regexp.test(o.val()))) {
      o.addClass('ui-state-error');
      mod.updateTips(n, tips);
      return false;
    } else {
      return true;
    }
  };

  // Date Picker
  mod.initDateFields = function () {
    $(".date").datepicker({
      dateFormat: "yy-mm-dd"
    });

    return true;
  };

  mod.fillOptionsFromUrl = function (url, selectElement, callback) {
    $.get(url, function (options) {
      selectElement.html(options);
      if (callback) {
        callback();
      }
    });

    return false;
  };

  return mod;
})();
shimi.sess = function () {
  var mod = {};

  mod.put = function (doc) {
    if (!window.sessionStorage[doc._id]) {
      window.sessionStorage[doc._id] = JSON.stringify(doc);
    }

    return doc._id;
  };

  mod.get = function (docId) {
    var doc = window.sessionStorage[docId];

    if (doc) {
      return JSON.parse(doc);
    } else {
      return null;
    }
  };

  return mod;
};
shimi.sets = (function () {
  var mod = {};

  var member = function (arr, x) {
    return arr.some(function (y) {
      return x[0] === y[0] && x[1] === y[1];
    });
  };

  var unique = function (x) {
    return x.reduce(function (acc, curr) {
      if (member(acc, curr)) {
        return acc;
      } else {
        return acc.concat([curr]);
      }
    }, []);
  };

  var union = function (xs, ys) {
    return unique(xs.concat(ys));
  };

  var intersection = function (xs, ys) {
    return xs.filter(function (x) {
      return member(ys, x);
    });
  };

  var relativeComplement = function (xs, ys) {
    return xs.filter(function (x) {
      return !member(ys, x);
    });
  };

  var processSet = function (set) {
    var name = set[0];
    var arr = unique(set[1]);
    var procSet = [name, arr];
    return procSet;
  };

  mod.union = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = union(setElemsA, setElemsB);
    return newSet;
  };

  mod.intersection = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = intersection(setElemsA, setElemsB);
    return newSet;
  };

  // To be basically read as A minus B.
  mod.relativeComplement = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = relativeComplement(setElemsA, setElemsB);
    return newSet;
  };

  mod.symetricDifference = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = union(relativeComplement(setElemsA, setElemsB), relativeComplement(setElemsB, setElemsA));
    return newSet;
  };

  mod.getSets = function () {
    var curr = window.sessionStorage.getItem("sets");
    var retval = [];

    if (curr !== null) {
      retval = JSON.parse(curr);
    }

    return retval;
  };

  mod.getSet = function (setName) {
    var retval;
    var curr = mod.getSets();
    retval = curr.filter(function (x) {
      return x[0] === setName;
    })[0];
    return retval;
  };

  mod.removeSet = function (setName) {
    var nnew;
    var curr = mod.getSets();
    nnew = curr.filter(function (x) {
      return x[0] !== setName;
    });
    mod.setSets(nnew);
    return mod;
  };

  mod.getSetNames = function () {
    var curr = mod.getSets();
    return curr.map(function (x) {
      return x[0];
    });
  };

  mod.setSets = function (nnew) {
    var procSets;
    if (Array.isArray(nnew)) {
      procSets = nnew.map(function (x) {
        return processSet(x);
      });
      window.sessionStorage.setItem("sets", JSON.stringify(procSets));
    } else {
      window.sessionStorage.settem("sets", "[]");
    }

    return mod;
  };

  mod.setSet = function (nnew) {
    if (Array.isArray(nnew) && nnew.length === 2) {
      var curr = mod.getSets();
      var newName = nnew[0];
      var filtered = curr.filter(function (x) {
        return x[0] !== newName;
      });
      mod.setSets(filtered.concat([nnew]));
    }
    return mod;
  };

  return mod;
})();
// Dialog for manipulating doctypes
shimi.charseqDialog = function (values) {
  var f = shimi.charseqElems.get(values);

  var dialog = $("#charseq-dialog").dialog({
    width: 650,
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function (context) {
          shimi.charseqTab.init();
          $(context).dialog("close");
        };

        if (values && values.rev) {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }

        shimi.form.send(url, obj, method, complete, this);
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};
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

shimi.charseqElems = (function () {
  var mod = {};

  mod.attrs = ["description", "characters", "name", "sort_ignore", "locale", "tailoring", "vowels", "consonants", "ietf_tag", "iso639_tag", "charseq", "rev"];

  mod.get = function (values) {
    var cObj = {};

    cObj.attrs = mod.attrs;

    cObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        cObj[field].val(source[field]);
      });
      return cObj;
    };

    cObj.getCharseqInputVals = function () {
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
      };
      return valObj;
    };

    cObj.parse = function (val) {
      if (val && !val.isBlank()) {
        return JSON.parse(val);
      } else {
        return [];
      }
    };

    cObj.clear = function () {
      shimi.form.clear($('#charseq-dialog .input')).removeClass('ui-state-error');
      return cObj;
    };

    cObj.attrs.forEach(function (item) {
      cObj[item] = $('#charseq-' + item + '-input');
    });

    if (values) {
      cObj.copyValues(values);
    }

    return cObj;
  };

  return mod;
})();
shimi.charseqTab = (function () {
  var mod = {};

  mod.add = function () {
    shimi.charseqDialog().dialog("open");
    return mod;
  };

  mod.edit = function (target) {
    var oldobj = {};
    var attrs = shimi.charseqElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get64('charseq-' + item);
    });
    shimi.charseqDialog(oldobj).dialog("open");

    return mod;
  };

  mod.del = function (target) {
    var s = shimi.store(target);
    var id = s.get('charseq-charseq');
    var rev = s.get('charseq-rev');
    var url = 'config/charseqs/' + id + '?rev=' + rev;
    var complete = function () {
      mod.init();
    };

    if (window.confirm("Are you sure? This is permanent.")) {
      shimi.form.send(url, {}, 'DELETE', complete, this);
    }

    return mod;
  };

  mod.init = function () {
    var tabs = $("#charseq-tabs");
    var heads = $("#charseq-tabs-headings");
    var url = "config/charseqs";

    tabs.tabs();

    $.get(url, function (charseqs) {
      heads.empty();
      //$("#charseq-tabs-headings + .ui-tabs-panel").remove();
      heads.find('.ui-tabs-panel').remove();
      tabs.tabs("destroy");
      heads.html(charseqs);

      tabs.tabs();
    });

    return mod;
  };

  return mod;
})();
shimi.upgradeButton = function (target) {
  $.post("config/upgrade");
  window.alert("Upgrade In Progress");
};

shimi.initTabs = function () {
  shimi.doctypeTab.init();
  $("#main-tabs").tabs();
  shimi.charseqTab.init();

  return true;
};
// Dialog for manipulating doctypes
shimi.doctypeDialog = function (url, values) {
  var f = shimi.doctypeElems.get(values);

  if (values.rev && !values.rev.isBlank()) {
    f.doctype.attr('disabled', 'disabled');
  } else {
    f.doctype.removeAttr('disabled');
  }

  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        var obj = f.getDoctypeInputVals();
        var complete = function (context) {
          shimi.doctypeTab.init();
          $(context).dialog("close");
        };

        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.doctype;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};
// Returns an object with references to add/edit doctype dialog
// field elements with helper functions. 
shimi.doctypeElems = (function () {
  var mod = {};

  mod.attrs = ["description", "doctype", "rev"];

  mod.get = function (values) {
    var fObj = {};

    fObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        fObj[field].val(source[field]);
      });
      return fObj;
    };

    fObj.getDoctypeInputVals = function () {
      var valObj = {
        "category": "doctype",
        "description": fObj.description.val(),
        "_id": fObj.doctype.val()
      };
      return valObj;
    };

    fObj.clear = function () {
      shimi.form.clear($('#doctype-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    mod.attrs.forEach(function (item) {
      fObj[item] = $('#doctype-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();
shimi.doctypeTab = (function () {
  var mod = {};

  var cpath = function (source, category) {
    return shimi.path(source, category, "config");
  };

  // Populate the listing of fields
  mod.initFields = function (path) {
    path.field = false;

    $.get(path.toString(), function (fields) {
      var fieldContainer = $("#fields-" + path.fieldset);
      fieldContainer.empty();
      fieldContainer.html(fields);
    });

    return mod;
  };

  // Populate the listing of fieldsets
  mod.initFieldsets = function (url) {
    $.get(url.toString(), function (fieldsets) {
      var fieldsetContainer = $("#fieldsets-" + url.doctype);

      fieldsetContainer.empty();
      fieldsetContainer.accordion("destroy");
      fieldsetContainer.html(fieldsets);

      fieldsetContainer.accordion({
        autoHeight: false,
        collapsible: true,
        active: false
      });
    });
  };

  // populate the tabs listing the doctypes
  mod.init = function () {
    var url = "config/doctypes";

    $("#doctype-tabs").tabs();

    $.get(url, function (doctypes) {
      var fieldsetDoctype = $("#fieldset-doctype-input");

      $("#doctype-tabs-headings").empty();
      $("#doctype-tabs-headings + .ui-tabs-panel").remove();
      $("#doctype-tabs").tabs("destroy");
      $("#doctype-tabs-headings").html(doctypes);

      var loadFun = function (event, ui) {
        var source = $(ui.panel).children('div[data-fieldset-doctype]');
        var fieldsetsPath = shimi.path(source, "fieldset", "config");
        mod.initFieldsets(fieldsetsPath);
      };

      $("#doctype-tabs").tabs({
        load: function (e, ui) {
          loadFun(e, ui);
        }
      });
    });
  };

  // Button that opens a dialog for editing a field
  mod.editField = function (target) {
    var url = cpath(target, "field");
    var oldobj = {};
    var attrs = shimi.fieldElems.attrs;
    var charseqUrl = "config/charseqs?as=options";

    $.get(charseqUrl, function (charseqs) {
      $("#field-charseq-input").html(charseqs);
      attrs.forEach(function (item) {
        oldobj[item] = shimi.store(target).get('field-' + item);
      });
      shimi.fieldDialog(url, oldobj).dialog("open");
    });
  };

  // Button that opens a dialog for deleting a field
  mod.deleteField = function (target) {
    var answer = window.confirm("Are you sure? This is permanent.");

    if (answer) {
      var url = cpath(target, "field");
      var complete = function () {
        url.field = false;
        url.rev = false;

        mod.initFields(url);
      };
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a field
  mod.addField = function (target) {
    var url = cpath(target, "field");
    var charseqUrl = "config/charseqs?as=options";

    $.get(charseqUrl, function (charseqs) {
      $("#field-charseq-input").html(charseqs);
      shimi.fieldDialog(url, {
        fieldset: url.fieldset,
        doctype: url.doctype
      }).dialog("open");
    });
  };

  // Button that opens a dialog for editing a fieldset
  mod.editFieldset = function (target) {
    var url = cpath(target, "fieldset");
    var oldobj = {};
    var attrs = shimi.fieldsetElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get('fieldset-' + item);
    });

    shimi.fieldsetDialog(url, oldobj).dialog("open");
  };

  // Button that opens a dialog for deleting a fieldset
  mod.deleteFieldset = function (target) {
    var url = cpath(target, "fieldset");

    var complete = function () {
      url.fieldset = false;
      url.rev = false;
      mod.initFieldsets(url);
    };

    if (window.confirm("Are you sure? This is permanent.")) {
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a fieldset
  mod.addFieldset = function (target) {
    var url = cpath(target, "fieldset");
    shimi.fieldsetDialog(url, {
      doctype: url.doctype
    }).dialog("open");
  };

  mod.editDoctype = function (target) {
    var url = cpath(target, "doctype");
    var oldobj = {};
    var attrs = shimi.doctypeElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get('doctype-' + item);
    });
    shimi.doctypeDialog(url, oldobj).dialog("open");
  };

  mod.touchDoctype = function (target) {
    var docid = shimi.store(target).get("doctype-doctype");
    $.post("config/doctypes/" + docid + "/touch");
    window.alert("Touch In Progress");
  };

  mod.deleteDoctype = function (target) {
    var url = cpath(target, "doctype");
    var complete = function () {
      url.doctype = false;
      url.rev = false;
      mod.init();
    };

    if (window.confirm("Are you sure? This is permanent.")) {
      url.del(complete, this);
    }
  };

  mod.addDoctype = function (target) {
    var url = cpath(target, "doctype");
    shimi.doctypeDialog(url, {}).dialog("open");
  };

  return mod;
})();
// Dialog for manipulating fields
shimi.fieldDialog = function (url, values) {
  var f = shimi.fieldElems.get(values);

  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        var obj = f.clearDisabled().getFieldInputVals();
        var complete = function (context) {
          shimi.doctypeTab.initFields(url);
          $(context).dialog("close");
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};
// Returns an object with references to add/edit fields dialog
// field elements with helper functions. 
shimi.fieldElems = (function () {
  var mod = {};

  mod.attrs = ["name", "label", "order", "description", "subcategory", "head", "reversal", "default", "required", "allowed", "source", "max", "min", "regex", "doctype", "fieldset", "charseq", "rev", "field"];

  mod.get = function (values) {
    var fObj = {};

    fObj.attrs = mod.attrs;

    // These are fields that only some field subcategories use.
    // Below you'll see them being disabled and reenabled depending on the
    // chosen subcategory.
    fObj.notDefault = function () {
      return [fObj.charseq, fObj.allowed, fObj.source, fObj.min, fObj.max, fObj.regex];
    };

    fObj.disable = function () {
      fObj.notDefault().forEach(function (field) {
        field.attr("disabled", "disabled");
      });
      return fObj;
    };

    fObj.clearDisabled = function () {
      fObj.notDefault().forEach(function (field) {
        if (field.attr("disabled")) {
          field.val("");
        }
      });
      return fObj;
    };

    fObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]')) {
          if (source[field] === "true") {
            fObj[field].attr('checked', true);
          }
        }
      });
      return fObj;
    };

    fObj.getFieldInputVals = function () {
      var valObj = {
        "category": "field",
        "name": fObj.name.val(),
        "label": fObj.label.val(),
        "default": fObj.decodeDefaults(fObj.subcategory.val(), fObj["default"].val()),
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

    fObj.clear = function () {
      shimi.form.clear($('#field-dialog .input')).removeClass('ui-state-error');
      fObj.disable();
      return fObj;
    };

    fObj.decodeBound = function (subcategory, bound) {
      if (subcategory === "date") {
        return bound;
      } else {
        return shimi.utils().stringToNumber(fObj.min.val());
      }
    };

    fObj.decodeSource = function (subcategory, source) {
      if (subcategory === "file") {
        return source.split("/").trimAll();
      } else {
        return source;
      }
    };

    fObj.decodeDefaults = function (subcategory, defaults) {
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

    fObj.displayFields = function (subcategory) {
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

    fObj.attrs.forEach(function (item) {
      fObj[item] = $('#field-' + item + '-input');
    });

    fObj.copyValues(values);
    fObj.displayFields(fObj.subcategory.val());

    fObj.subcategory.change(function () {
      fObj.displayFields(fObj.subcategory.val());
    });

    return fObj;
  };

  return mod;
})();
shimi.fieldsetDialog = function (url, values) {
  var f = shimi.fieldsetElems.get(values);

  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        var obj = f.getFieldsetInputVals();
        var complete = function (context) {
          url.fieldset = false;
          url.rev = false;

          shimi.doctypeTab.initFieldsets(url);
          $(context).dialog("close");
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.fieldset;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};
// Returns an object with references to add/edit fieldset dialog
// field elements with helper functions. 
shimi.fieldsetElems = (function () {
  var mod = {};

  mod.attrs = ["name", "label", "order", "description", "doctype", "rev", "multiple", "collapse", "fieldset"];

  mod.get = function (values) {
    var fObj = {};

    fObj.attrs = mod.attrs;

    fObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]')) {
          if (source[field] === "true") {
            fObj[field].attr('checked', true);
          }
        }
      });
      return fObj;
    };

    fObj.getFieldsetInputVals = function () {
      var valObj = {
        "category": "fieldset",
        "name": fObj.name.val(),
        "label": fObj.label.val(),
        "order": fObj.order.val() * 1,
        "description": fObj.description.val(),
        "doctype": fObj.doctype.val(),
        "multiple": fObj.multiple.is(':checked'),
        "collapse": fObj.collapse.is(':checked')
      };
      return valObj;
    };

    fObj.clear = function () {
      shimi.form.clear($('#fieldset-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    fObj.attrs.forEach(function (item) {
      fObj[item] = $('#fieldset-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();
shimi.loadHash = function (urlHash) {
  if (urlHash) {
    shimi.viewui.get(urlHash);
  }
  return false;
};

shimi.jumpForm = function () {
  $('#view-jump').live("submit", function () {
    return false;
  });
  $('#view-jump-id').live("keypress", function (e) {
    if (e.which === 13) {
      var docid = $('#view-jump-id').val();
      shimi.viewui.get(docid);
    }
    return true;
  });
};

shimi.setForm = function () {
  $('#new-set-form').on("submit", function () {
    return false;
  });
  $('#document-sets-form').on("keydown", function (e) {
    if (e.which === 13) {
      shimi.setsui.performOp();
      return false;
    }
    return true;
  });
  shimi.setsui.updateSelection();
};

shimi.searchForm = function () {
  shimi.searchui.loadSearchVals();
  $('#document-search-form').on("submit", function () {
    return false;
  });
  $('#document-search-term').on("keydown", function (e) {
    if (e.which === 13) {
      shimi.searchui.getSearch();
      return false;
    }
    return true;
  });
};
// Edit pane UI elements
shimi.editui = (function () {
  var mod = {};

  // Imports
  var store = shimi.store;
  var flash = shimi.flash;

  // UI Elements
  var saveButton = function () {
    return $('#save-document-button');
  };
  var createButton = function () {
    return $('#create-document-button');
  };
  var editButton = function () {
    return $('#document-edit-button');
  };

  var keyboard = function () {
    var inputable = 'input, select';
    var t = $('#edit-tabs');

    var selectInput = function () {
      var cur = t.find('.ui-tabs-selected a').attr('href');
      $(cur).find(inputable + ", textarea").first().focus();
    };

    $(document).bind('keydown', 'Alt+p', function (e) {
      var totaltabs = t.tabs('length');
      var selected = t.tabs('option', 'selected');

      if (selected !== 0) {
        t.tabs('select', selected - 1);
        selectInput();
      } else {
        t.tabs('select', totaltabs - 1);
        selectInput();
      }

      return false;
    });

    $(document).bind('keydown', 'Alt+n', function (e) {
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

    $(document).live('keydown', 'Alt+c', function (e) {
      var active = $(document.activeElement);
      mod.showCommandDialog(active);
      return true;
    });

    $('#edit-command-form').live("submit", function (e) {
      return false;
    });

    $('#edit-command-input').live("keydown", function (e) {
      if (e.which === 13) {
        var command = $('#edit-command-input').val();
        var restoreFocus = true;
        $('#command-dialog').dialog("close");

        switch (command) {
        case "w":
        case "clear":
          mod.clear();
          break;
        case "c":
        case "create":
          mod.create();
          break;
        case "s":
        case "save":
          mod.save();
          break;
        case "d":
        case "delete":
          $("#document-view").show();
          if ($("#document-delete-button").css("display") !== "none") {
            $("#document-delete-button").click();
          }
          break;
        case "e":
        case "edit":
          $("#document-view").show();
          if ($("#document-edit-button").css("display") !== "none") {
            $("#document-edit-button").click();
            restoreFocus = false;
          }
          break;
        case "r":
        case "restore":
          $("#document-view").show();
          if ($("#document-restore-button").css("display") !== "none") {
            $("#document-restore-button").click();
          }
          break;
        }

        if (restoreFocus) {
          $('#' + $('#command-dialog').attr('data-last-active')).focus();
        } else {
          selectInput();
        }
      }

      return true;
    });

    $("#edit-document-form input").live('keydown', function (e) {
      if (e.which === 13) {
        if ($("#save-document-button").css("display") === "none") {
          mod.create();
        } else {
          mod.save();
        }
      }
      return true;
    });

    $("#edit-document-form textarea").on('keydown', 'Alt+x', function (e) {
      mod.toggleTextarea($(e.target));
      return false;
    });

    return mod;
  };

  var validationError = function (req) {
    var body = JSON.parse(req.responseText);
    var title = req.statusText;

    var invalid = $('[data-field-instance=' + body.instance + ']');
    var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');

    invalidTab.addClass('ui-state-error');
    invalid.addClass('ui-state-error');

    flash(title, body.fieldname + " " + body.message).error();

    return mod;
  };

  var instances = function () {
    var text = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
    var makeInstance = function () {
      return text.map(function () {
        return text[Math.floor(Math.random() * text.length)];
      }).join('');
    };

    $("[data-field-instance]").each(

    function (index, item) {
      var newInstance = makeInstance();
      $(item).first().attr('data-field-instance', newInstance);
      $(item).first().attr('data-group-id', newInstance);
      $(item).first().attr('id', newInstance);
      $(item).first().next('.expander').attr('data-group-id', newInstance);
      $(item).first().next().next('.expander').attr('data-group-id', newInstance);
    });

    return mod;
  };

  mod.init = function () {
    var url = "documents/edit";

    $.get(url, function (documentEditHtml) {

      $('#document-edit').html(documentEditHtml);
      $('#edit-tabs').tabs();
      keyboard();
      shimi.fieldsets.initFieldsets();
    });

    return mod;
  };

  mod.afterFreshRefresh = function () {
    afterRefresh();

    return mod;
  };

  mod.afterEditRefresh = function () {
    var sharedAttrs = ['data-document-id', 'data-document-rev'];

    sharedAttrs.forEach(function (elem) {
      saveButton().attr(elem, editButton().attr(elem));
    });

    saveButton().show();
    afterRefresh();

    return mod;
  };

  var afterRefresh = function () {
    shimi.form.initDateFields();
    instances();

    return mod;
  };

  mod.resetFields = function () {
    $('.field').each(function (index) {
      var field = $(this);
      var thedefault = field.attr('data-field-default');

      if (thedefault && thedefault !== '') {
        if (field.is('select.multiselect')) {
          field.val(thedefault.split(","));
        } else if (field.is('input.boolean')) {
          field.attr('checked', thedefault === true);
        } else {
          field.val(thedefault);
        }
      } else {
        field.val('');
        field.removeAttr('checked');
      }
    });

    return mod;
  };

  mod.save = function () {
    if (saveButton().hasClass('oldrev')) {
      if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
        return false;
      }
    }

    var body;
    var title;
    var s = store(saveButton());
    var root = $('#edit-document-form');
    var document = s.d("document");
    var rev = s.d("rev");
    var url = "./documents/" + document + "?rev=" + rev;
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };

    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton().hide();
    $.extend(obj, shimi.fieldsets.fieldsetsToObject(root));

    $.ajax({
      type: "PUT",
      url: url,
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function (req, status) {
        if (req.status === 204 || req.status === 200) {
          title = "Success";
          body = "Your document was saved.";
          shimi.viewui.get(document);
          shimi.indexiu.get(skey, sid);
          flash(title, body).highlight();
          saveButton().removeClass('oldrev').show();
        } else if (req.status === 403) {
          validationError(req);
          saveButton().show();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          flash(title, body.message).error();
          saveButton().hide();
        }
      }
    });
  };

  mod.create = function () {
    var s = store(createButton());
    var root = $('#edit-document-form');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };

    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    createButton().hide();
    $.extend(obj, shimi.fieldsets.fieldsetsToObject(root));

    var postUrl = $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function (req, status) {
        if (req.status === 201) {
          var title = "Success";
          var body = "Your document was created.";
          var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);

          saveButton().hide().attr('disabled', 'true');
          $('.fields').remove();
          shimi.fieldsets.initFieldsets();
          shimi.viewui.get(documentId);
          shimi.indexiu.get(skey, sid);
          flash(title, body).highlight();
          createButton().show();
        } else if (req.status === 403) {
          validationError(req);
          createButton().show();
        }
      }
    });
  };

  mod.clear = function () {
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton().hide().attr('disabled', 'disabled');
    $('.fields').remove();
    shimi.fieldsets.initFieldsets();
  };

  mod.showHelpDialog = function (target) {
    if (target.is('.label-text')) {
      target = target.parent('label').find('.ui-icon-help');
    }

    $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));

    return mod;
  };

  mod.showCommandDialog = function (context) {
    $('#edit-command-input').val("");
    $('#edit-dialog').attr('data-last-active', context.id);
    var commandDialog = $('#command-dialog').dialog({
      modal: true,
      close: function () {
        $('#edit-command-input').val("");
        context.focus();
      }
    });
    commandDialog.dialog('open');
    return mod;
  };

  mod.toggleTextarea = function (target) {
    var textarea = $('#' + target.attr('data-group-id'));

    if (target.attr("id") === textarea.attr("data-group-id")) {
      textarea.toggleClass('expanded');
      textarea.next().next("span").toggleClass('expanded');
    } else {
      textarea.toggleClass('expanded');
      target.toggleClass('expanded');
    }

    return mod;
  };

  return mod;
})();
shimi.fieldsets = (function () {
  var mod = {};
  var store = shimi.store;
  var utils = shimi.utils();

  var fsContainer = function (id) {
    return $("#container-" + id);
  };

  var dpath = function (source, category) {
    var url = shimi.path(source, category);
    url.doctype = false;
    return url;
  };

  var ifStoredElse = function (key, success, otherwise) {
    var item = null;

    item = sessionStorage.getItem(key);

    if (item) {
      success(item);
    } else {
      $.get(key, otherwise);
    }
  };

  // Convert field values to an object that can be converted to JSON
  var fieldsToObject = function (fields, index) {
    fields = fields.children('.field-container').children('.field');
    var obj = {
      fields: []
    };

    fields.each(function (i, field) {
      field = $(field);
      var s = store(field);

      obj.fields[i] = {
        id: s.f("field"),
        name: s.f("name"),
        label: s.f("label"),
        head: s.f("head") === "true",
        reversal: s.f("reversal") === "true",
        required: s.f("required") === "true",
        min: dateOrNumber(s.f("subcategory"), s.f("min")),
        max: dateOrNumber(s.f("subcategory"), s.f("max")),
        instance: s.f("instance"),
        charseq: s.f("charseq"),
        regex: s.f("regex"),
        order: s.f("order") * 1,
        subcategory: s.f("subcategory"),
        value: getFieldValue(field)
      };

      if (index >= 0) {
        obj.fields[i].index = index;
      }
    });

    return obj;
  };

  var dateOrNumber = function (subcategory, fieldvalue) {
    if (subcategory === "date") {
      return fieldvalue;
    } else {
      return utils.stringToNumber(fieldvalue);
    }
  };

  // Get the correct value for a boolean that can be null
  var getOpenboolean = function (value) {
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

    return value;
  };

  // Get a number from a string. Blanks are returned as an empty string.
  var getNumber = function (value) {
    if (utils.isBlank(value)) {
      value = '';
    } else if (!isNaN(value)) {
      value = value * 1;
    }

    return value;
  };

  // Items in multiple select lists are URL encoded
  var getMultiple = function (value) {
    if (value) {
      value = value.map(function (v) {
        return getEncoded(v);
      });
    } else {
      value = null;
    }

    return value;
  };

  // Items in select lists are URL encoded
  var getEncoded = function (value) {
    return window.decodeURIComponent(value.replace(/\+/g, " "));
  };

  // Get the value from a field using the subcategory to ensure
  // that the value has the correct type and is properly formatted.
  var getFieldValue = function (field) {
    var value;

    switch (store(field).f("subcategory")) {
    case "boolean":
      value = field.is('input:checkbox:checked');
      break;
    case "openboolean":
      value = getOpenboolean(field.val());
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
  };

  var initFields = function (container, callback) {
    var url = dpath(container, "field");
    var section = container.children('.fields').last();
    var prependIt = function (data) {
      section.prepend(data);
      if (callback) {
        callback(section);
      }

      shimi.editui.afterFreshRefresh();
    };
    var storeIt = function (data) {
      sessionStorage.setItem(url, data);
      prependIt(data);
    };

    ifStoredElse(url.toString(), prependIt, storeIt);

    return true;
  };

  var fillMultiFieldsets = function (vfieldset) {
    vfieldset = $(vfieldset);
    var id = store(vfieldset).fs("fieldset");
    var container = $('#container-' + id);
    var url = dpath(vfieldset, "fieldset");

    container.html('');

    vfieldset.find('.multifield').each(function (i, multifield) {
      mod.initFieldset(container, function (fieldset) {
        fillFields($(multifield), fieldset);
      });
    });
  };

  var fillNormalFieldsets = function (vfieldset) {
    fillFields($(vfieldset));
  };

  var fillFields = function (container, context) {
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    $('#save-document-button').show();

    container.find('.field-view').each(function (i, field) {
      var value = $(field).attr('data-field-value');
      var id = $(field).attr('data-field-field');

      if (!context) {
        context = $('body');
      }

      setFieldValue(context.find('.field[data-field-field=' + id + ']'), value);
    });
  };

  var loadLabels = function (project, doctype, url) {
    $.getJSON(url, function (data) {
      sessionStorage.setItem(project + "_" + doctype + "_labels", JSON.stringify(data));
    });
  };

  var loadDoctype = function (project, doctype) {
    $.getJSON("./", function (data) {
      sessionStorage.setItem(project + "_" + doctype + "_info", JSON.stringify(data));
    });
  };

  var setFieldValue = function (field, value) {
    if (field.is('input.boolean')) {
      field.attr("checked", value === "true");
    } else if (value && field.is('select.multiselect')) {
      field.val(value.split(","));
    } else if (value && (field.is('input.text') || field.is('select.file'))) {
      field.val(decodeURIComponent(value.replace(/\+/g, " ")));
    } else if (field.is('textarea.textarea')) {
      field.val(decodeURIComponent(value.replace(/\+/g, " ")));
    } else {
      field.val(value);
    }
  };

  mod.initFieldset = function (fieldset, callback) {
    var url = dpath($(fieldset), "fieldset").toString();
    var id = store($(fieldset)).fs("fieldset");
    var container = $('#container-' + id);
    var appendIt = function (data) {
      container.append(data);
      initFields(container, callback);
    };
    var storeIt = function (data) {
      sessionStorage.setItem(url, data);
      appendIt(data);
    };

    ifStoredElse(url.toString(), appendIt, storeIt);

    return false;
  };

  // Before submitting the form, the form data is converted into an object
  // that can be serialized to JSON. This begins with the fieldsets.
  mod.fieldsetsToObject = function (root) {
    var obj = {
      fieldsets: []
    };

    root.find('fieldset').each(function (i, fieldset) {
      fieldset = $(fieldset);
      var s = store(fieldset);

      var fields;

      var fsObj = {
        id: s.fs("fieldset"),
        multiple: s.fs("multiple") === "true",
        collapse: s.fs("collapse") === "true",
        name: s.fs("name"),
        label: s.fs("label"),
        order: s.fs("order") * 1
      };

      fields = fsContainer(fsObj.id).children('.fields');

      if (!fsObj.multiple) {
        $.extend(fsObj, fieldsToObject(fields.first()));
      } else {
        fsObj.multifields = [];

        fields.each(function (j, field) {
          field = $(field);

          fsObj.multifields[j] = fieldsToObject(field, j);
        });
      }

      obj.fieldsets[i] = fsObj;
    });

    return obj;
  };

  mod.initFieldsets = function () {
    var container = $("#create-document-button");
    var s = store(container);
    var doctype = s.d("doctype");
    var project = store($("#container")).get("project-id");
    var versionKey = doctype + "_version";
    var oldVersion = sessionStorage.getItem(versionKey);
    var curVersion = s.d("version");

    if (oldVersion !== curVersion) {
      sessionStorage.clear();
      var labelsUrl = shimi.path(container, "fieldset").toString();
      loadDoctype(project, doctype);
      loadLabels(project, doctype, labelsUrl);
    }

    sessionStorage.setItem(versionKey, curVersion);

    $('fieldset').each(function (i, fieldset) {
      var fs = store($(fieldset));

      if (fs.fs("multiple") === "false") {
        mod.initFieldset(fieldset, false);
      }
    });

    return mod;
  };

  mod.removeFieldset = function (target) {
    target.parent().remove();
  };

  mod.fillFieldsets = function () {
    $('.fieldset-view').each(function (i, fieldset) {
      if (store($(fieldset)).fs("multiple") === "true") {
        fillMultiFieldsets(fieldset);
      } else {
        fillNormalFieldsets(fieldset);
      }
    });

    shimi.editui.afterEditRefresh();

    return mod;
  };

  return mod;
})();
shimi.indexiu = (function () {
  var mod = {};
  var store = shimi.store;
  var flash = shimi.flash;
  var index = shimi.index;

  mod.get = function (startkey, startid, prevkeys, previds) {
    var url = 'documents/index';
    var indexId = $('#index-index-input').val();
    var target = $('#index-listing');

    index({
      url: url,
      indexId: indexId,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  mod.iOpts = function () {
    var url = "/projects/project-" + $('#container').attr('data-project-id') + "/indexes?as=options";

    $.get(url, function (data) {
      $('#index-index-input').html(data);
    });

    return mod;
  };

  mod.load = function (target) {
    var id = $(target).attr('href').slice(1);
    $("#document-view").html("<em>Loading...</em>");
    shimi.editui.clear();
    shimi.viewui.get(id);

    return mod;
  };

  return mod;
})();
shimi.searchui = (function () {
  var mod = {};
  var utils = shimi.utils();
  var sets = shimi.sets;
  var setsui = shimi.setsui;
  var localStorage = window.localStorage;
  var searchIndex = function () {
    return $('#document-search-index');
  };
  var searchIndexLabel = function () {
    return $('#search-index-label');
  };
  var searchTerm = function () {
    return $('#document-search-term');
  };
  var searchFields = function () {
    return $('#document-search-field');
  };
  var searchFieldsLabel = function () {
    return $('#search-field-label');
  };
  var searchExclude = function () {
    return $('#document-search-exclude');
  };
  var searchInvert = function () {
    return $('#document-search-invert');
  };
  var searchAll = function () {
    return $('#search-all-fields-switch');
  };
  var searchListing = function () {
    return $('#search-listing');
  };
  var getDoctype = function () {
    return shimi.store($("#all-document-container")).d("doctype");
  };
  var getProject = function () {
    return shimi.store($("#container")).get("project-id");
  };
  var getIdentifier = function () {
    return getProject() + "_" + getDoctype();
  };
  var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

  var indexVal = function () {
    var val = $("#index-index-input").val();
    if (val.length === 0) {
      return null;
    } else {
      return val;
    }
  };

  var maybeTrue = function (bool) {
    if (bool) {
      return true;
    } else {
      return null;
    }
  };

  var clearStore = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchIndex", null);
    localStorage.setItem(ident + "_searchIndexLabel", null);
    localStorage.setItem(ident + "_searchFields", null);
    localStorage.setItem(ident + "_searchExclude", null);
    localStorage.setItem(ident + "_searchInvert", null);
  };

  var clearVals = function () {
    formElems.forEach(function (x) {
      var elem = x();
      switch (elem.attr('type')) {
      case "hidden":
        elem.val('');
        break;
      case "checkbox":
        elem.attr("checked", false);
        break;
      }
    });
  };

  var hideElems = function () {
    formElems.forEach(function (x) {
      var elem = x();
      switch (elem.attr('type')) {
      case "hidden":
        break;
      case "checkbox":
        elem.parent("div").hide();
        break;
      default:
        elem.hide();
      }
    });
  };

  var fieldLabels = function () {
    var ident = getIdentifier();
    var fieldlabels = JSON.parse(sessionStorage.getItem(ident + "_labels"));
    return fieldlabels;
  };

  var searchFieldItem = function (field, fieldLabel) {
    return templates['search-field-item'].render({
      fieldLabel: fieldLabel,
      field: field
    });
  };

  var setFields = function (fields) {
    var fLabels = fieldLabels();
    var jFields = JSON.stringify(fields);
    var sfls = searchFieldsLabel();
    var ident = getIdentifier();

    searchFields().val(jFields);
    localStorage.setItem(ident + "_searchFields", jFields);

    var linkLabels = fields.map(function (x) {
      return searchFieldItem(x, fLabels[x].join(": "));
    });

    sfls.html(linkLabels.join(" "));

    return true;
  };

  mod.allFields = function () {
    clearStore();
    hideElems();
    clearVals();
    return mod;
  };

  mod.singleField = function (fields) {
    mod.multipleFields(fields);
    searchInvert().parent().show();
    return mod;
  };

  mod.singleFieldInverse = function (fields) {
    var ident = getIdentifier();
    mod.singleField(fields);
    searchInvert().attr('checked', true);
    localStorage.setItem(ident + "_searchInvert", true);
    return mod;
  };

  mod.multipleFields = function (fields) {
    mod.allFields();
    setFields(fields);
    [searchAll(), searchFieldsLabel(), searchExclude().parent()].forEach(function (x) {
      x.show();
    });
    return mod;
  };

  mod.excludedFields = function (fields) {
    var ident = getIdentifier();
    if (fields.length > 1) {
      mod.multipleFields(fields);
    } else {
      mod.singleField(fields);
    }
    searchExclude().attr('checked', true);
    localStorage.setItem(ident + "_searchExclude", true);
    return mod;
  };

  mod.indexOnly = function (index, indexLabel) {
    var ident = getIdentifier();
    mod.allFields();
    localStorage.setItem(ident + "_searchIndex", index);
    localStorage.setItem(ident + "_searchIndexLabel", indexLabel);
    searchIndex().val(index);
    searchIndexLabel().html(indexLabel);
    [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parent()].forEach(function (x) {
      x.show();
    });
    return mod;
  };

  mod.indexInverse = function (index, indexLabel) {
    var ident = getIdentifier();
    mod.indexOnly(index, indexLabel);
    searchInvert().attr('checked', true);
    localStorage.setItem(ident + "_searchInvert", true);
    return mod;
  };

  mod.getSearch = function () {
    var query = searchTerm().val();
    var url = "documents/search?q=" + window.encodeURIComponent(query);
    var field = searchFields().val();
    var exclude = searchExclude().is(':checked');
    var invert = searchInvert().is(':checked');
    var index = searchIndex().val();
    var fieldlabels = fieldLabels();

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
    if (invert) {
      url = url + "&invert=true";
    }

    searchListing().hide();

    $.get(url, function (searchResults) {
      searchListing().html(searchResults);
      $('.search-result-field-id').each(function (index, item) {
        var label = fieldlabels[$(item).attr('data-field-field')].join(": ");
        var target = $(item).children('a').first();
        target.html(label);
        target.attr('data-search-label', label);
      });
      if (!invert) {
        $('.search-results th').each(function (index, item) {
          var itemText = $.trim($(item).children('a').html());
          var re = new RegExp("(" + query + ")", "g");
          var newText =
          itemText.replace(re, "<span class='highlight'>$1</span>");
          $(item).children('a').html(newText);
        });
      }
      searchListing().show();
    });

    return mod;
  };

  mod.removeField = function (t) {
    var ident = getIdentifier();
    var searchFields = localStorage.getItem(ident + "_searchFields");
    var newSearchFields;
    var fields = JSON.parse(searchFields);
    var newFields;
    var id = $(t).attr("data-field-field");

    if (fields !== null) {
      newFields = fields.filter(function (x) {
        return x !== id;
      });
      newSearchFields = JSON.stringify(newFields);
      localStorage.setItem(ident + "_searchFields", (newFields.length === 0) ? null : newSearchFields);
      localStorage.setItem(ident + "_searchIndex", null);
      mod.loadSearchVals();
    }

    return mod;
  };

  mod.addField = function (t) {
    var ident = getIdentifier();
    var searchFields = localStorage.getItem(ident + "_searchFields");
    var newSearchFields;
    var fields = JSON.parse(searchFields);
    var newFields;
    var id = $(t).attr("data-field-field");

    if (fields === null) {
      fields = [];
    }

    newFields = fields.concat(id);
    newSearchFields = JSON.stringify(newFields);
    localStorage.setItem(ident + "_searchFields", (newFields.length === 0) ? null : newSearchFields);
    localStorage.setItem(ident + "_searchIndex", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.addIndex = function () {
    var val = indexVal();
    var ident = getIdentifier();

    if (val) {
      localStorage.setItem(ident + "_searchFields", null);
      localStorage.setItem(ident + "_searchIndex", val);
      localStorage.setItem(ident + "_searchIndexLabel", $("option[value=" + val + "]").html());
      mod.loadSearchVals();
    }

    return mod;
  };

  mod.toggleInversion = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchInvert", maybeTrue(searchInvert().is(":checked")));
    localStorage.setItem(ident + "_searchExclude", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.toggleExclusion = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchExclude", maybeTrue(searchExclude().is(":checked")));
    localStorage.getItem(ident + "_searchInvert", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.loadSearchVals = function () {
    var ident = getIdentifier();
    var exclude = localStorage.getItem(ident + "_searchExclude");
    var invert = localStorage.getItem(ident + "_searchInvert");
    var index = localStorage.getItem(ident + "_searchIndex");
    var fieldids = localStorage.getItem(ident + "_searchFields");
    var fields;
    var indexLabel;
    var params = [exclude, invert, index, fieldids].map(function (x) {
      return (x === "null" || x === "false" || x === "true") ? JSON.parse(x) : x;
    });
    var allNull = params.every(function (x) {
      return x === null;
    });

    try {
      if (allNull) {
        mod.allFields();
      } else if (params[0] === true) {
        fields = JSON.parse(fieldids);
        mod.excludedFields(fields);
      } else if (params[1] === null && params[3] !== null) {
        fields = JSON.parse(fieldids);
        if (fields.length > 1) {
          mod.multipleFields(fields);
        } else {
          mod.singleField(fields);
        }
      } else if (params[3] !== null) {
        fields = JSON.parse(fieldids);
        if (fields.length > 1) {
          mod.multipleFields(fields);
        } else {
          mod.singleFieldInverse(fields);
        }
      } else if (params[1] === null) {
        indexLabel = localStorage.getItem(ident + "_searchIndexLabel");
        mod.indexOnly(index, indexLabel);
      } else if (params[1] === true) {
        indexLabel = localStorage.getItem(ident + "_searchIndexLabel");
        mod.indexInverse(index, indexLabel);
      }
    } catch (e) {
      window.console.log(e);
      mod.allFields();
    }

    return mod;
  };

  mod.toggleSelection = function (t) {
    var target = $(t);

    if (target.is(":checked")) {
      target.next("label").next("table").addClass("selected-for-save");
    } else {
      target.next("label").next("table").removeClass("selected-for-save");
    }

    return mod;
  };

  return mod;
})();
shimi.setsui = (function () {
  var mod = {};
  var sets = shimi.sets;
  var utils = shimi.utils();
  var setA = function () {
    return $("#document-set-a-input");
  };
  var setB = function () {
    return $("#document-set-b-input");
  };
  var op = function () {
    return $("#document-set-operation-input");
  };
  var setListing = function () {
    return $("#set-listing");
  };

  var selectedToArray = function (target) {
    var retval = [];

    switch (target) {
    case "search":
      retval = selectedSaveResultsToArray();
      break;
    case "sets":
      retval = selectedElementsToArray();
      break;
    }

    return retval;
  };

  var selectedElementsToArray = function () {
    var retval;
    var selected = $("input.set-element-selection:checked");

    retval = $.map(selected, function (elem) {
      var anchor = $(elem).parent("td").next("td").find("a").first();
      var id = anchor.first().attr("href").replace(/^#/, "");
      var context = anchor.html().trim();
      return [[context, id]];
    });
    return retval;
  };

  var selectedSaveResultsToArray = function () {
    var retval;
    var selected = $("table.selected-for-save tr");

    retval = $.map(selected, function (elem) {
      var id = $(elem).find("th a").first().attr("href").replace(/^#/, "");
      var context = $(elem).find("td.search-result-context a").first().html().trim();
      return [[context, id]];
    });

    return retval;
  };

  var render = function (setElems) {
    var total = setElems.length;
    var elems = setElems.map(function (x) {
      return {
        id: x[1],
        context: x[0]
      };
    });
    var listing = templates['set-listing'].render({
      elements: elems,
      total: total
    });
    setListing().html(listing);
    return mod;
  };

  var view = function (setName) {
    var elems = sets.getSet(setName)[1];
    render(elems);
    return mod;
  };

  var remove = function (setName) {
    sets.removeSet(setName);
    render([]);
    mod.updateSelection();
    return mod;
  };

  var union = function (setNameA, setNameB) {
    var newSet = sets.union(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var intersection = function (setNameA, setNameB) {
    var newSet = sets.intersection(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var symetricDifference = function (setNameA, setNameB) {
    var newSet = sets.symetricDifference(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var relativeComplement = function (setName1, setName2) {
    var newSet = sets.relativeComplement(setName1, setName2);
    render(newSet);
    return mod;
  };

  mod.performOp = function () {
    switch (op().val()) {
    case "view-a":
      view(setA().val());
      break;
    case "view-b":
      view(setB().val());
      break;
    case "remove-a":
      remove(setA().val());
      break;
    case "remove-b":
      remove(setB().val());
      break;
    case "union":
      union(setA().val(), setB().val());
      break;
    case "intersection":
      intersection(setA().val(), setB().val());
      break;
    case "symetric-difference":
      symetricDifference(setA().val(), setB().val());
      break;
    case "relative-complement-b-in-a":
      relativeComplement(setA().val(), setB().val());
      break;
    case "relative-complement-a-in-b":
      relativeComplement(setB().val(), setA().val());
      break;
    default:
      break;
    }
    return mod;
  };

  mod.updateSelection = function () {
    var currNames = sets.getSetNames();
    var newOptions = templates['set-options'].render({
      names: currNames
    });
    setA().html(newOptions);
    setB().html(newOptions);
    return mod;
  };

  mod.saveSelected = function () {
    var dialog = $("#new-set-dialog");
    var name = $("#new-set-input").val();
    var target = $("#new-set-target-input").val();
    var selected;
    var newSet;

    if (!utils.isBlank(name)) {
      dialog.hide();
      selected = selectedToArray(target);
      newSet = [name, selected];
      sets.setSet(newSet);
      $("#new-set-input").val("");
      mod.updateSelection();
      shimi.flash("Success:", "Set '" + name + "' saved.").highlight();
    } else {
      shimi.flash("Input invalid:", "You must supply a valid name.").error();
    }

    return mod;
  };

  mod.toggleSelectAll = function (target) {
    if ($(target).is(":checked")) {
      $("input.set-element-selection").attr("checked", true);
    } else {
      $("input.set-element-selection").attr("checked", false);
    }
    return mod;
  };

  return mod;
})();
// View pane UI elements
shimi.viewui = (function (args) {
  var mod = {};
  var dv = function () {
    return $("#document-view");
  };
  var dvt = function () {
    return $("#document-view-tree");
  };
  var viewInfo = function () {
    return $("#document-view-info");
  };

  mod.formatTimestamps = function () {
    $('.timestamp').each(

    function (i, item) {
      var newDate = (new Date($(item).text())).toLocaleString();
      if (newDate !== "Invalid Date") {
        $(item).text(newDate);
      }
    });

    return mod;
  };

  mod.get = function (id, rev, callback) {
    var url = "documents/" + id;
    var htmlTarget = dv();

    if (rev) {
      url = url + "/" + rev;
      htmlTarget = dvt();
    }

    $.get(url, function (documentHtml) {
      htmlTarget.html(documentHtml);
      window.location.hash = id;
      mod.formatTimestamps();
      dv().fadeTo('slow', 1);
      if (callback) {
        callback();
      }

      if (!rev) {
        var restoreButton = $('#document-restore-button');
        var editButton = $('#document-edit-button');
        var deleteButton = $('#document-delete-button');

        if (shimi.store(restoreButton).d("deleted") === "true") {
          editButton.hide();
          deleteButton.hide();
          restoreButton.show();
        }
      }
    });

    return mod;
  };

  mod.restore = function (id, rev) {
    var url = "./documents/" + id + "?rev=" + rev;
    var restoreButton = $('#document-restore-button');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var body;
    var title;

    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function (req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was restored.";

          mod.get(id, null, function () {
            dv().fadeTo('slow', 1);
            shimi.indexiu.get(skey, sid);
          });
          shimi.flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          shimi.flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document was erased and cannot be restored.";
          title = req.statusText;

          shimi.flash(title, body).error();
        }
      }
    });

    return mod;
  };

  mod.del = function (id, rev) {
    var url = "./documents/" + id + "?rev=" + rev;
    var restoreButton = $('#document-restore-button');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var body;
    var title;

    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function (req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was deleted.";
          var response = JSON.parse(req.responseText);

          shimi.store(restoreButton).put("document-rev", response.rev);

          $('#document-delete-button').hide();
          $('#document-edit-button').hide();
          restoreButton.show();
          dv().fadeTo('slow', 0.5);

          shimi.indexiu.get(skey, sid);
          shimi.flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          shimi.flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document appears to have been deleted already.";
          title = req.statusText;

          shimi.flash(title, body).error();
        }
      }
    });

    return mod;
  };

  mod.confirmIt = function (callback) {
    if (window.confirm("Are you sure?")) {
      var s = shimi.store(viewInfo());
      var id = s.d("document");
      var rev = s.d("rev");

      callback(id, rev);
    }

    return mod;
  };

  mod.edit = function () {
    shimi.editui.resetFields();
    if ($('#document-view-tree').hasClass('oldrev')) {
      $('#save-document-button').addClass('oldrev');
    } else {
      $('#save-document-button').removeClass('oldrev');
    }
    shimi.fieldsets.fillFieldsets();

    return mod;
  };

  mod.confirmDelete = function () {
    var s = shimi.store(viewInfo());
    var id = s.d("document");
    var rev = s.d("rev");
    return mod.confirmIt(function () {
      mod.del(id, rev);
    });
  };

  mod.confirmRestore = function () {
    var s = shimi.store(viewInfo());
    var id = s.d("document");
    var rev = s.d("rev");
    return mod.confirmIt(function () {
      mod.restore(id, rev);
    });
  };

  mod.collapseToggle = function (target) {
    $(target).parent('li').toggleClass('collapsed');

    return mod;
  };

  mod.fetchRevision = function (target) {
    var s = shimi.store($(target));
    var id = s.d("document");
    var oldrev = s.d("oldrev");

    $('.revision-link').removeClass('selected-revision');
    $(target).addClass('selected-revision');

    mod.get(id, oldrev);

    return mod;
  };

  return mod;
})();
shimi.fm = (function () {
  var mod = {};

  var getDirListing = function (path) {
    if (path === undefined) {
      path = "";
    }

    $.get("file_manager/list_dirs/" + path, function (data) {
      $('#file-paths').html(data);
    });
  };

  mod.goDir = function (target) {
    var newpath = $(target).attr('data-path');
    window.sessionStorage.fmPath = newpath;
    mod.refreshListings(newpath);

    return mod;
  };

  mod.rootDir = function () {
    var path = window.sessionStorage.fmPath = "";
    mod.refreshListings();

    return mod;
  };

  mod.upDir = function () {
    var path = window.sessionStorage.fmPath;
    var newpath = path.split("/");
    newpath.pop();
    newpath = newpath.join("/");
    window.sessionStorage.fmPath = newpath;

    mod.refreshListings(newpath);

    return mod;
  };

  var getFileListing = function (path) {
    if (path === undefined) {
      path = "";
    }

    $.get("file_manager/list_files/" + path, function (data) {
      $('#file-listing').html(data);
    });
  };

  mod.editFile = function (target) {
    var path = window.sessionStorage.fmPath;
    var fileId = target.attr('data-file-id');
    var url = "file_manager/" + fileId;

    $.getJSON(url, function (obj) {
      pathEditDialog(obj, path).dialog('open');
    });

    return mod;
  };

  mod.deleteFile = function (target) {
    var path = window.sessionStorage.fmPath;
    var fileId = target.attr('data-file-id');
    var fileRev = target.attr('data-file-rev');
    var url = "file_manager/" + fileId + "?rev=" + fileRev;
    var complete = function () {
      mod.refreshListings(path);
      shimi.flash("Success", "File Deleted").highlight();
    };

    shimi.form.send(url, null, 'DELETE', complete, target);

    return mod;
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
            mod.refreshListings(path);
            shimi.flash("Success", "File Moved").highlight();
          };

          obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split("/");
          shimi.form.send(url, obj, 'PUT', complete, dialog);
          $(this).dialog("close");
        },
        "Cancel": function () {
          $(this).dialog("close");
        }
      }
    });

    return dialog;
  };

  mod.refreshListings = function (path) {
    getDirListing(path);
    getFileListing(path);
  };

  return mod;
})();
shimi.initIndexBuilderDialog = function (indexDoctype) {
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
  var evs = shimi.ihelpers.evs;

  $('.ui-helper-reset div').show();

  var appendCondition = function (builderRow) {
    var tableBody = $('#index-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();

    return false;
  };

  shimi.ihelpers.fOpts(fieldset_url, builderFieldset, function () {
    builderFieldset.inputEnable();
  });

  builderOr.change(function () {
    if (builderOr.is(':checked')) {
      $('#builder-conditions').hide();
      $('#builder-parens').hide();
    } else {
      $('#builder-conditions').show();
      $('#builder-parens').show();
    }
  });

  builderParen.change(function () {
    if (builderParen.val()) {
      $('#builder-or').hide();
      $('#builder-conditions').hide();
    } else {
      $('#builder-or').show();
      $('#builder-conditions').show();
    }
  });

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderField.inputDisable();
      builderArgument.inputDisable();

      return function () {
        builderField.inputEnable();
      };
    });
  };

  var fieldEvents = function () {
    evs.setIndexFieldEvents(indexDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderArgument.inputDisable();

      return function () {
        builderOperator.inputEnable();
      };
    });
  };

  var operatorEvents = function () {
    evs.setIndexOperatorEvents(builderArgument, builderOperator, builderField, function () {
      builderArgument.inputDisable();

      return function () {
        builderArgument.inputEnable();
      };
    });
  };

  var dialog = $("#index-builder-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function () {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!builderOr.is(':checked') && !builderParen.val()) {
          notBlank.forEach(function (item) {
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
            $.get(condition_url, {
              "is_or": true
            }, function (data) {
              appendCondition(data);
            });
          } else if (builderParen.val()) {
            $.get(condition_url, {
              "is_or": false,
              "parens": builderParen.val(),
              "negate": false
            }, function (data) {
              appendCondition(data);
            });
          } else {
            $.get(condition_url, {
              "is_or": false,
              "parens": false,
              "negate": builderNegate.is(':checked'),
              "fieldset": builderFieldset.val(),
              "field": builderField.val(),
              "operator": builderOperator.val(),
              "argument": builderArgument.val()
            }, function (data) {
              appendCondition(data);
            });
          }

          $(this).dialog("close");
        }
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      $('#builder-conditions').show();
      builderFieldset.unbind('change');
      builderField.unbind('change');
      builderOperator.unbind('change');
      shimi.form.clear($('.input')).removeClass('ui-state-error');
    }
  });

  fieldsetEvents();
  fieldEvents();
  operatorEvents();

  return dialog;
};
shimi.ieditui = (function () {
  var mod = {};

  var tableBody = function () {
    return $('#index-conditions-listing tbody');
  };

  var editingData = function () {
    return $('#index-editing-data');
  };

  var fixArgumentType = function (argument, subcategory, operator) {
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

  var getIndexConditions = function (doctypeId, rows) {
    var conditions = rows.map(

    function (index, row) {
      row = $(row);
      var is_or = row.find('td.or-condition').attr('data-value') === "true";
      var paren = row.find('td.paren-condition').attr('data-value');
      var condition;

      if (is_or) {
        condition = {
          "is_or": true,
          "parens": false
        };
      } else if (paren) {
        condition = {
          "is_or": false,
          "parens": paren
        };
      } else {
        var fieldId = row.find('td.field-condition').attr('data-value');
        var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
        var argument = row.find('td.argument-condition').attr('data-value');
        var fieldDoc = shimi.ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
        var negate =
        row.find('td.negate-condition').attr('data-value') === "true";
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

  var saveIndex = function (buttonData, completeFunction) {
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

    shimi.form.send(url, obj, 'PUT', completeFunction, this);

    return false;
  };

  var deleteIndex =

  function (indexId, indexRev, completeMessage, completeFunction) {
    var url = "indexes/" + indexId + "?rev=" + indexRev;
    var title;
    var body;

    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function (req, status) {
        if (req.status === 204) {
          title = "Success";
          body = completeMessage;

          completeFunction();

          shimi.flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          shimi.flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Index appears to have been deleted already.";
          title = req.statusText;

          shimi.flash(title, body).error();
        }
      }
    });

    return false;
  };

  mod.init = function (target) {
    var indexId = $(target).attr('data-index-id');
    var url = "indexes/" + indexId;
    var htmlTarget = $('#index-conditions');

    $.get(url, function (indexData) {
      htmlTarget.html(indexData);
      tableBody().sortable();
      shimi.ipreviewui.get();
    });

    return false;
  };

  mod.save = function () {
    var bData = editingData();

    if (bData.length !== 0) {
      var completeFunction = function () {
        mod.init(bData);
        shimi.flash("Success", "Your index has been saved.").highlight();
      };

      saveIndex(bData, completeFunction);
    } else {
      shimi.flash("Info", "No index has been chosen to save.").highlight();
    }
  };

  mod.replace = function () {
    var bData = editingData();

    if (bData.length !== 0) {
      shimi.initReplaceDialog().dialog("open");
    } else {
      shimi.flash("Info", "You must choose an index first.").highlight();
    }

    return mod;
  };

  mod.addCond = function () {
    var bData = editingData();

    if (bData.length !== 0) {
      shimi.initIndexBuilderDialog(
      bData.attr('data-index-doctype')).dialog("open");
    } else {
      shimi.flash("Info", "You must choose an index first.").highlight();
    }

    return mod;
  };

  mod.remCond = function (target) {
    $(target).closest('tr').remove();
    return mod;
  };

  mod.newCond = function () {
    shimi.initIndexNewDialog().dialog("open");
    return mod;
  };

  mod.del = function () {
    var bData = editingData();

    if (bData.length !== 0) {
      var indexId = bData.attr('data-index-id');
      var indexRev = bData.attr('data-index-rev');
      var completeMessage = "Your index has been deleted.";
      var completeFunction = function () {
        $('#index-conditions').empty();
        shimi.ilistingui.init();
      };

      if (window.confirm("Are you sure?")) {
        deleteIndex(indexId, indexRev, completeMessage, completeFunction);
      }
    } else {
      shimi.flash("Info", "No index has been chosen to delete.").highlight();
    }

    return mod;
  };

  return mod;
})();
shimi.ihelpers = (function () {
  var mod = {};
  var s = shimi.sess();
  mod.evs = {};

  var disableOptions = function (options, disables) {
    options.children().show();

    disables.forEach(function (item) {
      options.children('option:contains(' + item + ')').hide();
    });

    return false;
  };

  var disableOperatorOptions = function (fieldDoc) {
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

  mod.alterArg =

  function (argumentField, operatorField, fieldField, callback) {
    var fieldDoc = function () {
      return s.get(fieldField.val());
    };

    callback();

    argumentField.removeAttr('disabled').datepicker('destroy');
    argumentField.removeAttr('disabled').autocomplete('destroy');

    var dateOrText = function (argumentField, fdoc) {
      if (fdoc.subcategory === 'date') {
        argumentField.removeAttr('disabled');
        argumentField.datepicker({
          dateFormat: "yy-mm-dd"
        });
      } else {
        argumentField.removeAttr('disabled');
        argumentField.autocomplete({
          source: fdoc.allowed
        });
      }

      return mod;
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

    return mod;
  };

  mod.alterOpts = function (fieldDoc, fieldId, callback) {
    disableOperatorOptions(fieldDoc);
    callback();

    return mod;
  };

  mod.fOpts = function (url, selectElement, callback) {
    $.get(url, function (options) {
      selectElement.html(options);
      if (callback) {
        callback();
      }
    });

    return mod;
  };

  mod.getFieldDoc = function (fieldId, fieldsetId, doctypeId, callback) {
    var fieldDoc = s.get(fieldId);
    var url = 'doctypes/' + doctypeId + '/fieldsets/' + fieldsetId + '/fields/' + fieldId + '?format=json';

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
        success: function (data) {
          s.put(data);
          if (callback) {
            callback(s.get(fieldId));
          }
        }
      });

      return s.get(fieldId);
    }
  };

  mod.evs.setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
    indexDoctype.change(function () {
      var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      mod.fOpts(url, indexFieldset, callback2);
    });

    return false;
  };

  mod.evs.setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexFieldset.change(function () {
      var callback2;

      if (typeof indexDoctype !== "string") {
        indexDoctype = indexDoctype.val();
      }

      if (indexFieldset.val()) {
        var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.val() + '/fields?as=options';

        if (callback) {
          callback2 = callback();
        }

        mod.fOpts(url, indexField, callback2);
      }
    });

    return mod;
  };

  mod.evs.setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexField.change(function () {
      var fieldId = indexField.val();
      var fieldsetId = indexFieldset.val();
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      if (!(fieldId.isBlank())) {
        mod.getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
          shimi.ihelpers.alterOpts(data, fieldId, callback2);
        });
      }
    });

    return mod;
  };

  mod.evs.setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
    operatorField.change(function () {
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      mod.alterArg(argumentField, operatorField, fieldField, callback2);
    });

    return mod;
  };

  return mod;
})();
shimi.ilistingui = (function () {
  var mod = {};

  mod.init = function () {
    var url = "indexes";
    var target = $('#index-index-listing');

    $.get(url, function (index) {
      target.html(index);
    });

    return mod;
  };

  return mod;
})();
shimi.ipreviewui = (function () {
  var mod = {};
  var index = shimi.index;

  mod.get = function (startkey, startid, prevkeys, previds) {
    var indexId = $('#index-editing-data').attr('data-index-id');
    var url = 'indexes/' + indexId + "/view";
    var target = $('#index-list-view');
    var filterForm = $('#index-filter-form input');

    if (indexId) {
      index({
        url: url,
        target: target
      }).get(startkey, startid, prevkeys, previds);
    }

    return mod;
  };

  return mod;
})();
shimi.initIndexNewDialog = function () {
  var indexDoctype = $("#index-doctype-input");
  var indexFieldset = $("#index-fieldset-input").inputDisable();
  var indexField = $("#index-field-input").inputDisable();
  var indexName = $("#index-name-input");
  var indexShowDeleted = $("#index-show_deleted-input");
  var evs = shimi.ihelpers.evs;

  var doctypeEvents = function () {
    evs.setIndexDoctypeEvents(indexDoctype, indexFieldset, function () {
      indexFieldset.inputDisable();
      indexField.inputDisable();

      return function () {
        indexFieldset.inputEnable();
      };
    });
  };

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, function () {
      indexField.inputDisable();

      return function () {
        indexField.inputEnable();
      };
    });
  };

  var getLabelForVal = function (val) {
    return $('#index-new-dialog option[value="' + val + '"]').text();
  };

  var getLabel = function () {
    return [getLabelForVal(indexFieldset.val()), getLabelForVal(indexField.val())].join(":");
  };

  var dialog = $("#index-new-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function () {
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
              complete = function (context) {
              shimi.ilistingui.init();
              $(context).dialog("close");
              };
          shimi.form.send("indexes", obj, 'POST', complete, this);
        }
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      indexFieldset.unbind('change');
      indexDoctype.unbind('change');
      shimi.form.clear($('.input')).removeClass('ui-state-error');
    }
  });

  doctypeEvents();
  fieldsetEvents();

  return dialog;
};
shimi.initReplaceDialog = function () {
  var replaceFunction = $('#index-replace_function-input');
  var indexData = $('#index-editing-data');
  var remove = $('#index-remove_function-input');

  if (indexData.attr('data-index-replace_function')) {
    replaceFunction.val(indexData.attr('data-index-replace_function'));
  } else {
    shimi.form.clear(replaceFunction).removeClass('ui-state-error');
  }

  var dialog = $("#index-replace-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
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
            indexData.attr('data-index-replace_function', replaceFunction.val());
            $('#replace-function-message').text("This index has a replacement function.");
          }
        } else {
          indexData.removeAttr('data-index-replace_function');
          $('#replace-function-message').empty();
        }

        $(this).dialog("close");
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      shimi.form.clear(replaceFunction).removeClass('ui-state-error');
    }
  });

  return dialog;
};
shimi.projectui = (function () {
  var mod = {};

  var deleteProject = function (id) {
    if (window.confirm("Are you sure? This is permanent.")) {
      $.ajax({
        type: "DELETE",
        url: "/projects/" + id,
        dataType: "json",
        contentType: "application/json",
        complete: function (req, status) {
          if (req.status === 204) {
            mod.init();
          } else {
            window.alert("An error occurred" + req.status);
          }
        }
      });
    }
  };

  mod.add = function () {
    var projectName = $("#project-name");
    var projectDescription = $("#project-description");
    var tips = $(".validate-tips");
    var allFields = $([]).add(projectName).add(projectDescription);

    var dialog = $("#add-dialog").dialog({
      autoOpen: false,
      modal: true,
      buttons: {
        "Add project": function () {
          allFields.removeClass('ui-state-error');

          var checkResult = shimi.form.checkLength(projectName, "project name", 1, 50, tips);

          if (checkResult) {
            $.ajax({
              type: "POST",
              url: "projects/index",
              dataType: "json",
              contentType: "application/json",
              processData: false,
              data: JSON.stringify({
                name: projectName.val(),
                description: projectDescription.val()
              }),
              complete: function (req, status) {
                if (req.status === 201) {
                  mod.init();
                } else {
                  window.alert("An error occurred" + req.status);
                }
              }
            });
            $(this).dialog("close");
          }
        },
        "Cancel": function () {
          $(this).dialog("close");
        }
      },
      close: function () {
        allFields.val('').removeClass('ui-state-error');
      }
    });

    return dialog;
  };

  mod.del = function (target) {
    var id = $(target).attr("id");
    deleteProject(id);

    return mod;
  };

  mod.init = function () {
    var url = "/projects/index";

    $.get(url, function (projects) {
      $('tbody').empty();
      $('tbody').html(projects);
    });
  };

  return mod;
})();
$(function () {
  $('.notification').hide();

  $('#loading').hide().ajaxStart(function () {
    $(this).show();
  }).ajaxStop(function () {
    $(this).hide();
  });

  shimi.form.initDateFields();

  // Config
  if ($('#configuration').length > 0) {
    shimi.initTabs();
    $('.simple-tabs').tabs();
  }

  // Documents
  if ($('#all-document-container').length > 0) {
    var getIndexTimer;

    shimi.indexiu.iOpts().get();
    shimi.jumpForm();
    shimi.searchForm();
    shimi.setForm();
    shimi.editui.init();

    $('#index-filter-form input').keyup(

    function (e) {
      clearTimeout(getIndexTimer);
      getIndexTimer = setTimeout(function () {
        if (e.which !== 8 && e.which !== 46) {
          shimi.indexiu.get();
        }
      }, 500);
    });

    $('#index-filter-form select').change(

    function () {
      shimi.indexiu.get();
    });

    shimi.loadHash($(location)[0].hash.split("#")[1]);
  }

  // File Manager
  if ($('#file-upload').length > 0) {
    shimi.fm.refreshListings();

    $('#file-upload-target').load(function () {
      var encoded = $('#file-upload-target').contents().find('body pre').html();
      var obj = function () {
        if (encoded && encoded.length > 0) {
          return JSON.parse(encoded);
        } else {
          return {
            message: false
          };
        }
      };

      if (obj() && obj().message && obj().status === "error") {
        shimi.flash("Error", obj().message).error();
        shimi.fm.refreshListings();
      } else if (obj().message) {
        shimi.flash("Success", obj().message).highlight();
        shimi.fm.refreshListings();
      }
    });
  }

  // Index Tool
  if ($('#all-index-container').length > 0) {
    shimi.ilistingui.init();
  }

  // Project
  if ($('#projects-container').length > 0) {
    shimi.projectui.init();
  }
});