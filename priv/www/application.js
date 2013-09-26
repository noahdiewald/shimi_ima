// # Globals object
//
// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
//
// This is not loaded as a module like other code here. It is concatenated
// to the beginning of the target JavaScript file created by the build
// process.

var globals = {};

require=(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
// # The Client Code Entry Point
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// This is the entry point for the client side code. This is where
// basic initializations take place and helper functions are added to
// JavaScript Objects. The 'onload' code is here.

// ## Variable Definitions

var exports = module.exports;

require('./jquery-ui-input-state.js');

var clickDispatch = require('./click-dispatch.js').clickDispatch;
var dblclickDispatch = require('./dblclick-dispatch.js').dblclickDispatch;
var changes = require('./changes.js').changes;
var keystrokes = require('./keystrokes.js').keystrokes;
var form = require('./form.js');

// These are the basic sub-application entry points.
var documents = require('./documents/documents.js');
var fm = require('./file_manager/fm.js');
var ilistingui = require('./index_tool/ilistingui.js');
var projectui = require('./projects/projectui.js');
var config = require('./config/config.js');

// ## Extensions to String and Array Objects

// ### Functions added to String

// This is a poorly implement `isBlank` predicate.
String.prototype.isBlank = function ()
{
  'use strict';

  return ((/^\s*$/).test(this) && !(/\S/).test(this));
};

// Remove white space at the beginning and end of string.
String.prototype.trim = function ()
{
  'use strict';

  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// ### Functions added to Array

// Remove white space on all strings in array.
Array.prototype.trimAll = function ()
{
  'use strict';

  return this.map(function (i)
  {
    return i.trim();
  }).filter(function (i)
  {
    return !i.match(/^$/);
  });
};

// ## On Load

// Using the JQuery function for running code after the page loads.
$(function ()
{
  'use strict';

  // All clicks handled centraly
  $('body').click(function (e)
  {
    clickDispatch(e);
  });

  // All double clicks handled centraly
  $('body').dblclick(function (e)
  {
    dblclickDispatch(e);
  });

  // Other event handling
  keystrokes();
  changes();

  // Hide notification boxes.
  // TODO: move to stylesheets
  $('.notification').hide();

  // Hide ajax loading indicator.
  // TODO: move to stylesheets
  $('#loading').hide();

  // Show and hide the AJAX loading indicator.
  $(document).ajaxStart(function ()
  {
    $('#loading').show();
  }).ajaxStop(function ()
  {
    $('#loading').hide();
  });

  // Initialize any data fields, which use JQueryUI.
  form.initDateFields();

  // ### Determine the sub-application.
  //
  // TODO: With the CommonJS module system there should be a better way
  // of sharing code between these sub-applications.

  // Detect if this is the configuration sub-application
  if ($('#all-config-container').length > 0)
  {
    config.init();
  }

  // Detect if this is the document editing sub-application
  if ($('#all-document-container').length > 0)
  {
    documents.init();
  }

  // Detect if this is the file manager sub-application
  if ($('#file-upload').length > 0)
  {
    fm.init();
  }

  // Detect if this is the index tool sub-application
  if ($('#all-index-container').length > 0)
  {
    ilistingui.init();
  }

  // Detect if this is the project creation sub-application
  if ($('#projects-container').length > 0)
  {
    projectui.init();
  }
});

},{"./changes.js":2,"./click-dispatch.js":3,"./config/config.js":8,"./dblclick-dispatch.js":18,"./documents/documents.js":22,"./file_manager/fm.js":30,"./form.js":32,"./index_tool/ilistingui.js":39,"./jquery-ui-input-state.js":43,"./keystrokes.js":45,"./projects/projectui.js":49}],2:[function(require,module,exports){
// # Change Event Handling
//
// *Implicit depends:* DOM, JQuery
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the 'change' events. This is a start and a bit of
// an experiment. It uses the JQuery `on()` function, which was not
// available when I first began programming this application.

// ## Variable Definitions

var searchui = require('./documents/searchui.js');

// ## Exported Functions

// Run to add event listeners to `document`.
var changes = function ()
{
  'use strict';

  // ### Search UI Change Events

  $(document).on('change', '#document-search-exclude', function (e)
  {
    searchui.toggleExclusion();
    return true;
  });

  $(document).on('change', '#document-search-invert', function (e)
  {
    searchui.toggleInversion();
    return true;
  });
};

exports.changes = changes;

},{"./documents/searchui.js":26}],3:[function(require,module,exports){
// # Dispatching click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all click events that are handled by the system are listed
// here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var S = require('./sender.js');
var dispatcher = require('./dispatcher.js').dispatcher;
var panelToggler = require('./panel-toggle.js').panelToggler;
var editui = require('./documents/editui.js');
var viewui = require('./documents/viewui.js');
var indexui = require('./documents/indexui.js');
var setsui = require('./documents/setsui.js');
var searchui = require('./documents/searchui.js');
var worksheetui = require('./documents/worksheetui.js');
var fieldsets = require('./documents/fieldsets.js');
var ieditui = require('./index_tool/ieditui.js');
var form = require('./form.js');
var projectui = require('./projects/projectui.js');
var fm = require('./file_manager/fm.js');
var maintenanceui = require('./config/maintenanceui.js');
var doctypeTab = require('./config/doctype-tab.js');
var charseqTab = require('./config/charseq-tab').charseqTab;

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var clickDispatch = function (e)
{
  'use strict';

  var action = dispatcher(
  {
    // ### Config

    '.edit-field-button': function (t)
    {
      doctypeTab.editField(t);
    },
    '.delete-field-button': function (t)
    {
      doctypeTab.deleteField(t);
    },
    '.add-field-button': function (t)
    {
      doctypeTab.addField(t);
    },
    '.edit-fieldset-button': function (t)
    {
      doctypeTab.editFieldset(t);
    },
    '.delete-fieldset-button': function (t)
    {
      doctypeTab.deleteFieldset(t);
    },
    '.add-fieldset-button': function (t)
    {
      doctypeTab.addFieldset(t);
    },
    '.delete-doctype-button': function (t)
    {
      doctypeTab.deleteDoctype(t);
    },
    '.edit-doctype-button': function (t)
    {
      doctypeTab.editDoctype(t);
    },
    '.touch-doctype-button': function (t)
    {
      doctypeTab.touchDoctype(t);
    },
    '#doctype-add-button': function (t)
    {
      doctypeTab.addDoctype(t);
    },
    '.delete-charseq-button': function (t)
    {
      charseqTab.del(t);
    },
    '.edit-charseq-button': function (t)
    {
      charseqTab.edit(t);
    },
    '#charseq-add-button': function (t)
    {
      charseqTab.add();
    },
    '#maintenance-upgrade-button': function (t)
    {
      maintenanceui.upgradeButton(t);
    },

    // ### Documents

    '.add-button': function (t)
    {
      fieldsets.initFieldset(t, false, true);
    },
    '.remove-button': function (t)
    {
      fieldsets.removeFieldset(t);
    },
    '#save-document-button': function (t)
    {
      editui.save();
    },
    '#create-document-button': function (t)
    {
      editui.create();
    },
    '#clear-document-button': function (t)
    {
      editui.clear();
    },
    '.expander': function (t)
    {
      editui.toggleTextarea(t);
    },
    'label span.ui-icon-help': function (t)
    {
      editui.showHelpDialog(t);
    },
    '#document-edit-button': function (t)
    {
      viewui.edit(t);
    },
    '#document-delete-button': function (t)
    {
      viewui.confirmDelete();
    },
    '#document-restore-button': function (t)
    {
      viewui.confirmRestore();
    },
    '#document-view-tree > ul > li > b': function (t)
    {
      viewui.collapseToggle(t);
    },
    '.revision-link': function (t)
    {
      viewui.fetchRevision(t);
    },
    '#search-all-fields-switch a': function ()
    {
      searchui.allFields();
    },
    '.search-field-item': function (t)
    {
      searchui.removeField(t);
    },
    '.select-results': function (t)
    {
      searchui.toggleSelection(t);
    },
    '#save-search-results a': function ()
    {
      $('#new-set-target-input').val('search');
      $('#new-set-dialog').show();
    },
    '#save-set-results a': function ()
    {
      $('#new-set-target-input').val('sets');
      $('#new-set-dialog').show();
    },
    '#new-set-save-button': function ()
    {
      S.sender('new-set-form-submit');
    },
    '#select-all-set-elements': function (t)
    {
      setsui.toggleSelectAll(t);
    },
    '.view-document-link span': function (t)
    {
      var parent = t[0].parentNode;
      indexui.load(parent);
    },
    '.view-document-link': function (t)
    {
      indexui.load(t);
    },
    '.select-worksheet-column': function (t)
    {
      var target = $(t);
      var checked = target.is(':checked');
      var field = target.attr('data-field-field');
      worksheetui.columnSelection(field, checked);
    },
    '.select-worksheet-row': function (t)
    {
      var target = $(t);
      var checked = target.is(':checked');
      var row = target.attr('data-row');
      worksheetui.rowSelection(row, checked);
    },
    '#select-all-worksheet-rows': function (t)
    {
      var checked = $(t).is(':checked');
      worksheetui.selectAllRows(checked);
    },
    '#toggle-handles': function (t)
    {
      worksheetui.showHandles();
    },
    '.fieldset-handle': function (t)
    {
      worksheetui.showFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t)
    {
      worksheetui.showField($(t).attr('data-field-field'));
    },
    '.field-header': function (t)
    {
      worksheetui.hideField($(t).attr('data-field-field'));
    },

    // ### Index Tool

    '#new-index-button': function (t)
    {
      ieditui.newCond();
    },
    '.remove-condition-button': function (t)
    {
      ieditui.remCond(t);
    },
    '#delete-index-button': function (t)
    {
      ieditui.del();
    },
    '#save-index-button': function (t)
    {
      ieditui.save();
    },
    '#replace-button': function (t)
    {
      ieditui.replace();
    },
    '#add-index-condition-button': function (t)
    {
      ieditui.addCond();
    },
    '#index-index-listing a': function (t)
    {
      ieditui.init(t);
    },

    // ### Project

    '#create-project': function ()
    {
      projectui.add().dialog('open');
    },
    '.project-delete-button': function (t)
    {
      projectui.del(t);
    },

    // ### File Manager

    '#up-dir': function ()
    {
      fm.upDir();
    },
    '#root-dir': function ()
    {
      fm.rootDir();
    },
    '.dir': function (t)
    {
      fm.goDir(t);
    },
    '.delete-file-button': function (t)
    {
      fm.deleteFile(t);
    },
    '.edit-file-button': function (t)
    {
      fm.editFile(t);
    },

    // ### General

    '.toggler': function (t)
    {
      form.toggle(t);
    },
    '.cancel-dialog': function (t)
    {
      form.cancelDialog(t);
    },
    '#panel-toggle li': function (t)
    {
      panelToggler(t);
    }
  });

  action(e);
};

exports.clickDispatch = clickDispatch;

},{"./config/charseq-tab":6,"./config/doctype-tab.js":11,"./config/maintenanceui.js":17,"./dispatcher.js":19,"./documents/editui.js":23,"./documents/fieldsets.js":24,"./documents/indexui.js":25,"./documents/searchui.js":26,"./documents/setsui.js":27,"./documents/viewui.js":28,"./documents/worksheetui.js":29,"./file_manager/fm.js":30,"./form.js":32,"./index_tool/ieditui.js":36,"./panel-toggle.js":47,"./projects/projectui.js":49,"./sender.js":55}],4:[function(require,module,exports){
// # Charseq manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var charseqElems = require('./charseq-elems.js').charseqElems;
var charseqTab = require('./charseq-tab.js').charseqTab;
var form = require('../form.js');

// Exported functions

// Dialog for manipulating doctypes
var charseqDialog = function (values)
{
  'use strict';
  var f = charseqElems.get(values);

  var dialog = $('#charseq-dialog').dialog(
  {
    width: 650,
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function (context)
        {
          charseqTab.init();
          $(context).dialog('close');
        };

        if (values && values.rev)
        {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }

        form.send(url, obj, method, complete, this);
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      f.clear();
    }
  });

  return dialog;
};

exports.charseqDialog = charseqDialog;

},{"../form.js":32,"./charseq-elems.js":5,"./charseq-tab.js":6}],5:[function(require,module,exports){
// # Working with elements of a charseq manipulation HTML form
//
// *Implicit depends:* DOM, JQuery
//
// A charaseq is a collection of information used in definining properies
// of a script, including some phonological information and information
// used for collation of items written in the script.

// Variable Definitions

var form = require('../form.js');

// Exported functions

// Return object for working with charseq elements
var charseqElems = (function ()
{
  'use strict';

  var mod = {};

  mod.attrs = ['description', 'characters', 'name', 'sort_ignore', 'locale', 'tailoring', 'vowels', 'consonants', 'ietf_tag', 'iso639_tag', 'charseq', 'rev'];

  mod.get = function (values)
  {
    var cObj = {};

    cObj.attrs = mod.attrs;

    cObj.copyValues = function (source)
    {
      Object.keys(source).forEach(function (field)
      {
        cObj[field].val(source[field]);
      });
      return cObj;
    };

    cObj.getCharseqInputVals = function ()
    {
      var valObj = {
        'category': 'charseq',
        'description': cObj.description.val(),
        'characters': cObj.parse(cObj.characters.val()),
        'name': cObj.name.val(),
        'sort_ignore': cObj.parse(cObj.sort_ignore.val()),
        'locale': cObj.locale.val(),
        'tailoring': cObj.tailoring.val(),
        'vowels': cObj.parse(cObj.vowels.val()),
        'consonants': cObj.parse(cObj.consonants.val()),
        'ietf_tag': cObj.ietf_tag.val(),
        'iso639_tag': cObj.iso639_tag.val(),
        '_id': (cObj.charseq.val() || undefined),
        'rev': (cObj.rev.val() || undefined)
      };
      return valObj;
    };

    cObj.parse = function (val)
    {
      if (val && !val.isBlank())
      {
        return JSON.parse(val);
      }
      else
      {
        return [];
      }
    };

    cObj.clear = function ()
    {
      form.clear($('#charseq-dialog .input')).removeClass('ui-state-error');
      return cObj;
    };

    cObj.attrs.forEach(function (item)
    {
      cObj[item] = $('#charseq-' + item + '-input');
    });

    if (values)
    {
      cObj.copyValues(values);
    }

    return cObj;
  };

  return mod;
})();

exports.charseqElems = charseqElems;

},{"../form.js":32}],6:[function(require,module,exports){
// # Charseq tab initialization
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var charseqDialog = require('./charseq-dialog.js').charseqDialog;
var charseqElems = require('./charseq-elems.js').charseqElems;
var store = require('../store.js').store;
var form = require('../form.js');

// Exported functions

// Object containing initialization and other functions.
var charseqTab = (function ()
{
  'use strict';

  var mod = {};

  mod.add = function ()
  {
    charseqDialog().dialog('open');
    return mod;
  };

  mod.edit = function (target)
  {
    var oldobj = {};
    var attrs = charseqElems.attrs;

    attrs.forEach(function (item)
    {
      oldobj[item] = store(target).get64('charseq-' + item);
    });
    charseqDialog(oldobj).dialog('open');

    return mod;
  };

  mod.del = function (target)
  {
    var s = store(target);
    var id = s.get('charseq-charseq');
    var rev = s.get('charseq-rev');
    var url = 'config/charseqs/' + id + '?rev=' + rev;
    var complete = function ()
    {
      mod.init();
    };

    if (window.confirm('Are you sure? This is permanent.'))
    {
      form.send(url,
      {}, 'DELETE', complete, this);
    }

    return mod;
  };

  mod.init = function ()
  {
    var tabs = $('#charseq-tabs');
    var heads = $('#charseq-tabs-headings');
    var url = 'config/charseqs';

    tabs.tabs();

    $.get(url, function (charseqs)
    {
      heads.empty();
      //$('#charseq-tabs-headings + .ui-tabs-panel').remove();
      heads.find('.ui-tabs-panel').remove();
      tabs.tabs('destroy');
      heads.html(charseqs);

      tabs.tabs();
    });

    return mod;
  };

  return mod;
})();

exports.charseqTab = charseqTab;

},{"../form.js":32,"../store.js":57,"./charseq-dialog.js":4,"./charseq-elems.js":5}],7:[function(require,module,exports){
// # Charseq Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of charseqs that can be edited.

var templates = require('templates.js');
var pager = require('../pager.js').pager;

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'charseqs';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var url = 'config/' + prefix();
  var target = document.getElementById(prefix() + '-listing');

  pager(
  {
    prefix: prefix(),
    url: url,
    target: target
  }).get();

  return 'charseq-listing-requested';
};

var init = function ()
{
  'use strict';

  get();

  return 'charsequi-initialized';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;

},{"../pager.js":46,"templates.js":"e8H8MT"}],8:[function(require,module,exports){
// # Config Sub-App Init
//
// *Implicit depends:* DOM
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeui = require('./doctypeui.js');
var maintenanceui = require('./maintenanceui.js');
var charsequi = require('./charsequi.js');

// ## Exported Functions

// Run initialization code for the configuration sub-application.
var init = function ()
{
  'use strict';

  doctypeui.init();
  charsequi.init();
  maintenanceui.init();

  return 'config-initialized';
};

exports.init = init;

},{"./charsequi.js":7,"./doctypeui.js":12,"./maintenanceui.js":17}],9:[function(require,module,exports){
// # Doctype manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var doctypeElems = require('./doctype-elems.js').doctypeElems;
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating doctypes
var doctypeDialog = function (url, values)
{
  'use strict';

  var f = doctypeElems.get(values);

  if (values.rev && !values.rev.isBlank())
  {
    f.doctype.attr('disabled', 'disabled');
  }
  else
  {
    f.doctype.removeAttr('disabled');
  }

  var dialog = $('#doctype-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.getDoctypeInputVals();
        var complete = function (context)
        {
          doctypeTab.init();
          $(context).dialog('close');
        };

        if (!values.rev || values.rev.isBlank())
        {
          url.post(obj, complete, this);
        }
        else
        {
          obj._id = url.doctype;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      f.clear();
    }
  });

  return dialog;
};

exports.doctypeDialog = doctypeDialog;

},{"./doctype-elems.js":10,"./doctype-tab.js":11}],10:[function(require,module,exports){
// # Working with elements of a doctype manipulation HTML form
//
// *Implicit depends:* DOM, JQuery

// Variable Definitions

var form = require('../form.js');

// Exported functions

// Returns an object with references to add/edit doctype dialog
// field elements with helper functions.
var doctypeElems = (function ()
{
  'use strict';

  var mod = {};

  mod.attrs = ['description', 'doctype', 'rev'];

  mod.get = function (values)
  {
    var fObj = {};

    fObj.copyValues = function (source)
    {
      Object.keys(source).forEach(function (field)
      {
        fObj[field].val(source[field]);
      });
      return fObj;
    };

    fObj.getDoctypeInputVals = function ()
    {
      var valObj = {
        'category': 'doctype',
        'description': fObj.description.val(),
        '_id': fObj.doctype.val()
      };
      return valObj;
    };

    fObj.clear = function ()
    {
      form.clear($('#doctype-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    mod.attrs.forEach(function (item)
    {
      fObj[item] = $('#doctype-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();

exports.doctypeElems = doctypeElems;

},{"../form.js":32}],11:[function(require,module,exports){
// # Doctype tab initialization
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var doctypeDialog = require('./doctype-dialog.js').doctypeDialog;
var doctypeElems = require('./doctype-elems.js').doctypeElems;
var fieldDialog = require('./field-dialog.js').fieldDialog;
var fieldElems = require('./field-elems.js').fieldElems;
var fieldsetDialog = require('./fieldset-dialog.js').fieldsetDialog;
var fieldsetElems = require('./fieldset-elems.js').fieldsetElems;
var store = require('../store.js').store;
var path = require('../path.js').path;

// Internal functions

var cpath = function (source, category)
{
  'use strict';

  return path(source, category, 'config');
};

// Exported functions

// Populate the listing of fields
var initFields = function (path)
{
  'use strict';

  path.field = false;

  $.get(path.toString(), function (fields)
  {
    var fieldContainer = $('#fields-' + path.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
  });

  return true;
};

// Populate the listing of fieldsets
var initFieldsets = function (url)
{
  'use strict';

  $.get(url.toString(), function (fieldsets)
  {
    var fieldsetContainer = $('#fieldsets-' + url.doctype);

    fieldsetContainer.empty();
    fieldsetContainer.accordion();
    fieldsetContainer.accordion('destroy');
    fieldsetContainer.html(fieldsets);

    fieldsetContainer.accordion(
    {
      autoHeight: false,
      collapsible: true,
      active: false
    });
  });
};

// populate the tabs listing the doctypes
var init = function ()
{
  'use strict';

  var url = 'config/doctypes';

  $('#doctype-tabs').tabs();

  $.get(url, function (doctypes)
  {
    var fieldsetDoctype = $('#fieldset-doctype-input');

    $('#doctype-tabs-headings').empty();
    $('#doctype-tabs-headings + .ui-tabs-panel').remove();
    $('#doctype-tabs').tabs('destroy');
    $('#doctype-tabs-headings').html(doctypes);

    var loadFun = function (event, ui)
    {
      var source = $(ui.panel).children('div[data-fieldset-doctype]');
      var fieldsetsPath = path(source, 'fieldset', 'config');
      initFieldsets(fieldsetsPath);
    };

    $('#doctype-tabs').tabs(
    {
      load: function (e, ui)
      {
        loadFun(e, ui);
      }
    });
  });
};

// Button that opens a dialog for editing a field
var editField = function (target)
{
  'use strict';

  var url = cpath(target, 'field');
  var oldobj = {};
  var attrs = fieldElems.attrs;
  var charseqUrl = 'config/charseqs?as=options';

  $.get(charseqUrl, function (charseqs)
  {
    $('#field-charseq-input').html(charseqs);
    attrs.forEach(function (item)
    {
      oldobj[item] = store(target).get('field-' + item);
    });
    fieldDialog(url, oldobj).dialog('open');
  });
};

// Button that opens a dialog for deleting a field
var deleteField = function (target)
{
  'use strict';

  var answer = window.confirm('Are you sure? This is permanent.');

  if (answer)
  {
    var url = cpath(target, 'field');
    var complete = function ()
    {
      url.field = false;
      url.rev = false;

      initFields(url);
    };
    url.del(complete, this);
  }
};

// Button that opens a dialog for adding a field
var addField = function (target)
{
  'use strict';

  var url = cpath(target, 'field');
  var charseqUrl = 'config/charseqs?as=options';

  $.get(charseqUrl, function (charseqs)
  {
    $('#field-charseq-input').html(charseqs);
    fieldDialog(url,
    {
      fieldset: url.fieldset,
      doctype: url.doctype
    }).dialog('open');
  });
};

// Button that opens a dialog for editing a fieldset
var editFieldset = function (target)
{
  'use strict';

  var url = cpath(target, 'fieldset');
  var oldobj = {};
  var attrs = fieldsetElems.attrs;

  attrs.forEach(function (item)
  {
    oldobj[item] = store(target).get('fieldset-' + item);
  });

  fieldsetDialog(url, oldobj).dialog('open');
};

// Button that opens a dialog for deleting a fieldset
var deleteFieldset = function (target)
{
  'use strict';

  var url = cpath(target, 'fieldset');

  var complete = function ()
  {
    url.fieldset = false;
    url.rev = false;
    initFieldsets(url);
  };

  if (window.confirm('Are you sure? This is permanent.'))
  {
    url.del(complete, this);
  }
};

// Button that opens a dialog for adding a fieldset.
var addFieldset = function (target)
{
  'use strict';

  var url = cpath(target, 'fieldset');
  fieldsetDialog(url,
  {
    doctype: url.doctype
  }).dialog('open');
};

// Button that opens a dialog for editing a doctype.
var editDoctype = function (target)
{
  'use strict';

  var url = cpath(target, 'doctype');
  var oldobj = {};
  var attrs = doctypeElems.attrs;

  attrs.forEach(function (item)
  {
    oldobj[item] = store(target).get('doctype-' + item);
  });
  doctypeDialog(url, oldobj).dialog('open');
};

// Button for initiating the touch operation.
var touchDoctype = function (target)
{
  'use strict';

  var docid = store(target).get('doctype-doctype');
  $.post('config/doctypes/' + docid + '/touch');
  window.alert('Touch In Progress');
};

// Button for deleting a doctype.
var deleteDoctype = function (target)
{
  'use strict';

  var url = cpath(target, 'doctype');
  var complete = function ()
  {
    url.doctype = false;
    url.rev = false;
    init();
  };

  if (window.confirm('Are you sure? This is permanent.'))
  {
    url.del(complete, this);
  }
};

// Button for adding a doctype.
var addDoctype = function (target)
{
  'use strict';

  var url = cpath(target, 'doctype');
  doctypeDialog(url, {}).dialog('open');
};

exports.initFields = initFields;
exports.initFieldsets = initFieldsets;
exports.init = init;
exports.editField = editField;
exports.deleteField = deleteField;
exports.addField = addField;
exports.editFieldset = editFieldset;
exports.deleteFieldset = deleteFieldset;
exports.addFieldset = addFieldset;
exports.editDoctype = editDoctype;
exports.touchDoctype = touchDoctype;
exports.deleteDoctype = deleteDoctype;
exports.addDoctype = addDoctype;

},{"../path.js":48,"../store.js":57,"./doctype-dialog.js":9,"./doctype-elems.js":10,"./field-dialog.js":13,"./field-elems.js":14,"./fieldset-dialog.js":15,"./fieldset-elems.js":16}],12:[function(require,module,exports){
// # Doctype Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of doctypes that can be edited.

var templates = require('templates.js');
var pager = require('../pager.js').pager;

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'doctypes';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var url = 'config/' + prefix();
  var target = document.getElementById(prefix() + '-listing');

  pager(
  {
    prefix: prefix(),
    url: url,
    target: target
  }).get();

  return 'doctype-listing-requested';
};

var init = function ()
{
  'use strict';

  get();

  return 'doctypeui-initialized';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;

},{"../pager.js":46,"templates.js":"e8H8MT"}],13:[function(require,module,exports){
// # Field manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var fieldElems = require('./field-elems.js').fieldElems;
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating fields
var fieldDialog = function (url, values)
{
  'use strict';

  var f = fieldElems.get(values);

  var dialog = $('#field-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.clearDisabled().getFieldInputVals();
        var complete = function (context)
        {
          doctypeTab.initFields(url);
          $(context).dialog('close');
        };
        if (!values.rev || values.rev.isBlank())
        {
          url.post(obj, complete, this);
        }
        else
        {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      f.clear();
    }
  });

  return dialog;
};

exports.fieldDialog = fieldDialog;

},{"./doctype-tab.js":11,"./field-elems.js":14}],14:[function(require,module,exports){
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

},{"../form.js":32,"../utils.js":58}],15:[function(require,module,exports){
// # Fieldset manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var fieldsetElems = require('./fieldset-elems.js').fieldsetElems;
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating fieldsets
var fieldsetDialog = function (url, values)
{
  'use strict';

  var f = fieldsetElems.get(values);

  var dialog = $('#fieldset-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.getFieldsetInputVals();
        var complete = function (context)
        {
          url.fieldset = false;
          url.rev = false;

          doctypeTab.initFieldsets(url);
          $(context).dialog('close');
        };
        if (!values.rev || values.rev.isBlank())
        {
          url.post(obj, complete, this);
        }
        else
        {
          obj._id = url.fieldset;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      f.clear();
    }
  });

  return dialog;
};

exports.fieldsetDialog = fieldsetDialog;

},{"./doctype-tab.js":11,"./fieldset-elems.js":16}],16:[function(require,module,exports){
// # Working with elements of a fieldset manipulation HTML form
//
// *Implicit depends:* DOM, JQuery

// Variable Definitions

var form = require('../form.js');

// Exported functions

// Returns an object with references to add/edit fieldset dialog
// field elements with helper functions.
var fieldsetElems = (function ()
{
  'use strict';

  var mod = {};

  mod.attrs = ['name', 'label', 'order', 'description', 'doctype', 'rev', 'multiple', 'collapse', 'fieldset'];

  mod.get = function (values)
  {
    var fObj = {};

    fObj.attrs = mod.attrs;

    fObj.copyValues = function (source)
    {
      Object.keys(source).forEach(function (field)
      {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]'))
        {
          if (source[field] === 'true')
          {
            fObj[field].attr('checked', true);
          }
        }
      });
      return fObj;
    };

    fObj.getFieldsetInputVals = function ()
    {
      var valObj = {
        'category': 'fieldset',
        'name': fObj.name.val(),
        'label': fObj.label.val(),
        'order': fObj.order.val() * 1,
        'description': fObj.description.val(),
        'doctype': fObj.doctype.val(),
        'multiple': fObj.multiple.is(':checked'),
        'collapse': fObj.collapse.is(':checked')
      };
      return valObj;
    };

    fObj.clear = function ()
    {
      form.clear($('#fieldset-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    fObj.attrs.forEach(function (item)
    {
      fObj[item] = $('#fieldset-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();

exports.fieldsetElems = fieldsetElems;

},{"../form.js":32}],17:[function(require,module,exports){
// # Maintenance User Interface
// 
// *Implicit depends:* DOM
//
// This handles UI elements that are used for maintaining a project.

// ## Variable Definitions

var templates = require('templates.js');
var form = require('../form.js');
var flash = require('../flash.js');

// ## Exported Functions

// When the upgrade button is pressed in the configuration UI, this
// will carry out the necessary action. It will make an empty `POST`
// to the upgrade path and alert the user that this was done.
var upgradeButton = function ()
{
  'use strict';

  form.send('config/upgrade', 'null', 'POST', function ()
  {
    flash.highlight('Task Started', 'Upgrade Project');
  });

  return 'upgrade-initiated';
};

// Initialize and display the interface.
var init = function ()
{
  'use strict';

  var renderedHTML = templates['config-maintenance']();
  document.getElementById('config-maintenance').insertAdjacentHTML('beforeend', renderedHTML);

  return 'maintenanceui-initialized';
};

exports.init = init;
exports.upgradeButton = upgradeButton;

},{"../flash.js":31,"../form.js":32,"templates.js":"e8H8MT"}],18:[function(require,module,exports){
// # Dispatching double click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all double click events that are handled by the system are
// listed here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var dispatcher = require('./dispatcher.js').dispatcher;
var panelToggler = require('./panel-toggle.js').panelToggler;
var searchui = require('./documents/searchui.js');
var worksheetui = require('./documents/worksheetui.js');

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var dblclickDispatch = function (e)
{
  'use strict';

  var action = dispatcher(
  {
    '.search-result-field-id a': function (t)
    {
      searchui.addField($(t).parent('h5'));
    },
    '.field-view b': function (t)
    {
      searchui.addField($(t).parent('li'));
    },
    '.field-container label span': function (t)
    {
      searchui.addField($(t).parent('label').parent('div'));
    },
    '#index-index-input-label': function ()
    {
      searchui.addIndex();
    },
    '.panel > h2': function (t)
    {
      panelToggler(t);
    },
    '#toggle-handles': function (t)
    {
      worksheetui.hideHandles();
    },
    '.fieldset-handle': function (t)
    {
      worksheetui.hideFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t)
    {
      worksheetui.hideField($(t).attr('data-field-field'));
    }
  });

  action(e);
};

exports.dblclickDispatch = dblclickDispatch;

},{"./dispatcher.js":19,"./documents/searchui.js":26,"./documents/worksheetui.js":29,"./panel-toggle.js":47}],19:[function(require,module,exports){
// # Dispatcher for clicks and double clicks
//
// *Implicit depends:* DOM, JQuery
//
// See [`click-dispatch.js`](./click-dispatch.html) and
// [`dblclick-dispatch.js`](./dblclick-dispatch.html).

// # Exported Functions

// Match the target to a pattern and run its action.
var dispatcher = function (patterns)
{
  'use strict';

  var d = function (e)
  {
    var target = $(e.target);

    Object.keys(patterns).forEach(function (pattern)
    {
      if (target.is(pattern))
      {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

exports.dispatcher = dispatcher;
},{}],20:[function(require,module,exports){
// # Paging For Changes Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads changes based on user suplied values.

// Variable Definitions

var pager = require('../pager.js').pager;

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'changelog';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var url = prefix();
  var target = document.getElementById(prefix() + '-listing');

  var format = function (text)
  {
    var resp = JSON.parse(text);

    resp.rows.map(function (item)
    {
      if (item.doc.changes)
      {
        item.doc.changes = Object.keys(item.doc.changes).map(function (key)
        {
          return item.doc.changes[key];
        });
      }
    });

    return resp;
  };

  pager(
  {
    prefix: prefix(),
    url: url,
    format: format,
    target: target
  }).get();

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"../pager.js":46}],21:[function(require,module,exports){
// # Keyboard shortcuts
//
// *Implicit depends:* DOM, JQuery
//
// Handles the input area and command execution. Keyboard events are
// handled in [keystrokes.js](./keystrokes.html).

// Variable Definitions

var editui = require('./editui.js');
var S = require('../sender.js');

// Internal functions

var commandInput = function ()
{
  'use strict';

  return document.getElementById('edit-command-input');
};

var commandDialog = function ()
{
  'use strict';

  return $('#command-dialog');
};

var setContext = function (elem, context)
{
  'use strict';

  return elem.attr('data-last-active', context);
};

var getContext = function (elem)
{
  'use strict';

  return elem.attr('data-last-active');
};

// Exported functions

// Lookup the command and perform an action.
var execute = function (command)
{
  'use strict';

  var restoreFocus = true;

  switch (command)
  {
  case 'w':
  case 'clear':
    editui.clear();
    break;
  case 'c':
  case 'create':
    editui.create();
    restoreFocus = false;
    break;
  case 's':
  case 'save':
    editui.save();
    break;
  case 'd':
  case 'delete':
    $('#document-view').show();
    if ($('#document-delete-button').css('display') !== 'none')
    {
      $('#document-delete-button').click();
    }
    break;
  case 'e':
  case 'edit':
    $('#document-view').show();
    if ($('#document-edit-button').css('display') !== 'none')
    {
      $('#document-edit-button').click();
      restoreFocus = false;
    }
    break;
  case 'r':
  case 'restore':
    $('#document-view').show();
    if ($('#document-restore-button').css('display') !== 'none')
    {
      $('#document-restore-button').click();
    }
    break;
  }

  if (restoreFocus)
  {
    var cdialog = commandDialog();
    var context = getContext(cdialog);
    $('#' + context).focus();
  }
  else
  {
    S.sender('lost-focus');
  }

  S.sender('executed-command');
  return true;
};

// Open the command dialog
var dialogOpen = function (context)
{
  'use strict';

  var cinput = commandInput();
  var cdialog = commandDialog();
  cinput.value = '';
  setContext(cdialog, context).show();
  cinput.focus();
  return true;
};

// Close the command dialog
var dialogClose = function ()
{
  'use strict';

  var cinput = commandInput();
  var cdialog = commandDialog();
  setContext(cdialog, '').hide();
  cinput.value = '';
  return true;
};

exports.execute = execute;
exports.dialogOpen = dialogOpen;
exports.dialogClose = dialogClose;

},{"../sender.js":55,"./editui.js":23}],22:[function(require,module,exports){
// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// ## Variable Definitions

var setsui = require('./setsui.js');
var editui = require('./editui.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var changeui = require('./changeui.js');
var S = require('../sender.js');
var store = require('../store.js').store;
var identifier;

// ## Internal functions

// In practice this is the select listing of the user created indexes
// which is triggering the change event.
//
// *TODO* put this with other change handlers.
var indexForm = function ()
{
  'use strict';

  $('#index-filter-form select').change(function ()
  {
    indexui.get();
  });

  return true;
};

// If there is a hash at the end of the URL with a document ID specified,
// this will pass the information on the correct funciont in `viewui`.
var loadHash = function (urlHash)
{
  'use strict';

  if (urlHash)
  {
    viewui.get(urlHash);
  }

  return true;
};

// A user interface element.
var allDocContainer = function ()
{
  'use strict';

  return document.getElementById('all-document-container');
};

// Key used in retrieving cached information from session storage.
var versionKey = function ()
{
  'use strict';

  return identifier() + '_version';
};

// Key used in retrieving cached information from session storage.
var infoKey = function ()
{
  'use strict';

  return identifier() + '_info';
};

// Key used in retrieving cached information from session storage.
var labelsKey = function ()
{
  'use strict';

  return identifier() + '_labels';
};

// Store the doctype info in the session store.
var storeDoctype = function (doctype)
{
  'use strict';

  sessionStorage.setItem(infoKey(), doctype);

  return S.sender('doctype-info-ready');
};

// Get the stored doctype version.
var getVersion = function ()
{
  'use strict';

  return sessionStorage.getItem(versionKey());
};

// Get the most recent doctype version, which is placed in a `data`
// attribute that is updated on page reloads.
var getCurrentVersion = function ()
{
  'use strict';

  return store(allDocContainer()).d('version');
};

// Check if the stored doctype version matches the version found in the
// `data` attribute.
var isCurrentVersionStored = function ()
{
  'use strict';

  return (getVersion() && getVersion() === getCurrentVersion());
};

var isInfoStored = function ()
{
  'use strict';

  return sessionStorage.getItem(infoKey()) !== null;
};

var isLabelsStored = function ()
{
  'use strict';

  return sessionStorage.getItem(labelsKey()) !== null;
};

// Reset the doctype version
var setVersion = function ()
{
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  S.sender('version-set');

  return true;
};

// Check the session state to ensure it is up to date and fully
// loaded.
var checkState = function ()
{
  'use strict';

  var retval;

  if (isCurrentVersionStored() && isInfoStored() && isLabelsStored())
  {
    retval = S.sender('labels-ready');
  }
  else
  {
    retval = S.sender('bad-session-state');
  }

  return retval;
};

// Get the doctype name
var dname = function ()
{
  'use strict';

  return store(allDocContainer()).d('doctype');
};

// Get the project id
var project = function ()
{
  'use strict';

  var container = document.getElementById('container');
  return store(container).get('project-id');
};

// ## Exported functions

// Clear the session storage
var clearSession = function ()
{
  'use strict';

  sessionStorage.clear();
  S.sender('session-cleared');

  return true;
};

// Identifier is a combination of the project and doctype name.
identifier = function ()
{
  'use strict';

  return project() + '_' + dname();
};

// Get information about doctype.
var info = function ()
{
  'use strict';

  return JSON.parse(sessionStorage.getItem(infoKey()));
};

// Load the doctype document stored on the server.
var loadDoctype = function ()
{
  'use strict';

  $.getJSON('./', function (data)
  {
    storeDoctype(JSON.stringify(data));
  });

  return true;
};

// Process the field and fieldset info to create a field label to field
// id index.
var makeLabels = function ()
{
  'use strict';

  var info1 = info();
  var labels = {};

  info1.fieldsets.forEach(function (fieldset)
  {
    fieldset.fields.forEach(function (field)
    {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));

  return S.sender('labels-ready');
};

// Initialize the documents sub-application.
var init = function ()
{
  'use strict';

  $('form').on('submit', function ()
  {
    return false;
  });
  checkState();
  setsui.updateSelection();
  indexui.iOpts();
  indexui.get();
  indexForm();
  editui.init();
  loadHash($(location)[0].hash.split('#')[1]);
  changeui.get();
};

exports.setVersion = setVersion;
exports.clearSession = clearSession;
exports.identifier = identifier;
exports.info = info;
exports.loadDoctype = loadDoctype;
exports.makeLabels = makeLabels;
exports.init = init;

},{"../sender.js":55,"../store.js":57,"./changeui.js":20,"./editui.js":23,"./indexui.js":25,"./setsui.js":27,"./viewui.js":28}],23:[function(require,module,exports){
// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Edit pane UI elements

// Variable Definitions

var store = require('../store.js').store;
var form = require('../form.js');
var flash = require('../flash.js');
var fieldsets = require('./fieldsets.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var afterRefresh;

// Internal functions

// UI Element
var saveButton = function ()
{
  'use strict';

  return $('#save-document-button');
};

// UI Element
var createButton = function ()
{
  'use strict';

  return $('#create-document-button');
};

// UI Element
var editButton = function ()
{
  'use strict';

  return $('#document-edit-button');
};


// Display validation error properly.
var validationError = function (req)
{
  'use strict';

  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = $('[data-field-instance=' + body.instance + ']');
  var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');

  invalidTab.addClass('ui-state-error');
  invalid.addClass('ui-state-error');

  flash.error(title, body.fieldname + ' ' + body.message);

  return true;
};

// Fields need to have instances. This should ensure they have them.
var instances = function (addInstances)
{
  'use strict';

  var text = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
  var makeInstance = function ()
  {
    return text.map(function ()
    {
      return text[Math.floor(Math.random() * text.length)];
    }).join('');
  };

  $('#last-added [data-field-instance]').each(function (index, item)
  {
    var itemElem = $(item).first();
    var oldInstance = itemElem.attr('data-field-instance');
    var newInstance = oldInstance;

    if (addInstances)
    {
      newInstance = makeInstance();
    }

    itemElem.attr('data-group-id', newInstance);
    // TODO: This is a little redundant
    itemElem.attr('id', newInstance);
    itemElem.attr('data-field-instance', newInstance);
    // Differences in Firefox and Chrome
    itemElem.next('.expander').attr('data-group-id', newInstance);
    itemElem.next().next('.expander').attr('data-group-id', newInstance);
  });

  if (addInstances)
  {
    $('#last-added').removeAttr('id');
  }

  return true;
};

// Exported functions

// Initialize the editing pane.
var init = function ()
{
  'use strict';

  var url = 'documents/edit';

  $.get(url, function (documentEditHtml)
  {

    $('#document-edit').html(documentEditHtml);
    $('#edit-tabs').tabs();
    fieldsets.initFieldsets();
  });

  return true;
};

// Focus on the first focusable input element in an active tab.
var selectInput = function ()
{
  'use strict';

  var inputable = 'input, select, textarea';
  var t = function ()
  {
    return $('#edit-tabs');
  };

  var cur = t().find('.ui-tabs-active a').attr('href');
  $(cur).find(inputable).first().focus();

  return true;
};

// Used as a variation of `afterRefresh` where a boolean is provided
// to specify if new instances identifiers should be created and
// set. Basically this is for a completely fresh refresh, when the form
// is in the state such that a document can be created but no information
// is available to do an update.
var afterFreshRefresh = function (addInstances)
{
  'use strict';

  afterRefresh(addInstances);

  return true;
};

// Run after the edit button in the view UI is clicked.
var afterEditRefresh = function ()
{
  'use strict';

  var sharedAttrs = ['data-document-id', 'data-document-rev'];

  sharedAttrs.forEach(function (elem)
  {
    saveButton().attr(elem, editButton().attr(elem));
  });

  saveButton().show();
  afterRefresh();

  return true;
};

// Essentially initialization of the form. If `addInstances` is true,
// new instance identifiers will be created for a blank form.
afterRefresh = function (addInstances)
{
  'use strict';

  form.initDateFields();
  instances(addInstances);

  return true;
};

// Reset field values to defaults.
var resetFields = function ()
{
  'use strict';

  $('.field').each(function (index)
  {
    var field = $(this);
    var thedefault = field.attr('data-field-default');

    if (thedefault && thedefault !== '')
    {
      if (field.is('select.multiselect'))
      {
        field.val(thedefault.split(','));
      }
      else if (field.is('input.boolean'))
      {
        field.attr('checked', thedefault === true);
      }
      else
      {
        field.val(thedefault);
      }
    }
    else
    {
      field.val('');
      field.removeAttr('checked');
    }
  });

  return true;
};

// To be run if the user chooses to save the form contents. This is an
// update, not creation.
var save = function ()
{
  'use strict';

  if (saveButton().hasClass('oldrev'))
  {
    if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?'))
    {
      return false;
    }
  }

  var body;
  var title;
  var s = store(saveButton());
  var root = $('#edit-document-form');
  var document = s.d('document');
  var rev = s.d('rev');
  var url = './documents/' + document + '?rev=' + rev;
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton().hide();
  $.extend(obj, fieldsets.fieldsetsToObject(root));

  $.ajax(
  {
    type: 'PUT',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    processData: false,
    data: JSON.stringify(obj),
    complete: function (req, status)
    {
      if (req.status === 204 || req.status === 200)
      {
        title = 'Success';
        body = 'Your document was saved.';
        viewui.get(document);
        indexui.get(skey, sid);
        flash.highlight(title, body);
        saveButton().removeClass('oldrev').show();
      }
      else if (req.status === 403)
      {
        validationError(req);
        saveButton().show();
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
        saveButton().hide();
      }
    }
  });
};

// To be run if creating a new document.
var create = function ()
{
  'use strict';

  var s = store(createButton());
  var root = $('#edit-document-form');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  createButton().hide();
  $.extend(obj, fieldsets.fieldsetsToObject(root));

  var postUrl = $.ajax(
  {
    type: 'POST',
    dataType: 'json',
    contentType: 'application/json',
    processData: false,
    data: JSON.stringify(obj),
    complete: function (req, status)
    {
      if (req.status === 201)
      {
        var title = 'Success';
        var body = 'Your document was created.';
        var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);

        saveButton().hide().attr('disabled', 'true');
        $('.fields').remove();
        fieldsets.initFieldsets();
        viewui.get(documentId);
        indexui.get(skey, sid);
        flash.highlight(title, body);
        createButton().show();
      }
      else if (req.status === 403)
      {
        validationError(req);
        createButton().show();
      }
    }
  });
};

// Clear the form.
var clear = function ()
{
  'use strict';

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton().hide().attr('disabled', 'disabled');
  $('.fields').remove();
  fieldsets.initFieldsets();
};

// Display a help dialog for a form field.
var showHelpDialog = function (target)
{
  'use strict';

  if (target.is('.label-text'))
  {
    target = target.parent('label').find('.ui-icon-help');
  }

  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));

  return true;
};

// Contract and expand textarea elements.
var toggleTextarea = function (target)
{
  'use strict';

  var textarea = $('#' + target.attr('data-group-id'));

  if (target.attr('id') === textarea.attr('data-group-id'))
  {
    textarea.toggleClass('expanded');
    textarea.next().next('span').toggleClass('expanded');
  }
  else
  {
    textarea.toggleClass('expanded');
    target.toggleClass('expanded');
  }

  return true;
};

exports.init = init;
exports.selectInput = selectInput;
exports.afterFreshRefresh = afterFreshRefresh;
exports.afterEditRefresh = afterEditRefresh;
exports.afterRefresh = afterRefresh;
exports.resetFields = resetFields;
exports.save = save;
exports.create = create;
exports.clear = clear;
exports.toggleTextarea = toggleTextarea;

},{"../flash.js":31,"../form.js":32,"../store.js":57,"./fieldsets.js":24,"./indexui.js":25,"./viewui.js":28}],24:[function(require,module,exports){
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
var dateOrNumber;
var getEncoded;
var getFieldValue;
var fillFields;
var setFieldValue;
var initFieldset;

// Internal functions

// Get the container for a fieldset with `id`.
var fsContainer = function (id)
{
  'use strict';

  return $('#container-' + id);
};

// Get the doctype path.
var dpath = function (source, category)
{
  'use strict';

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
  'use strict';

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
  'use strict';

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
dateOrNumber = function (subcategory, fieldvalue)
{
  'use strict';

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
  'use strict';

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
  'use strict';

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
  'use strict';

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
getEncoded = function (value)
{
  'use strict';

  return window.decodeURIComponent(value.replace(/\+/g, ' '));
};

// Get the value from a field using the subcategory to ensure
// that the value has the correct type and is properly formatted.
getFieldValue = function (field)
{
  'use strict';

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
  'use strict';

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
  'use strict';

  vfieldset = $(vfieldset);
  var id = store(vfieldset).fs('fieldset');
  var container = $('#container-' + id);
  var url = dpath(vfieldset, 'fieldset');

  container.html('');

  vfieldset.find('.multifield').each(function (i, multifield)
  {
    initFieldset(container, function (fieldset)
    {
      fillFields($(multifield), fieldset);
    });
  });
};

// Initialize and fill normal fieldsets.
var fillNormalFieldsets = function (vfieldset)
{
  'use strict';

  fillFields($(vfieldset));
};

// Fill the fields with values taken from the view pane.
fillFields = function (container, context)
{
  'use strict';

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
setFieldValue = function (field, value, instance)
{
  'use strict';

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
initFieldset = function (fieldset, callback, addInstances)
{
  'use strict';

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
  'use strict';

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
  'use strict';

  $('fieldset').each(function (i, fieldset)
  {
    var fs = store($(fieldset));

    if (fs.fs('multiple') === 'false')
    {
      initFieldset(fieldset, false);
    }
  });

  return true;
};

// Remove a multifieldset. This is done after the remove button is
// pressed.
var removeFieldset = function (target)
{
  'use strict';

  target.parent().remove();
};

// Fill the fieldset with values from the view pane.
var fillFieldsets = function ()
{
  'use strict';

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

exports.initFieldset = initFieldset;
exports.fieldsetsToObject = fieldsetsToObject;
exports.initFieldsets = initFieldsets;
exports.removeFieldset = removeFieldset;
exports.fillFieldsets = fillFieldsets;

},{"../path.js":48,"../store.js":57,"../utils.js":58,"./editui.js":23}],25:[function(require,module,exports){
// # Index Listing
//
// *Implicit depends:* DOM, JSON, JQuery
//
// Loads index based on user suplied values. It also loads some other
// preliminary data, such as the listing of user created indexes. The
// `load()` function performs some initialization.

// ## Variable Definitions

var templates = require('templates.js');
var pager = require('../pager.js').pager;
var viewui = require('./viewui.js');
var editui = require('./editui.js');

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'index';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var url = 'documents/' + prefix();
  var indexId = document.getElementById('index-' + prefix() + '-input').value;
  var target = document.getElementById(prefix() + '-listing');

  var format = function (text)
  {
    var resp = JSON.parse(text);

    resp.rows = resp.rows.map(function (item)
    {
      item.display_key = item.key.map(function (k)
      {
        return k[1];
      });

      if (indexId && item.value.length > 0)
      {
        item.value = item.value.split(', ');
      }

      return item;
    });

    return resp;
  };

  pager(
  {
    prefix: prefix(),
    format: format,
    url: url,
    indexId: indexId,
    target: target
  }).get();

  return true;
};

// Loads the listing of user created indexes.
var iOpts = function ()
{
  'use strict';

  var url = 'indexes?as=options';
  var options;

  $.getJSON(url, function (data)
  {
    options = templates['index-options'](data);
    $('#index-index-input').html(options);
  });

  return true;
};

// This is the entry point that loads the data for this section of
// the application.
//
// TODO: Move to documents.js
var load = function (target)
{
  'use strict';

  var id = $(target).attr('href').slice(1);
  $('#document-view').html('<em>Loading...</em>');
  editui.clear();
  viewui.get(id);

  return true;
};

exports.prefix = prefix;
exports.get = get;
exports.iOpts = iOpts;
exports.load = load;

},{"../pager.js":46,"./editui.js":23,"./viewui.js":28,"templates.js":"e8H8MT"}],26:[function(require,module,exports){
// # The search user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the search user interface.

// Variable Definitions

var templates = require('templates.js');
var utils = require('../utils.js');
var sets = require('../sets.js');
var setsui = require('./setsui.js');
var documents = require('./documents.js');
var multipleFields;
var loadSearchVals;

// Internal functions

// User interface element
var searchIndex = function ()
{
  'use strict';

  return $('#document-search-index');
};

// User interface element
var searchIndexLabel = function ()
{
  'use strict';

  return $('#search-index-label');
};

// User interface element
var searchTerm = function ()
{
  'use strict';

  return $('#document-search-term');
};

// User interface element
var searchFields = function ()
{
  'use strict';

  return $('#document-search-field');
};

// User interface element
var searchFieldsLabel = function ()
{
  'use strict';

  return $('#search-field-label');
};

// User interface element
var searchExclude = function ()
{
  'use strict';

  return $('#document-search-exclude');
};

// User interface element
var searchInvert = function ()
{
  'use strict';

  return $('#document-search-invert');
};

// User interface element
var searchAll = function ()
{
  'use strict';

  return $('#search-all-fields-switch');
};

// User interface element
var searchListing = function ()
{
  'use strict';

  return $('#search-listing');
};

// User interface element
var getIdentifier = function ()
{
  'use strict';

  return documents.identifier();
};

// All the form elements.
var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

// If searching a user created index, the value of the hidden input
// where the index id specified.
var indexVal = function ()
{
  'use strict';

  var val = $('#index-index-input').val();
  if (val.length === 0)
  {
    return null;
  }
  else
  {
    return val;
  }
};

// Used for values that must either be true or null.
var maybeTrue = function (bool)
{
  'use strict';

  if (bool)
  {
    return true;
  }
  else
  {
    return null;
  }
};

// Clear all search information that is stored in local storage.
var clearStore = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchIndex', null);
  localStorage.setItem(ident + '_searchIndexLabel', null);
  localStorage.setItem(ident + '_searchFields', null);
  localStorage.setItem(ident + '_searchExclude', null);
  localStorage.setItem(ident + '_searchInvert', null);
};

// Clear the search form.
var clearVals = function ()
{
  'use strict';

  formElems.forEach(function (x)
  {
    var elem = x();
    switch (elem.attr('type'))
    {
    case 'hidden':
      elem.val('');
      break;
    case 'checkbox':
      elem.prop('checked', false);
      break;
    }
  });
};

// Hide all the form elements.
var hideElems = function ()
{
  'use strict';

  formElems.forEach(function (x)
  {
    var elem = x();
    switch (elem.attr('type'))
    {
    case 'hidden':
      break;
    case 'checkbox':
      elem.parent('div').hide();
      break;
    default:
      elem.hide();
    }
  });
};

// Get the field labels from session storage.
var fieldLabels = function ()
{
  'use strict';

  var ident = getIdentifier();
  var fieldlabels = JSON.parse(sessionStorage.getItem(ident + '_labels'));
  return fieldlabels;
};

// Render the search field item template using given values.
var searchFieldItem = function (field, fieldLabel)
{
  'use strict';

  return templates['search-field-item'](
  {
    fieldLabel: fieldLabel,
    field: field
  });
};

// Set the fields to search.
var setFields = function (fields)
{
  'use strict';

  var fLabels = fieldLabels();
  var jFields = JSON.stringify(fields);
  var sfls = searchFieldsLabel();
  var ident = getIdentifier();

  searchFields().val(jFields);
  localStorage.setItem(ident + '_searchFields', jFields);

  var linkLabels = fields.map(function (x)
  {
    return searchFieldItem(x, fLabels[x].join(': '));
  });

  sfls.html(linkLabels.join(' '));

  return true;
};

// Exported functions

// Put the form in a state where all fields will be searched.
var allFields = function ()
{
  'use strict';

  clearStore();
  hideElems();
  clearVals();
  return true;
};

// Put the form in a state where one field will be searched.
var singleField = function (fields)
{
  'use strict';

  multipleFields(fields);
  searchInvert().parent().show();
  return true;
};

// Put the form in a state where one field will be used to perform an
// inverse search.
var singleFieldInverse = function (fields)
{
  'use strict';

  var ident = getIdentifier();
  singleField(fields);
  searchInvert().prop('checked', true);
  localStorage.setItem(ident + '_searchInvert', true);
  return true;
};

// Put the form in a state where multiple fields will be searched.
multipleFields = function (fields)
{
  'use strict';

  allFields();
  setFields(fields);
  [searchAll(), searchFieldsLabel(), searchExclude().parent()].forEach(function (x)
  {
    x.show();
  });
  return true;
};

// Put the form in a state where fields will be excluded from search.
var excludedFields = function (fields)
{
  'use strict';

  var ident = getIdentifier();
  if (fields.length > 1)
  {
    multipleFields(fields);
  }
  else
  {
    singleField(fields);
  }
  searchExclude().prop('checked', true);
  localStorage.setItem(ident + '_searchExclude', true);
  return true;
};

// Put the form in a state where a user created index will be searched.
var indexOnly = function (index, indexLabel)
{
  'use strict';

  var ident = getIdentifier();
  allFields();
  localStorage.setItem(ident + '_searchIndex', index);
  localStorage.setItem(ident + '_searchIndexLabel', indexLabel);
  searchIndex().val(index);
  searchIndexLabel().html(indexLabel);
  [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parent()].forEach(function (x)
  {
    x.show();
  });
  return true;
};

// Put the form in a state where a user created index will be used to
// perform an inverse search.
var indexInverse = function (index, indexLabel)
{
  'use strict';

  var ident = getIdentifier();
  indexOnly(index, indexLabel);
  searchInvert().prop('checked', true);
  localStorage.setItem(ident + '_searchInvert', true);
  return true;
};

// Perform the search.
var getSearch = function ()
{
  'use strict';

  var query = searchTerm().val();
  var url = 'documents/search?q=' + window.encodeURIComponent(query);
  var field = searchFields().val();
  var exclude = searchExclude().is(':checked');
  var invert = searchInvert().is(':checked');
  var index = searchIndex().val();
  var fieldlabels = fieldLabels();

  if (index)
  {
    url = url + '&index=' + index;
  }
  else
  {
    if (field)
    {
      url = url + '&field=' + field;
    }
    if (exclude)
    {
      url = url + '&exclude=true';
    }
  }
  if (invert)
  {
    url = url + '&invert=true';
  }

  searchListing().hide();

  $.get(url, function (searchResults)
  {
    searchListing().html(searchResults);
    $('.search-result-field-id').each(function (index, item)
    {
      var label = fieldlabels[$(item).attr('data-field-field')].join(': ');
      var target = $(item).children('a').first();
      target.html(label);
      target.attr('data-search-label', label);
    });
    if (!invert)
    {
      $('.search-results th').each(function (index, item)
      {
        var itemText = $.trim($(item).children('a').html());
        var re = new RegExp('(' + query + ')', 'g');
        var newText = itemText.replace(re, '<span class="highlight">$1</span>');
        $(item).children('a').html(newText);
      });
    }
    searchListing().show();
  });

  return true;
};

// Remove a field from those that will be searched (or excluded in an
// exclusive search.)
var removeField = function (t)
{
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = $(t).attr('data-field-field');

  if (fields !== null)
  {
    newFields = fields.filter(function (x)
    {
      return x !== id;
    });
    newSearchFields = JSON.stringify(newFields);
    localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
    localStorage.setItem(ident + '_searchIndex', null);
    loadSearchVals();
  }

  return true;
};

// Add a field to those that will be searched (or excluded in an
// exclusive search.)
var addField = function (t)
{
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = $(t).attr('data-field-field');

  if (fields === null)
  {
    fields = [];
  }

  newFields = sets.union(fields, id);
  newSearchFields = JSON.stringify(newFields);
  localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
  localStorage.setItem(ident + '_searchIndex', null);
  loadSearchVals();

  return true;
};

// Add a user created index to be searched.
var addIndex = function ()
{
  'use strict';

  var val = indexVal();
  var ident = getIdentifier();

  if (val)
  {
    localStorage.setItem(ident + '_searchFields', null);
    localStorage.setItem(ident + '_searchIndex', val);
    localStorage.setItem(ident + '_searchIndexLabel', $('option[value=' + val + ']').html());
    loadSearchVals();
  }

  return true;
};

// Toggle the inverse search setting.
var toggleInversion = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchInvert', maybeTrue(searchInvert().is(':checked')));
  localStorage.setItem(ident + '_searchExclude', null);
  loadSearchVals();

  return true;
};

// Toggle the exclusive search setting.
var toggleExclusion = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchExclude', maybeTrue(searchExclude().is(':checked')));
  localStorage.getItem(ident + '_searchInvert', null);
  loadSearchVals();

  return true;
};

// The functions that alter the search form above store the values in
// local storage. This interprets those values and puts the search form
// in a consistent state.
loadSearchVals = function ()
{
  'use strict';

  var ident = getIdentifier();
  var exclude = localStorage.getItem(ident + '_searchExclude');
  var invert = localStorage.getItem(ident + '_searchInvert');
  var index = localStorage.getItem(ident + '_searchIndex');
  var fieldids = localStorage.getItem(ident + '_searchFields');
  var fields;
  var indexLabel;
  var params = [exclude, invert, index, fieldids].map(function (x)
  {
    return (x === 'null' || x === 'false' || x === 'true') ? JSON.parse(x) : x;
  });
  var allNull = params.every(function (x)
  {
    return x === null;
  });

  try
  {
    if (allNull)
    {
      allFields();
    }
    else if (params[0] === true)
    {
      fields = JSON.parse(fieldids);
      excludedFields(fields);
    }
    else if (params[1] === null && params[3] !== null)
    {
      fields = JSON.parse(fieldids);
      if (fields.length > 1)
      {
        multipleFields(fields);
      }
      else
      {
        singleField(fields);
      }
    }
    else if (params[3] !== null)
    {
      fields = JSON.parse(fieldids);
      if (fields.length > 1)
      {
        multipleFields(fields);
      }
      else
      {
        singleFieldInverse(fields);
      }
    }
    else if (params[1] === null)
    {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexOnly(index, indexLabel);
    }
    else if (params[1] === true)
    {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexInverse(index, indexLabel);
    }
  }
  catch (e)
  {
    window.console.log(e);
    allFields();
  }

  return true;
};

// Toggle selection of result to save to set.
var toggleSelection = function (t)
{
  'use strict';

  var target = $(t);

  if (target.is(':checked'))
  {
    target.next('label').next('table').addClass('selected-for-save');
  }
  else
  {
    target.next('label').next('table').removeClass('selected-for-save');
  }

  return true;
};

exports.allFields = allFields;
exports.singleField = singleField;
exports.singleFieldInverse = singleFieldInverse;
exports.multipleFields = multipleFields;
exports.excludedFields = excludedFields;
exports.indexOnly = indexOnly;
exports.indexInverse = indexInverse;
exports.getSearch = getSearch;
exports.removeField = removeField;
exports.addField = addField;
exports.addIndex = addIndex;
exports.toggleInversion = toggleInversion;
exports.toggleExclusion = toggleExclusion;
exports.loadSearchVals = loadSearchVals;
exports.toggleSelection = toggleSelection;

},{"../sets.js":50,"../utils.js":58,"./documents.js":22,"./setsui.js":27,"templates.js":"e8H8MT"}],27:[function(require,module,exports){
// # The sets user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the sets user interface.

// Variable Definitions

var templates = require('templates.js');
var S = require('../sender.js');
var flash = require('../flash.js');
var sets = require('../sets.js');
var utils = require('../utils.js');
var documents = require('./documents.js');
var removeSet;
var setSets;
var selectedElementsToArray;
var selectedSaveResultsToArray;
var render;
var getSets;
var getSet;

// Internal functions

// User interface element
var setA = function ()
{
  'use strict';

  return $('#document-set-a-input');
};

// User interface element
var setB = function ()
{
  'use strict';

  return $('#document-set-b-input');
};

// User interface element
var worksheetsSet = function ()
{
  'use strict';

  return $('#document-worksheets-set-input');
};

// User interface element
var op = function ()
{
  'use strict';

  return $('#document-set-operation-input');
};

// User interface element
var setListing = function ()
{
  'use strict';

  return $('#set-listing');
};

// User interface element
var sessionKey = function ()
{
  'use strict';

  return documents.identifier() + '_sets';
};

// Custom member function to use with [sets.js](./sets.html).
var member = function (arr, x)
{
  'use strict';

  return arr.some(function (y)
  {
    return x[0] === y[0] && x[1] === y[1];
  });
};

// Ensure that the set is correct.
var processSet = function (set)
{
  'use strict';

  var name = set[0];
  var arr = sets.unique(set[1], member);
  var procSet = [name, arr];
  return procSet;
};

// Perform the union of the sets specified by the user interface.
var union = function (setNameA, setNameB)
{
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.union(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the intersection of the sets specified by the user interface.
var intersection = function (setNameA, setNameB)
{
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.intersection(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the relative complement of the sets specified by the user
// interface.
var relativeComplement = function (setName1, setName2)
{
  'use strict';

  var setElems1 = getSet(setName1)[1];
  var setElems2 = getSet(setName2)[1];
  var newSet = sets.relativeComplement(setElems1, setElems2, member);
  render(newSet);

  return true;
};

// Perform the symmetric difference of the sets specified by the user
// interface.
var symmetricDifference = function (setNameA, setNameB)
{
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.symmetricDifference(setElemsA, setElemsB, member);
  render(newSet);

  return true;
};

// Get the sets saved in session storage
getSets = function ()
{
  'use strict';

  var curr = window.sessionStorage.getItem(sessionKey());
  var retval = [];

  if (curr !== null)
  {
    retval = JSON.parse(curr);
  }

  return retval;
};

// View a set.
var view = function (setName)
{
  'use strict';

  var elems = getSet(setName)[1];
  render(elems);

  return true;
};

// Remove a set.
var remove = function (setName)
{
  'use strict';

  removeSet(setName);
  render([]);
  S.sender('sets-changed');

  return true;
};

// Perform set removal.
removeSet = function (setName)
{
  'use strict';

  var nnew;
  var curr = getSets();
  nnew = curr.filter(function (x)
  {
    return x[0] !== setName;
  });
  setSets(nnew);

  return true;
};

// Retrieve the set names.
var getSetNames = function ()
{
  'use strict';

  var curr = getSets();
  return curr.map(function (x)
  {
    return x[0];
  });
};

// Save sets to session storage.
setSets = function (nnew)
{
  'use strict';

  var procSets;
  if (Array.isArray(nnew))
  {
    procSets = nnew.map(function (x)
    {
      return processSet(x);
    });
    window.sessionStorage.setItem(sessionKey(), JSON.stringify(procSets));
  }
  else
  {
    window.sessionStorage.settem(sessionKey(), '[]');
  }

  return true;
};

// Save a set to session storage.
var setSet = function (nnew)
{
  'use strict';

  if (Array.isArray(nnew) && nnew.length === 2)
  {
    var curr = getSets();
    var newName = nnew[0];
    var filtered = curr.filter(function (x)
    {
      return x[0] !== newName;
    });
    setSets(filtered.concat([nnew]));
  }
  return true;
};

// Convert selected search results or a selected elements to an array.
var selectedToArray = function (target)
{
  'use strict';

  var retval = [];

  switch (target)
  {
  case 'search':
    retval = selectedSaveResultsToArray();
    break;
  case 'sets':
    retval = selectedElementsToArray();
    break;
  }

  return retval;
};

// Convert selected elements to an array.
selectedElementsToArray = function ()
{
  'use strict';

  var retval;
  var selected = $('input.set-element-selection:checked');

  retval = $.map(selected, function (elem)
  {
    var anchor = $(elem).parent('td').next('td').find('a').first();
    var id = anchor.first().attr('href').replace(/^#/, '');
    var context = anchor.html().trim();
    return [[context, id]];
  });
  return retval;
};

// Convert selected search results to an array.
selectedSaveResultsToArray = function ()
{
  'use strict';

  var retval;
  var selected = $('table.selected-for-save tr');

  retval = $.map(selected, function (elem)
  {
    var id = $(elem).find('th a').first().attr('href').replace(/^#/, '');
    var context = $(elem).find('td.search-result-context a').first().html().trim();
    return [[context, id]];
  });

  return retval;
};

// Render the set for display.
render = function (setElems)
{
  'use strict';

  var total = setElems.length;
  var elems = setElems.map(function (x)
  {
    return {
      id: x[1],
      context: x[0]
    };
  });
  var listing = templates['set-listing'](
  {
    elements: elems,
    total: total
  });
  setListing().html(listing);
  return true;
};

// Exported functions

// Retrieve a set.
var getSet = function (setName)
{
  'use strict';

  var retval;
  var curr = getSets();
  retval = curr.filter(function (x)
  {
    return x[0] === setName;
  })[0];
  return retval;
};

// Perform a set operation.
var performOp = function ()
{
  'use strict';

  switch (op().val())
  {
  case 'view-a':
    view(setA().val());
    break;
  case 'view-b':
    view(setB().val());
    break;
  case 'remove-a':
    remove(setA().val());
    break;
  case 'remove-b':
    remove(setB().val());
    break;
  case 'union':
    union(setA().val(), setB().val());
    break;
  case 'intersection':
    intersection(setA().val(), setB().val());
    break;
  case 'symmetric-difference':
    symmetricDifference(setA().val(), setB().val());
    break;
  case 'relative-complement-b-in-a':
    relativeComplement(setA().val(), setB().val());
    break;
  case 'relative-complement-a-in-b':
    relativeComplement(setB().val(), setA().val());
    break;
  default:
    break;
  }
  return true;
};

// Update the selection of sets to choose from.
var updateSelection = function ()
{
  'use strict';

  var currNames = getSetNames();
  var newOptions = templates['set-options'](
  {
    names: currNames
  });
  setA().html(newOptions);
  setB().html(newOptions);
  worksheetsSet().html(newOptions);

  return true;
};

// Save select items as a set.
var saveSelected = function ()
{
  'use strict';

  var dialog = $('#new-set-dialog');
  var name = $('#new-set-input').val();
  var target = $('#new-set-target-input').val();
  var selected;
  var newSet;

  if (!utils.isBlank(name))
  {
    dialog.hide();
    selected = selectedToArray(target);
    newSet = [name, selected];
    setSet(newSet);
    $('#new-set-input').val('');
    S.sender('sets-changed');
    flash.highlight('Success:', 'Set "' + name + '" saved.');
  }
  else
  {
    flash.error('Input invalid:', 'You must supply a valid name.');
  }

  return true;
};

// Toggle the selection of all elements.
var toggleSelectAll = function (target)
{
  'use strict';

  if ($(target).is(':checked'))
  {
    $('input.set-element-selection').prop('checked', true);
  }
  else
  {
    $('input.set-element-selection').prop('checked', false);
  }
  return true;
};

exports.getSet = getSet;
exports.performOp = performOp;
exports.updateSelection = updateSelection;
exports.saveSelected = saveSelected;
exports.toggleSelectAll = toggleSelectAll;

},{"../flash.js":31,"../sender.js":55,"../sets.js":50,"../utils.js":58,"./documents.js":22,"templates.js":"e8H8MT"}],28:[function(require,module,exports){
// # The view user interface
//
// *Implicit depends:* DOM, JQuery
//
// View pane UI elements.
//
// *TODO* I may be exporting more than needed.

// Variable Definitions

var templates = require('templates.js');
var store = require('../store.js').store;
var indexui = require('./indexui.js');
var flash = require('../flash.js');
var editui = require('./editui.js');
var fieldsets = require('./fieldsets.js');

// Internal functions

// User interface element
var dv = function ()
{
  'use strict';

  return $('#document-view');
};

// User interface element
var dvt = function ()
{
  'use strict';

  return $('#document-view-tree');
};

// User interface element
var viewInfo = function ()
{
  'use strict';

  return $('#document-view-info');
};

// Make an object where fieldsets with deletions are identified.
var getDeletions = function (changes)
{
  'use strict';

  return Object.keys(changes).reduce(function (acc, x)
  {
    // If it was changed and there is no new value, it was deleted.
    if (changes[x].newValue === undefined)
    {
      if (acc[changes[x].fieldset] === undefined)
      {
        acc[changes[x].fieldset] = {};
      }
      acc[changes[x].fieldset][x] = changes[x];
    }

    return acc;
  },
  {});
};

// Process the document from the server.
var processIncoming = function (docJson, rev)
{
  'use strict';

  var withDeletions = {};

  if (docJson.changes)
  {
    withDeletions = getDeletions(docJson.changes);
  }

  docJson.fieldsets.forEach(function (fset)
  {
    var fsetId = fset.id;

    if (withDeletions[fsetId] !== undefined)
    {
      fset.removal = true;
      fset.altered = true;
    }

    var fieldFunc = function (field)
    {
      var changes = {};
      var change;

      if (docJson.changes)
      {
        changes = docJson.changes;
      }
      change = changes[field.instance];

      field.json_value = JSON.stringify(field.value);

      if (change !== undefined)
      {
        field.changed = true;
        fset.altered = true;

        if (change.originalValue === undefined)
        {
          fset.addition = true;
          field.newfield = true;
        }
        else
        {
          field.originalValue = JSON.parse(change.originalValue);
        }
      }

      if (field.subcategory === 'textarea')
      {
        field.is_textarea = true;
      }
      else if (field.value && field.subcategory.match('multi'))
      {
        field.value = field.value.join(', ');
      }

      return true;
    };

    if (fset.multiple)
    {
      fset.multifields.forEach(function (mfs)
      {
        mfs.fields.forEach(function (field)
        {
          fieldFunc(field);
          return true;
        });
      });
    }
    else
    {
      fset.fields.forEach(function (field)
      {
        fieldFunc(field);
        return true;
      });
    }

    return true;
  });

  return true;
};

// Exported functions

// Format the 'update at' and 'created at' timestamps and localize them
// to the current time zone.
var formatTimestamps = function ()
{
  'use strict';

  $('.timestamp').each(

  function (i, item)
  {
    var newDate = (new Date($(item).text())).toLocaleString();
    if (newDate !== 'Invalid Date')
    {
      $(item).text(newDate);
    }
  });

  return true;
};

// Get the document.
var get = function (id, rev, callback)
{
  'use strict';

  var url = 'documents/' + id;
  var htmlTarget = dv();
  var tmpl;

  if (rev)
  {
    url = url + '/' + rev;
    htmlTarget = dvt();
    tmpl = function (docJson)
    {
      return templates['document-view-tree'](docJson);
    };
  }
  else
  {
    tmpl = function (docJson)
    {
      return templates['document-view'](docJson);
    };

  }

  $.getJSON(url, function (docJson)
  {
    var documentHtml;

    processIncoming(docJson, rev);
    documentHtml = tmpl(docJson);
    htmlTarget.html(documentHtml);
    window.location.hash = id;
    formatTimestamps();
    dv().fadeTo('slow', 1);
    if (callback)
    {
      callback();
    }

    if (rev)
    {
      $('#document-view-tree').addClass('oldrev');
    }
    else
    {
      var restoreButton = $('#document-restore-button');
      var editButton = $('#document-edit-button');
      var deleteButton = $('#document-delete-button');

      if (store(restoreButton).d('deleted') === 'true')
      {
        editButton.hide();
        deleteButton.hide();
        restoreButton.show();
      }
    }
  });

  return true;
};

// Restore the state of a document to that of an earlier revision.
var restore = function (id, rev)
{
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var restoreButton = $('#document-restore-button');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var body;
  var title;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 200)
      {
        title = 'Success';
        body = 'Your document was restored.';

        get(id, null, function ()
        {
          dv().fadeTo('slow', 1);
          indexui.get(skey, sid);
        });
        flash.highlight(title, body);
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
      }
      else if (req.status === 404)
      {
        body = 'Document was erased and cannot be restored.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return true;
};

// Delete the document.
var del = function (id, rev)
{
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var restoreButton = $('#document-restore-button');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var body;
  var title;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 200)
      {
        title = 'Success';
        body = 'Your document was deleted.';
        var response = JSON.parse(req.responseText);

        store(restoreButton).put('document-rev', response.rev);

        $('#document-delete-button').hide();
        $('#document-edit-button').hide();
        restoreButton.show();
        dv().fadeTo('slow', 0.5);

        indexui.get(skey, sid);
        flash.highlight(title, body);
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
      }
      else if (req.status === 404)
      {
        body = 'Document appears to have been deleted already.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return true;
};

// Confirm an action.
var confirmIt = function (callback)
{
  'use strict';

  if (window.confirm('Are you sure?'))
  {
    var s = store(viewInfo());
    var id = s.d('document');
    var rev = s.d('rev');

    callback(id, rev);
  }

  return true;
};

// Move the document to the editor.
var edit = function ()
{
  'use strict';

  editui.resetFields();
  if ($('#document-view-tree').hasClass('oldrev'))
  {
    $('#save-document-button').addClass('oldrev');
  }
  else
  {
    $('#save-document-button').removeClass('oldrev');
  }
  fieldsets.fillFieldsets();

  return true;
};

// Ask for confirmation on deletion.
var confirmDelete = function ()
{
  'use strict';

  var s = store(viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');
  return confirmIt(function ()
  {
    del(id, rev);
  });
};

// Ask for confirmation on restoration.
var confirmRestore = function ()
{
  'use strict';

  var s = store(viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');
  return confirmIt(function ()
  {
    restore(id, rev);
  });
};

// Expand and collapse elements of the view tree.
var collapseToggle = function (target)
{
  'use strict';

  $(target).parent('li').toggleClass('collapsed');

  return true;
};

// Get a previous revision.
var fetchRevision = function (target)
{
  'use strict';

  var s = store($(target));
  var id = s.d('document');
  var oldrev = s.d('oldrev');

  $('.revision-link').removeClass('selected-revision');
  $(target).addClass('selected-revision');

  get(id, oldrev);

  return true;
};

exports.formatTimestamps = formatTimestamps;
exports.get = get;
exports.restore = restore;
exports.del = del;
exports.confirmIt = confirmIt;
exports.edit = edit;
exports.confirmDelete = confirmDelete;
exports.confirmRestore = confirmRestore;
exports.collapseToggle = collapseToggle;
exports.fetchRevision = fetchRevision;

},{"../flash.js":31,"../store.js":57,"./editui.js":23,"./fieldsets.js":24,"./indexui.js":25,"templates.js":"e8H8MT"}],29:[function(require,module,exports){
// # The worksheet user interface
//
// *Implicit depends:* DOM, JQuery, globals
// ([application.js](./application.html))
//
// Worksheet pane UI elements.

// Variable Definitions

var Hogan = require('hogan.js');
var templates = require('templates.js');
var setsui = require('./setsui.js');
var documents = require('./documents.js');
var form = require('../form.js');
var flash = require('../flash.js');

// Internal functions

// User interface element
var worksheetsSet = function ()
{
  'use strict';

  return $('#document-worksheets-set-input');
};

// User interface element
var worksheetsArea = function ()
{
  'use strict';

  return $('#worksheet-area');
};

// Name for the worksheet template.
var worksheetName = function ()
{
  'use strict';

  return documents.identifier() + '_worksheet-template';
};

// Exported functions

// Select all the visible rows.
var selectAllRows = function (select)
{
  'use strict';

  if (select)
  {
    $('#worksheet-table tbody tr').addClass('selected-row');
    $('#worksheet-table tbody tr input').prop('checked', true);
  }
  else
  {
    $('#worksheet-table tbody tr').removeClass('selected-row');
    $('#worksheet-table tbody tr input:checked').prop('checked', false);
  }

  return true;
};

// Set the proper class for a selected row and unset the 'select all'
var rowSelection = function (row, select)
{
  'use strict';

  if (select)
  {
    $('#' + row).addClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }
  else
  {
    $('#' + row).removeClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }

  return true;
};

// Select a column.
var columnSelection = function (column, select)
{
  'use strict';

  if (select)
  {
    $('.field-column.' + column).addClass('selected-column');
  }
  else
  {
    $('.field-column.' + column).removeClass('selected-column');
  }

  return true;
};

// Show vertical headers for fields and fieldsets.
var showHandles = function ()
{
  'use strict';

  $('#worksheet-table .handle-column.fieldset').show();

  return true;
};

// Hide vertical headers for fields and fieldsets.
var hideHandles = function ()
{
  'use strict';

  $('#worksheet-table .handle-column.fieldset').hide();

  return true;
};

// Show the fieldset handle.
var showFieldset = function (fsid)
{
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).show();

  return true;
};

// Hide the fieldset handle.
var hideFieldset = function (fsid)
{
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).hide();

  return true;
};

// Show a field.
var showField = function (fid)
{
  'use strict';

  $('.field-column.' + fid).show();

  return true;
};

// Hide a field.
var hideField = function (fid)
{
  'use strict';

  $('.field-column.' + fid).hide();

  return true;
};

// There are two layers of templating information in the
// template. Activate the second layer.
var buildTemplate = function ()
{
  'use strict';

  var doctypeInfo = documents.info();
  var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'](doctypeInfo);
  globals[worksheetName()] = Hogan.compile(metaTemp);

  return true;
};

// Render the worksheet.
var fillWorksheet = function ()
{
  'use strict';

  var setName = worksheetsSet().val();
  var url = 'worksheets';
  var complete = function (_ignore, req)
  {
    var data = JSON.parse(req.responseText);
    var ws = globals[worksheetName()].render(data);
    worksheetsArea().html(ws);
  };

  if (!setName.isBlank())
  {
    var thisSet = setsui.getSet(setName)[1];

    if (thisSet.length <= 250)
    {
      var setIds = thisSet.map(function (x)
      {
        return x[1];
      });

      form.send(url, setIds, 'POST', complete);
    }
    else
    {
      flash.error('Could not load worksheet', 'the current set size is limited to 250 items.');
    }
  }

  return true;
};

exports.selectAllRows = selectAllRows;
exports.rowSelection = rowSelection;
exports.columnSelection = columnSelection;
exports.showHandles = showHandles;
exports.hideHandles = hideHandles;
exports.showFieldset = showFieldset;
exports.hideFieldset = hideFieldset;
exports.showField = showField;
exports.hideField = hideField;
exports.buildTemplate = buildTemplate;
exports.fillWorksheet = fillWorksheet;

},{"../flash.js":31,"../form.js":32,"./documents.js":22,"./setsui.js":27,"hogan.js":"nLm5Ax","templates.js":"e8H8MT"}],30:[function(require,module,exports){
// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with CouchDB attachments within documents that
// exist only for the pupose of holding the attachment. A mock-file
// system path is given to these saved documents and may be used to
// retrieve them instead of the ID.

// Variable Definitions

var form = require('../form.js');
var flash = require('../flash.js');
var refreshListings;

// Internal functions

// Get information subdirectories within a path. As an example
// '/home/chuck/'.
var getDirListing = function (path)
{
  'use strict';

  if (path === undefined)
  {
    path = '';
  }

  $.get('file_manager/list_dirs/' + path, function (data)
  {
    $('#file-paths').html(data);
  });
};

// Get the document information for documents with a certain path.
var getFileListing = function (path)
{
  'use strict';

  if (path === undefined)
  {
    path = '';
  }

  $.get('file_manager/list_files/' + path, function (data)
  {
    $('#file-listing').html(data);
  });
};

// Open a dialog for editing a file path.
var pathEditDialog = function (obj, path)
{
  'use strict';

  var pathInput = $('#file-path-input');

  if (obj.path)
  {
    pathInput.val(obj.path.join('/'));
  }
  else
  {
    pathInput.val('');
  }

  var dialog = $('#edit-path-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Move': function ()
      {
        var url = 'file_manager/' + obj._id + '?rev=' + obj._rev;
        var complete = function ()
        {
          refreshListings(path);
          flash.highlight('Success', 'File Moved');
        };

        obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split('/');
        form.send(url, obj, 'PUT', complete, dialog);
        $(this).dialog('close');
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    }
  });

  return dialog;
};

// Exported functions

// Initialize the sub-application.
var init = function ()
{
  'use strict';

  refreshListings();
  $('#file-upload-target').load(function ()
  {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function ()
    {
      if (encoded && encoded.length > 0)
      {
        return JSON.parse(encoded);
      }
      else
      {
        return {
          message: false
        };
      }
    };

    if (obj() && obj().message && obj().status !== 'success')
    {
      flash.error('Error', obj().message);
      refreshListings();
    }
    else if (obj().message)
    {
      flash.highlight('Success', obj().message);
      refreshListings();
    }
  });
};

// Handle the mouse click action that initiates going to a directory.
var goDir = function (target)
{
  'use strict';

  var newpath = $(target).attr('data-path');
  window.sessionStorage.fmPath = newpath;
  refreshListings(newpath);

  return true;
};

// Return to the root directory.
var rootDir = function ()
{
  'use strict';

  var path = window.sessionStorage.fmPath = '';
  refreshListings();

  return true;
};

// Move up a directory.
var upDir = function ()
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var newpath = path.split('/');
  newpath.pop();
  newpath = newpath.join('/');
  window.sessionStorage.fmPath = newpath;

  refreshListings(newpath);

  return true;
};

// Handle the mouse click action that initiates editing a file by opening
// a dialog to edit its path.
var editFile = function (target)
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var url = 'file_manager/' + fileId;

  $.getJSON(url, function (obj)
  {
    pathEditDialog(obj, path).dialog('open');
  });

  return true;
};

// Handle the mouse click action that initiates deleting a file.
var deleteFile = function (target)
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var fileRev = target.attr('data-file-rev');
  var url = 'file_manager/' + fileId + '?rev=' + fileRev;
  var complete = function ()
  {
    refreshListings(path);
    flash.highlight('Success', 'File Deleted');
  };

  form.send(url, null, 'DELETE', complete, target);

  return true;
};

// Refresh the file listing using the given path.
refreshListings = function (path)
{
  'use strict';

  getDirListing(path);
  getFileListing(path);
};

exports.init = init;
exports.goDir = goDir;
exports.rootDir = rootDir;
exports.upDir = upDir;
exports.editFile = editFile;
exports.deleteFile = deleteFile;
exports.refreshListings = refreshListings;

},{"../flash.js":31,"../form.js":32}],31:[function(require,module,exports){
// # Brief Notification Messages
//
// *Implicit depends:* DOM, JQuery
//
// Helpers to display notifications.

// ## Internal Functions

// Helper function that handles the displaying and fading of the flashed
// notification.
var f = function (flasher, title, body)
{
  'use strict';

  var fadeout = function ()
  {
    flasher.fadeOut();
  };
  flasher.find('.notification-summary').text(title + ': ');
  flasher.find('.notification-message').text(body);
  var timeout = window.setTimeout(fadeout, 7000);
  flasher.fadeIn();
  flasher.find('.close').click(function ()
  {
    window.clearTimeout(timeout);
    flasher.hide();
  });
};

// # Exported Functions

// Display an error.
var error = function (title, body)
{
  'use strict';

  f($('#notifications-main .ui-state-error'), title, body);

  return true;
};

// Display a notification.
var highlight = function (title, body)
{
  'use strict';

  f($('#notifications-main .ui-state-highlight'), title, body);

  return true;
};

exports.error = error;
exports.highlight = highlight;

},{}],32:[function(require,module,exports){
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

},{"./flash.js":31}],33:[function(require,module,exports){
// # Formalize
//
// *implicit dependencies:* JSON
//
// Convert JSON to and from an HTML form. Also create forms from JSON
// Schema.

// ## Variable Definitions

var r = require('./recurse.js');
var templates = require('templates.js');

// ## Internal Functions

var validateToArg = function (obj)
{
  'use strict';

  var msg = 'cannot build form from: ';

  if (obj === null)
  {
    throw msg + 'null';
  }
  else if (typeof obj === 'string')
  {
    throw msg + 'string';
  }
  else if (typeof obj === 'number')
  {
    throw msg + 'number';
  }
  else if (obj.constructor === Array)
  {
    throw msg + 'array';
  }
  else if (Object.keys(obj).length === 0)
  {
    throw msg + 'empty object';
  }

  return obj;
};

var tryParseJSON = function (jsn)
{
  'use strict';

  var obj;

  try
  {
    obj = JSON.parse(jsn);
  }
  catch (e)
  {
    switch (e.name)
    {
    case 'SyntaxError':
      e.message = 'invalid JSON: ' + JSON.stringify(jsn);
      throw e;
    default:
      throw e;
    }
  }

  return obj;
};

var simpleToForm = function (obj)
{
  'use strict';

  var fields = Object.keys(obj).reduce(function (acc, key)
  {
    var val = obj[key];
    var ret = {key: key, val: val};

    if (typeof val === 'string' && val.length <= 32)
    {
      ret.string = true;
    }
    else if (typeof val === 'number')
    {
      ret.number = true;
    }
    else if (typeof val === 'string' && val.length > 32)
    {
      ret.text = true;
    }

    return acc.concat(ret);
  }, []);

  return templates['simple-to-form']({fields: fields});
};

// ## External Functions

var toForm = function (jsn)
{
  'use strict';

  var obj = tryParseJSON(jsn);

  obj = validateToArg(obj);

  return simpleToForm(obj);
};

exports.toForm = toForm;

},{"./recurse.js":53,"templates.js":"e8H8MT"}],34:[function(require,module,exports){
// # Globals object
//
// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
//
// This is not loaded as a module like other code here. It is concatenated
// to the beginning of the target JavaScript file created by the build
// process.

var globals = {};

},{}],35:[function(require,module,exports){
// # Builder dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding conditions to user created indexes.

// TODO I would rather avoid having this as a JQuery plugin.

require('../jquery-ui-input-state.js');

// Variable Definitions

var ihelpers = require('./ihelpers.js');
var form = require('../form.js');
var evs = require('./ievents.js');

// Exported functions

// The dialog for adding a condition to an index.
var initIndexBuilderDialog = function (indexDoctype)
{
  'use strict';

  var builderOr = $('#builder-or-input');
  var builderParen = $('#builder-paren-input');
  var builderNegate = $('#builder-negate-input');
  var builderOperator = $('#builder-operator-input').inputDisable();
  var builderArgument = $('#builder-argument-input').inputDisable();
  var builderFieldset = $('#builder-fieldset-input').inputDisable();
  var builderField = $('#builder-field-input').inputDisable();
  var notBlank = [builderOperator, builderFieldset, builderField];
  var fieldset_url = 'doctypes/' + indexDoctype + '/fieldsets';
  var condition_url = 'indexes/condition';

  $('.ui-helper-reset div').show();

  var appendCondition = function (builderRow)
  {
    var tableBody = $('#index-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();

    return false;
  };

  ihelpers.fOpts(fieldset_url, builderFieldset, function ()
  {
    builderFieldset.inputEnable();
  });

  builderOr.change(function ()
  {
    if (builderOr.is(':checked'))
    {
      $('#builder-conditions').hide();
      $('#builder-parens').hide();
    }
    else
    {
      $('#builder-conditions').show();
      $('#builder-parens').show();
    }
  });

  builderParen.change(function ()
  {
    if (builderParen.val())
    {
      $('#builder-or').hide();
      $('#builder-conditions').hide();
    }
    else
    {
      $('#builder-or').show();
      $('#builder-conditions').show();
    }
  });

  var fieldsetEvents = function ()
  {
    evs.setIndexFieldsetEvents(indexDoctype, builderFieldset, builderField, function ()
    {
      builderOperator.inputDisable();
      builderField.inputDisable();
      builderArgument.inputDisable();

      return function ()
      {
        builderField.inputEnable();
      };
    });
  };

  var fieldEvents = function ()
  {
    evs.setIndexFieldEvents(indexDoctype, builderFieldset, builderField, function ()
    {
      builderOperator.inputDisable();
      builderArgument.inputDisable();

      return function ()
      {
        builderOperator.inputEnable();
      };
    });
  };

  var operatorEvents = function ()
  {
    evs.setIndexOperatorEvents(builderArgument, builderOperator, builderField, function ()
    {
      builderArgument.inputDisable();

      return function ()
      {
        builderArgument.inputEnable();
      };
    });
  };

  var dialog = $('#index-builder-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Create': function ()
      {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!builderOr.is(':checked') && !builderParen.val())
        {
          notBlank.forEach(function (item)
          {
            if (item.val().isBlank())
            {
              item.addClass('ui-state-error');
              checkResult = false;
            }
            else
            {
              item.removeClass('ui-state-error');
            }
          });
        }

        if (checkResult)
        {
          if (builderOr.is(':checked'))
          {
            $.get(condition_url,
            {
              'is_or': true
            }, function (data)
            {
              appendCondition(data);
            });
          }
          else if (builderParen.val())
          {
            $.get(condition_url,
            {
              'is_or': false,
              'parens': builderParen.val(),
              'negate': false
            }, function (data)
            {
              appendCondition(data);
            });
          }
          else
          {
            $.get(condition_url,
            {
              'is_or': false,
              'parens': false,
              'negate': builderNegate.is(':checked'),
              'fieldset': builderFieldset.val(),
              'field': builderField.val(),
              'operator': builderOperator.val(),
              'argument': builderArgument.val()
            }, function (data)
            {
              appendCondition(data);
            });
          }

          $(this).dialog('close');
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      $('#builder-conditions').show();
      builderFieldset.unbind('change');
      builderField.unbind('change');
      builderOperator.unbind('change');
      form.clear($('.input')).removeClass('ui-state-error');
    }
  });

  fieldsetEvents();
  fieldEvents();
  operatorEvents();

  return dialog;
};

exports.initIndexBuilderDialog = initIndexBuilderDialog;

},{"../form.js":32,"../jquery-ui-input-state.js":43,"./ievents.js":37,"./ihelpers.js":38}],36:[function(require,module,exports){
// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for manipulating index conditions.

// Variable Definitions

var initIndexNewDialog = require('./new-dialog.js').initIndexNewDialog;
var initIndexBuilderDialog = require('./builder-dialog.js').initIndexBuilderDialog;
var initReplaceDialog = require('./replace-dialog.js').initReplaceDialog;
var ilistingui = require('./ilistingui.js');
var ipreviewui = require('./ipreviewui.js');
var ihelpers = require('./ihelpers.js');
var form = require('../form.js');
var flash = require('../flash.js');

// Internal functions

// User interface element
var tableBody = function ()
{
  'use strict';

  return $('#index-conditions-listing tbody');
};

// User interface element
var editingData = function ()
{
  'use strict';

  return $('#index-editing-data');
};

// Make sure the arguments are of the correct type.
var fixArgumentType = function (argument, subcategory, operator)
{
  'use strict';

  switch (subcategory)
  {
  case 'integer':
  case 'rational':
    argument = argument * 1;
    break;
  }

  switch (operator)
  {
  case 'hasExactly':
  case 'hasGreater':
  case 'hasLess':
    argument = Math.floor(argument * 1);
    break;
  }

  return argument;
};

// Use data in `data` attributes of HTML elements to produce an array
// of conditions.
var getIndexConditions = function (doctypeId, rows)
{
  'use strict';

  var conditions = rows.map(

  function (index, row)
  {
    row = $(row);
    var is_or = row.find('td.or-condition').attr('data-value') === 'true';
    var paren = row.find('td.paren-condition').attr('data-value');
    var condition;

    if (is_or)
    {
      condition = {
        'is_or': true,
        'parens': false
      };
    }
    else if (paren)
    {
      condition = {
        'is_or': false,
        'parens': paren
      };
    }
    else
    {
      var fieldId = row.find('td.field-condition').attr('data-value');
      var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
      var argument = row.find('td.argument-condition').attr('data-value');
      var fieldDoc = ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
      var negate =
        row.find('td.negate-condition').attr('data-value') === 'true';
      var operator = row.find('td.operator-condition').attr('data-value');

      argument = fixArgumentType(argument, fieldDoc.subcategory, operator);

      condition = {
        'is_or': false,
        'parens': false,
        'negate': negate,
        'fieldset': fieldsetId,
        'field': fieldId,
        'operator': operator,
        'argument': argument
      };
    }

    return condition;
  }).toArray();

  return conditions;
};

// Initiate the save action.
var saveIndex = function (buttonData, completeFunction)
{
  'use strict';

  var indexId = buttonData.attr('data-index-id');
  var indexRev = buttonData.attr('data-index-rev');
  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var doctype = buttonData.attr('data-index-doctype');

  var obj = {
    '_id': indexId,
    'category': 'index',
    'doctype': doctype,
    'show_deleted': buttonData.attr('data-index-show_deleted') === 'true',
    'fields': JSON.parse(buttonData.attr('data-index-fields')),
    'fields_label': JSON.parse(buttonData.attr('data-index-fields_label')),
    'name': buttonData.attr('data-index-name'),
    'conditions': getIndexConditions(doctype, $('#index-conditions-listing tbody tr'))
  };

  if (buttonData.attr('data-index-replace_function'))
  {
    obj.replace_function = buttonData.attr('data-index-replace_function');
  }

  form.send(url, obj, 'PUT', completeFunction, this);

  return false;
};

// Initiate the delete action.
var deleteIndex = function (indexId, indexRev, completeMessage, completeFunction)
{
  'use strict';

  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var title;
  var body;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 204)
      {
        title = 'Success';
        body = completeMessage;

        completeFunction();

        flash.highlight(title, body);
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
      }
      else if (req.status === 404)
      {
        body = 'Index appears to have been deleted already.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return false;
};

// Exported functions

// Initialize the index editing user interface.
var init = function (target)
{
  'use strict';

  var indexId = $(target).attr('data-index-id');
  var url = 'indexes/' + indexId;
  var htmlTarget = $('#index-conditions');

  $.get(url, function (indexData)
  {
    htmlTarget.html(indexData);
    tableBody().sortable();
    ipreviewui.get();
  });

  return false;
};

// Save the index.
var save = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    var completeFunction = function ()
    {
      init(bData);
      flash.highlight('Success', 'Your index has been saved.');
    };

    saveIndex(bData, completeFunction);
  }
  else
  {
    flash.highlight('Info', 'No index has been chosen to save.');
  }
};

// Open the replace dialog, which allows the user to enter a function
// that will replace the normal output of the index.
var replace = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    initReplaceDialog.dialog('open');
  }
  else
  {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Add a condition using the index builder dialog.
var addCond = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    initIndexBuilderDialog(bData.attr('data-index-doctype')).dialog('open');
  }
  else
  {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Handle the mouse click initiate action of removing a condition.
var remCond = function (target)
{
  'use strict';

  $(target).closest('tr').remove();
  return true;
};

// Open the new index dialog.
var newCond = function ()
{
  'use strict';

  initIndexNewDialog().dialog('open');
  return true;
};

// Delete the current index.
var del = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    var indexId = bData.attr('data-index-id');
    var indexRev = bData.attr('data-index-rev');
    var completeMessage = 'Your index has been deleted.';
    var completeFunction = function ()
    {
      $('#index-conditions').empty();
      ilistingui.init();
    };

    if (window.confirm('Are you sure?'))
    {
      deleteIndex(indexId, indexRev, completeMessage, completeFunction);
    }
  }
  else
  {
    flash.highlight('Info', 'No index has been chosen to delete.');
  }

  return true;
};

exports.init = init;
exports.save = save;
exports.replace = replace;
exports.addCond = addCond;
exports.remCond = remCond;
exports.newCond = newCond;
exports.del = del;

},{"../flash.js":31,"../form.js":32,"./builder-dialog.js":35,"./ihelpers.js":38,"./ilistingui.js":39,"./ipreviewui.js":40,"./new-dialog.js":41,"./replace-dialog.js":42}],37:[function(require,module,exports){
// # Dialog Events
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// These are change events triggered in the dialogs.

// Variable Definitions

var h = require('./ihelpers.js');

//
// Exported Functions
//

// Set change events for doctype field
var setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback)
{
  'use strict';

  indexDoctype.change(function ()
  {
    var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
    var callback2;

    if (callback)
    {
      callback2 = callback();
    }

    h.fOpts(url, indexFieldset, callback2);
  });

  return false;
};

// Set change events for index field
var setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback)
{
  'use strict';

  indexFieldset.change(function ()
  {
    var callback2;

    if (typeof indexDoctype !== 'string')
    {
      indexDoctype = indexDoctype.val();
    }

    if (indexFieldset.val())
    {
      var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.val() + '/fields?as=options';

      if (callback)
      {
        callback2 = callback();
      }

      h.fOpts(url, indexField, callback2);
    }
  });

  return true;
};

// Set change events for field
var setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback)
{
  'use strict';

  indexField.change(function ()
  {
    var fieldId = indexField.val();
    var fieldsetId = indexFieldset.val();
    var callback2;

    if (callback)
    {
      callback2 = callback();
    }

    if (!(fieldId.isBlank()))
    {
      h.getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data)
      {
        h.alterOpts(data, fieldId, callback2);
      });
    }
  });

  return true;
};

// Set change events for the operator field.
var setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback)
{
  'use strict';

  operatorField.change(function ()
  {
    var callback2;

    if (callback)
    {
      callback2 = callback();
    }

    h.alterArg(argumentField, operatorField, fieldField, callback2);
  });

  return true;
};

exports.setIndexOperatorEvents = setIndexOperatorEvents;
exports.setIndexFieldEvents = setIndexFieldEvents;
exports.setIndexFieldsetEvents = setIndexFieldsetEvents;
exports.setIndexDoctypeEvents = setIndexDoctypeEvents;

},{"./ihelpers.js":38}],38:[function(require,module,exports){
// # Index tool helpers.
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Shared functions used by a number of index tool modules.

// Variable Definitions

var s = require('../sess.js');

// Internal functions

// Disable certain options match `disables`.
var disableOptions = function (options, disables)
{
  'use strict';

  options.children().show();

  disables.forEach(function (item)
  {
    options.children('option:contains(' + item + ')').hide();
  });

  return false;
};

// Disable the operator options.
var disableOperatorOptions = function (fieldDoc)
{
  'use strict';

  var options = $('#builder-operator-input');

  switch (fieldDoc.subcategory)
  {
  case 'select':
  case 'docselect':
  case 'text':
  case 'textarea':
    disableOptions(options, ['member', 'true']);
    break;
  case 'integer':
  case 'rational':
  case 'date':
    disableOptions(options, ['member', 'true', 'match']);
    break;
  case 'boolean':
  case 'openboolean':
    disableOptions(options, ['equal', 'greater', 'less', 'member', 'match']);
    break;
  case 'multiselect':
  case 'docmultiselect':
    disableOptions(options, ['equal', 'greater', 'less', 'true', 'match']);
    break;
  }

  return false;
};

// Exported functions

// Handles an input field that presents different behavior depending on
// the values of previously filled in fields.
var alterArg = function (argumentField, operatorField, fieldField, callback)
{
  'use strict';

  var fieldDoc = function ()
  {
    return s.get(fieldField.val());
  };

  callback();

  try
  {
    // Destroy these if initialized already
    argumentField.removeAttr('disabled').datepicker('destroy');
    argumentField.removeAttr('disabled').autocomplete('destroy');
  }
  catch (err)
  {
    window.console.log(err.message);
  }

  var dateOrText = function (argumentField, fdoc)
  {
    if (fdoc.subcategory === 'date')
    {
      argumentField.removeAttr('disabled');
      argumentField.datepicker(
      {
        dateFormat: 'yy-mm-dd'
      });
    }
    else
    {
      argumentField.removeAttr('disabled');
      argumentField.autocomplete(
      {
        source: fdoc.allowed
      });
    }

    return true;
  };

  var fdoc = fieldDoc();

  if (fdoc)
  {
    switch (operatorField.val())
    {
    case 'true':
    case 'isDefined':
    case 'blank':
      argumentField.attr('disabled', 'disabled').val('');
      break;
    case 'equal':
    case 'member':
    case 'greater':
    case 'less':
    case 'hasExactly':
    case 'hasGreater':
    case 'hasLess':
      dateOrText(argumentField, fdoc);
      break;
    }

  }

  return true;
};

// Certain operator options only exist for certain types of fields.
var alterOpts = function (fieldDoc, fieldId, callback)
{
  'use strict';

  disableOperatorOptions(fieldDoc);
  callback();

  return true;
};

// Get the fields that the user may choose from.
var fOpts = function (url, selectElement, callback)
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

  return true;
};

// Get the document holding the field information.
var getFieldDoc = function (fieldId, fieldsetId, doctypeId, callback)
{
  'use strict';

  var fieldDoc = s.get(fieldId);
  var url = 'doctypes/' + doctypeId + '/fieldsets/' + fieldsetId + '/fields/' + fieldId + '?format=json';

  if (fieldDoc)
  {
    if (callback)
    {
      callback(fieldDoc);
    }
    return fieldDoc;
  }
  else
  {
    $.ajax(
    {
      url: url,
      async: false,
      dataType: 'json',
      success: function (data)
      {
        s.put(data);
        if (callback)
        {
          callback(s.get(fieldId));
        }
      }
    });

    return s.get(fieldId);
  }
};

// Return an object containing methods for working with common events.
var evs = function ()
{
  'use strict';

  var mod = {};

  mod.setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback)
  {
    indexDoctype.change(function ()
    {
      var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
      var callback2;

      if (callback)
      {
        callback2 = callback();
      }

      fOpts(url, indexFieldset, callback2);
    });

    return false;
  };

  mod.setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback)
  {
    indexFieldset.change(function ()
    {
      var callback2;

      if (typeof indexDoctype !== 'string')
      {
        indexDoctype = indexDoctype.val();
      }

      if (indexFieldset.val())
      {
        var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.val() + '/fields?as=options';

        if (callback)
        {
          callback2 = callback();
        }

        fOpts(url, indexField, callback2);
      }
    });

    return mod;
  };

  mod.setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback)
  {
    indexField.change(function ()
    {
      var fieldId = indexField.val();
      var fieldsetId = indexFieldset.val();
      var callback2;

      if (callback)
      {
        callback2 = callback();
      }

      if (!(fieldId.isBlank()))
      {
        getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data)
        {
          alterOpts(data, fieldId, callback2);
        });
      }
    });

    return mod;
  };

  mod.setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback)
  {
    operatorField.change(function ()
    {
      var callback2;

      if (callback)
      {
        callback2 = callback();
      }

      alterArg(argumentField, operatorField, fieldField, callback2);
    });

    return mod;
  };
};

exports.alterArg = alterArg;
exports.alterOpts = alterOpts;
exports.fOpts = fOpts;
exports.getFieldDoc = getFieldDoc;
exports.evs = evs;

},{"../sess.js":56}],39:[function(require,module,exports){
// # Index listing.
//
// *Implicit depends:* DOM, JQuery
//
// Displays a listing of user created indexes.

// Variable Definitions

var templates = require('templates.js');

// Exported functions

// Initialize the listing of user created indexes.
var init = function ()
{
  'use strict';

  var url = 'indexes';
  var target = $('#index-index-listing');
  var listing;

  $.getJSON(url, function (data)
  {
    listing = templates['index-listing'](data);
    target.html(listing);
  });

  return true;
};

exports.init = init;

},{"templates.js":"e8H8MT"}],40:[function(require,module,exports){
// # Paging For Index Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads sample of the user index based on user suplied values.

// Variable Definitions

var pager = require('../pager.js').pager;

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'preview';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var indexId = document.getElementById('index-editing-data').getAttribute('data-index-id');
  var url = 'indexes/' + indexId + '/preview';
  var target = document.getElementById(prefix() + '-list-view');

  var format = function (text)
  {
    var resp = JSON.parse(text);

    resp.rows = resp.rows.map(function (item)
    {
      item.display_key = item.key.map(function (k)
      {
        return k[1];
      });

      return item;
    });

    return resp;
  };

  if (indexId)
  {
    pager(
    {
      prefix: prefix(),
      format: format,
      url: url,
      target: target
    }).get();
  }

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"../pager.js":46}],41:[function(require,module,exports){
// # New dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding a new user created index.

// TODO I would rather avoid having this as a JQuery plugin.

require('../jquery-ui-input-state.js');

// Variable Definitions

var ihelpers = require('./ihelpers.js');
var ilistingui = require('./ilistingui.js');
var form = require('../form.js');
var evs = require('./ievents.js');

// Exported functions

// The dialog for adding a new index.
var initIndexNewDialog = function ()
{
  'use strict';

  var indexDoctype = $('#index-doctype-input');
  var indexFieldset = $('#index-fieldset-input').inputDisable();
  var indexField = $('#index-field-input').inputDisable();
  var indexName = $('#index-name-input');
  var indexShowDeleted = $('#index-show_deleted-input');

  var doctypeEvents = function ()
  {
    evs.setIndexDoctypeEvents(indexDoctype, indexFieldset, function ()
    {
      indexFieldset.inputDisable();
      indexField.inputDisable();

      return function ()
      {
        indexFieldset.inputEnable();
      };
    });
  };

  var fieldsetEvents = function ()
  {
    evs.setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, function ()
    {
      indexField.inputDisable();

      return function ()
      {
        indexField.inputEnable();
      };
    });
  };

  var getLabelForVal = function (val)
  {
    return $('#index-new-dialog option[value="' + val + '"]').text();
  };

  var getLabel = function ()
  {
    return [getLabelForVal(indexFieldset.val()), getLabelForVal(indexField.val())].join(':');
  };

  var dialog = $('#index-new-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Create': function ()
      {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (checkResult)
        {
          var obj = {
            'category': 'index',
            'name': indexName.val(),
            'show_deleted': indexShowDeleted.is(':checked'),
            'conditions': [],
            'doctype': indexDoctype.val(),
            'fields_label': [getLabel()],
            'fields': [indexField.val()]
          },
            complete = function (context)
            {
              ilistingui.init();
              $(context).dialog('close');
            };
          form.send('indexes', obj, 'POST', complete, this);
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      indexFieldset.unbind('change');
      indexDoctype.unbind('change');
      form.clear($('.input')).removeClass('ui-state-error');
    }
  });

  doctypeEvents();
  fieldsetEvents();

  return dialog;
};

exports.initIndexNewDialog = initIndexNewDialog;

},{"../form.js":32,"../jquery-ui-input-state.js":43,"./ievents.js":37,"./ihelpers.js":38,"./ilistingui.js":39}],42:[function(require,module,exports){
// # Replace dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for providing a function to replace the normal output of
// an index.

// Variable Definitions

var ihelpers = require('./ihelpers.js');
var form = require('../form.js');

// Exported functions

// The dialog for providing a function to replace the normal output of
// an index.
var initReplaceDialog = function ()
{
  'use strict';

  var replaceFunction = $('#index-replace_function-input');
  var indexData = $('#index-editing-data');
  var remove = $('#index-remove_function-input');

  if (indexData.attr('data-index-replace_function'))
  {
    replaceFunction.val(indexData.attr('data-index-replace_function'));
  }
  else
  {
    form.clear(replaceFunction).removeClass('ui-state-error');
  }

  var dialog = $('#index-replace-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!remove.is(':checked'))
        {
          if (replaceFunction.val().isBlank())
          {
            replaceFunction.addClass('ui-state-error');
          }
          else
          {
            replaceFunction.removeClass('ui-state-error');
          }

          if (checkResult)
          {
            indexData.attr('data-index-replace_function', replaceFunction.val());
            $('#replace-function-message').text('This index has a replacement function.');
          }
        }
        else
        {
          indexData.removeAttr('data-index-replace_function');
          $('#replace-function-message').empty();
        }

        $(this).dialog('close');
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      form.clear(replaceFunction).removeClass('ui-state-error');
    }
  });

  return dialog;
};

exports.initReplaceDialog = initReplaceDialog;

},{"../form.js":32,"./ihelpers.js":38}],43:[function(require,module,exports){
/*
 Simple plugin for manipulating input.
*/

(function ($)
{
  'use strict';

  $.fn.inputDisable = function ()
  {
    this.val('');
    this.attr('disabled', 'disabled');
    this.addClass('ui-state-disabled');
    return this;
  };

  $.fn.inputEnable = function ()
  {
    this.removeAttr('disabled');
    this.removeClass('ui-state-disabled');
    return this;
  };

})(jQuery);

},{}],44:[function(require,module,exports){
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

(function (jQuery)
{
  'use strict';

  jQuery.hotkeys = {
    version: '0.8',

    specialKeys:
    {
      8: 'backspace',
      9: 'tab',
      13: 'return',
      16: 'shift',
      17: 'ctrl',
      18: 'alt',
      19: 'pause',
      20: 'capslock',
      27: 'esc',
      32: 'space',
      33: 'pageup',
      34: 'pagedown',
      35: 'end',
      36: 'home',
      37: 'left',
      38: 'up',
      39: 'right',
      40: 'down',
      45: 'insert',
      46: 'del',
      96: '0',
      97: '1',
      98: '2',
      99: '3',
      100: '4',
      101: '5',
      102: '6',
      103: '7',
      104: '8',
      105: '9',
      106: '*',
      107: '+',
      109: '-',
      110: '.',
      111: '/',
      112: 'f1',
      113: 'f2',
      114: 'f3',
      115: 'f4',
      116: 'f5',
      117: 'f6',
      118: 'f7',
      119: 'f8',
      120: 'f9',
      121: 'f10',
      122: 'f11',
      123: 'f12',
      144: 'numlock',
      145: 'scroll',
      191: '/',
      224: 'meta'
    },

    shiftNums:
    {
      '`': '~',
      '1': '!',
      '2': '@',
      '3': '#',
      '4': '$',
      '5': '%',
      '6': '^',
      '7': '&',
      '8': '*',
      '9': '(',
      '0': ')',
      '-': '_',
      '=': '+',
      ';': ': ',
      '\'': '"',
      ',': '<',
      '.': '>',
      '/': '?',
      '\\': '|'
    }
  };

  function keyHandler(handleObj)
  {
    // Only care when a possible input has been specified
    if (typeof handleObj.data !== 'string')
    {
      return;
    }

    var origHandler = handleObj.handler,
      keys = handleObj.data.toLowerCase().split(' ');

    handleObj.handler = function (event)
    {
      // Don't fire in text-accepting inputs that we didn't directly bind to
      // MODIFIED FROM ORIGINAL
      //if ( this !== event.target && (/textarea|select/i.test( event.target.nodeName ) ||
      //      event.target.type === 'text') ) {
      //	return;
      //}
      // Keypress represents characters, not special keys
      var special = event.type !== 'keypress' && jQuery.hotkeys.specialKeys[event.which],
        character = String.fromCharCode(event.which).toLowerCase(),
        key, modif = '',
        possible = {};

      // check combinations (alt|ctrl|shift+anything)
      if (event.altKey && special !== 'alt')
      {
        modif += 'alt+';
      }

      if (event.ctrlKey && special !== 'ctrl')
      {
        modif += 'ctrl+';
      }

      // TODO: Need to make sure this works consistently across platforms
      if (event.metaKey && !event.ctrlKey && special !== 'meta')
      {
        modif += 'meta+';
      }

      if (event.shiftKey && special !== 'shift')
      {
        modif += 'shift+';
      }

      if (special)
      {
        possible[modif + special] = true;

      }
      else
      {
        possible[modif + character] = true;
        possible[modif + jQuery.hotkeys.shiftNums[character]] = true;

        // '$' can be triggered as 'Shift+4' or 'Shift+$' or just '$'
        if (modif === 'shift+')
        {
          possible[jQuery.hotkeys.shiftNums[character]] = true;
        }
      }

      for (var i = 0, l = keys.length; i < l; i++)
      {
        if (possible[keys[i]])
        {
          return origHandler.apply(this, arguments);
        }
      }
    };
  }

  jQuery.each(['keydown', 'keyup', 'keypress'], function ()
  {
    jQuery.event.special[this] = {
      add: keyHandler
    };
  });

})(jQuery);

},{}],45:[function(require,module,exports){
// # Change Event Handling
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the keystroke events. This is a start and a bit of
// an experiment. It uses the JQuery `on()` function, which was not
// available when I first began programming this application. It also
// uses the JQuery hotkeys plugin, which I'd like to remove at some point.

// ## Variable Definitions

var hotkeys = require('./jquery.hotkeys.js');
var S = require('./sender.js');
var ipreviewui = require('./index_tool/ipreviewui.js');
var indexui = require('./documents/indexui.js');
var changeui = require('./documents/changeui.js');
var editui = require('./documents/editui.js');
var viewui = require('./documents/viewui.js');
var searchui = require('./documents/searchui.js');
var charsequi = require('./config/charsequi.js');
var doctypeui = require('./config/doctypeui.js');

// # Exported Functions

// All this does is register a bunch of event handlers.
var keystrokes = function ()
{
  'use strict';

  [ipreviewui, indexui, changeui, doctypeui, charsequi].forEach(function (mod)
  {
    var keyupHandler = function (e)
    {
      var getIndexTimer;
      window.clearTimeout(getIndexTimer);
      getIndexTimer = setTimeout(function ()
      {
        if (e.which !== 8 && e.which !== 46)
        {
          mod.get();
        }
      }, 500);
    };

    document.addEventListener('keyup', function(e)
    {
      if (e.target.id === mod.prefix() + '-filter')
      {
        keyupHandler(e);
      }
      else if (e.target.id === mod.prefix() + '-limit')
      {
        keyupHandler(e);
      }
    });
  });

  $(document).on('keydown', '#document-worksheets-form', function (e)
  {
    if (e.which === 13)
    {
      S.sender('worksheet-form-submit');
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-sets-form', function (e)
  {
    if (e.which === 13)
    {
      S.sender('sets-form-submit');
      return false;
    }
    return true;
  });

  $('#new-set-form').on('keydown', function (e)
  {
    if (e.which === 13)
    {
      S.sender('new-set-form-submit');
      return false;
    }
    return true;
  });

  $(document).bind('keydown', 'Alt+n', function (e)
  {
    var t = function ()
    {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected < totaltabs - 1)
    {
      t().tabs('option', 'active', selected + 1);
      S.sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', 0);
      S.sender('lost-focus');
    }

    return false;
  });

  $(document).bind('keydown', 'Alt+c', function (e)
  {
    var active = $(document.activeElement).attr('id');
    S.sender('initiated-command', active);
    return true;
  });

  $(document).bind('keydown', 'Alt+p', function (e)
  {
    var t = function ()
    {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected !== 0)
    {
      t().tabs('option', 'active', selected - 1);
      S.sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', totaltabs - 1);
      S.sender('lost-focus');
    }

    return false;
  });


  $(document).on('keydown', '#edit-command-input', function (e)
  {
    if (e.which === 13)
    {
      var command = $('#edit-command-input').val();
      S.sender('submitted-command', command);
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form input', function (e)
  {
    if (e.which === 13)
    {
      if ($('#save-document-button').css('display') === 'none')
      {
        editui.create();
      }
      else
      {
        editui.save();
      }
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form textarea', 'Alt+x', function (e)
  {
    editui.toggleTextarea($(e.target));
    return false;
  });

  $(document).on('keypress', '#view-jump-id', function (e)
  {
    if (e.which === 13)
    {
      var docid = $('#view-jump-id').val();
      viewui.get(docid);
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-search-term', function (e)
  {
    if (e.which === 13)
    {
      searchui.getSearch();
      return false;
    }
    return true;
  });

  return true;
};

exports.keystrokes = keystrokes;

},{"./config/charsequi.js":7,"./config/doctypeui.js":12,"./documents/changeui.js":20,"./documents/editui.js":23,"./documents/indexui.js":25,"./documents/searchui.js":26,"./documents/viewui.js":28,"./index_tool/ipreviewui.js":40,"./jquery.hotkeys.js":44,"./sender.js":55}],46:[function(require,module,exports){
// # Paging List-like Info
//
// *Implicit depends:* DOM, JSON
//
// This is basically semi-generic paging code.
//
// Get the index that is displayed in the index pane.  startkey and
// startid map directly to the same concepts in couchdb view queries. The
// prevkeys and previds are used to hold information that will allow
// the user to page backward through the listing. They are arrays of
// keys and ids corresponding to previous page's startkeys and ids.
//
// There are a number of values that this function depends on that
// are taken from the HTML. These include the value for the limit and
// the nextkey and nextid for paging forward. Also the current key and
// id are taken from the html when needed to add to the prevkeys and
// previds. The startkey may be a user input value so a more reliable
// startkey and startid are needed.

// Variable Definitions

var templates = require('templates.js');
var form = require('./form.js');

// Exported functions

// Initialize the pager with an args object.
var pager = function (args)
{
  'use strict';

  var mod = {};
  // If the 'prefix' used to automatically determine certain element
  // ID's is not set, set it to 'index'.
  if (args.prefix === undefined)
  {
    args.prefix = 'index';
  }
  // Special formatting or template code.
  var format = args.format;
  var prefix = args.prefix;

  // Escape a value and base64 encode it.
  var escapeValue = function (value)
  {
    return window.btoa(window.unescape(window.encodeURIComponent(JSON.stringify(value))));
  };

  // The number of elements to display is given here. Note how `prefix`
  // is used.
  var limitField = function ()
  {
    return document.getElementById(prefix + '-limit');
  };

  // Get the first or next page. There won't be `prevkeys` or `previds`
  // if it is the first page. These accumulate during paging so that it
  // is possible to go backwards.
  mod.get = function (startkey, startid, prevkeys, previds)
  {
    // The URL given as one of the original args.
    var url = args.url + '?';
    // This would be a custom index ID.
    var indexId = args.indexId;
    // The given limit.
    var limit = limitField().value * 1;
    // Where the next page will be displayed.
    var target = args.target;
    // The filter is used to constrain the values listed.
    var filterVal = document.getElementById(prefix + '-filter').value;
    var state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!state.pks)
    {
      state.sk = escapeValue(filterVal);
      state.pks = [];
      state.pids = [];
    }

    if (state.sk)
    {
      url = url + 'startkey=' + window.escape(window.atob(state.sk));
      if (state.sid)
      {
        url = url + '&startkey_docid=' + state.sid;
      }
    }

    if (limit)
    {
      url = url + '&limit=' + (limit + 1);
    }
    else
    {
      limitField().value = 25;
      url = url + '&limit=26';
    }

    if (indexId)
    {
      url = url + '&index=' + indexId;
    }

    form.send(url, false, 'GET', function (context, req)
    {
      mod.fill(req, state, target);
    }, this);

    return mod;
  };

  mod.fill = function (req, state, target)
  {
    var limit = limitField().value * 1;
    var respJSON;
    var lastrow;
    var newRows;

    var prevElem = function ()
    {
      return document.getElementById('previous-' + prefix + '-page');
    };

    var nextElem = function ()
    {
      return document.getElementById('next-' + prefix + '-page');
    };

    var prevHandler = function ()
    {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    };

    var nextHandler = function ()
    {
      var firstElem = document.getElementById('first-' + prefix + '-element');
      var nextkey = nextElem().getAttribute('data-startkey');
      var nextid = nextElem().getAttribute('data-startid');
      var prevkey = firstElem.getAttribute('data-first-key');
      var previd = firstElem.getAttribute('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    };

    if (format === undefined)
    {
      respJSON = JSON.parse(req.responseText);
    }
    else
    {
      respJSON = format(req.responseText);
    }

    newRows = respJSON.rows.map(function (item, index, thisArray)
    {
      item.encoded_key = escapeValue(item.key);
      return item;
    });

    lastrow = newRows.slice(-1);

    if (newRows[0])
    {
      newRows[0].firstrow = true;
    }

    if (newRows.length > limit)
    {
      respJSON.rows = newRows.slice(0, -1);
    }
    else
    {
      respJSON.rows = newRows;
      respJSON.lastpage = true;
    }

    respJSON.lastrow = lastrow;
    respJSON.prefix = prefix;

    target.innerHTML = templates['paged-listing'](respJSON,
    {
      'listed-element': templates.templates[prefix + '-element']
    });

    nextElem().onclick = nextHandler;
    prevElem().onclick = prevHandler;

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0)
    {
      prevElem().classList.add('hidden');
    }

    // Disable the next button if we're at the end
    if (nextElem().getAttribute('data-last-page'))
    {
      nextElem().classList.add('hidden');
    }

    return mod;
  };

  return mod;
};

exports.pager = pager;

},{"./form.js":32,"templates.js":"e8H8MT"}],47:[function(require,module,exports){
// # Panel Toggler
//
// Interface elements called panels can be visible or hidden.

// Given an element that points to a panel id with a `data-panel`
// attribute, toggle the panel's visibility.
var panelToggler = function (target)
{
  'use strict';

  var panel;

  if ($(target).attr('data-panel'))
  {
    panel = $('#' + $(target).attr('data-panel'));
  }
  else
  {
    panel = $(target).closest('.panel');
  }

  if (panel.css('display') === 'none')
  {
    panel.css('display', 'table-cell');
  }
  else
  {
    panel.css('display', 'none');
  }

  return target;
};

exports.panelToggler = panelToggler;

},{}],48:[function(require,module,exports){
// # Path helper
//
// *Implicit depends:* DOM, JQuery
//
// This function returns an object with various helpers for URL
// path operations. In this application a common pattern in paths is
// `doctypes/<doctypeid>/fieldsets/<fiedsetid>/fields/<fieldid>`. The
// path function below will take a source, which is a jQuery object,
// such as `$('#some-id')`, which has an attribute named `data-group-id`
// having a value of the id of an element that stores data relevant to
// the current context as HTML data attributes, in particular the ids of
// doctypes, fieldsets and/or fields. The category is one of 'field',
// 'fieldset' or 'doctype'. The section argument is a section of the
// application, such as 'config' that will be prefixed to the path.
//
// #### Example HTML:
//
//     <div
//       id='someid'
//       data-fieldset-fieldset='fsid'
//       data-fieldset-doctype='did'></div>
//
//     <div
//      id='thisid'
//      data-group-id='someid'>
//
// #### Example usage:
//
//     mypath = path($('#thisid'), 'fieldset');
//     mypath.toString() == 'doctypes/did/fieldsets/fsid';
//
//     mypath = path($('#thisid'), 'fieldset', 'config');
//     mypath.toString() == 'config/doctypes/did/fieldsets/fsid';
//
//     mypath = path($('#thisid'), 'fieldset');
//     mypath.fieldset = false; // unsets the fielset id
//     mypath.toString() == 'doctypes/did/fieldsets'; // all fieldsets
//
// Note that the category matches the x of `data-x` in `someid`. Different
// values may be held for doctype or field in the same element. Sometimes
// this leads to repetition of information and a better scheme may be
// forthcoming. The positive side is that information about different
// paths may be held in the same location.
//
// ### CouchDB Revision Numbers
//
// Above, a revision could have been added to someid as `data-fieldset-rev`.
//
// #### More Information
//
// For more information on how data attributes are used in this application,
// see [store.js](./store.html).
//
// ## Manipulating the object
//
// Also note that setting certain path elements to false (or undefined)
// will exclude their ids from the end result. Setting the element to a
// different id would cause the path to be altered appropriately. This
// allows one to cleanly manipulate the paths without performing string
// manipulation.
//
// ## PUT, POST and DELETE using the object
//
// There are also helpers for using the path the work with the resource
// it points to.
//
// #### Example:
//
//     mypath = path($('#thisid'), 'fieldset');
//     mypath.put(object, callback, context);
//     mypath.post(object, callback, context);
//     mypath.del(callback, context);
//
// Object is an Javascript object that can be encoded as JSON, callback
// will be run on success and context provides information the environment
// from which the method was called, usually `this` is supplied.
//
// The object will be sent to the path that would be returned by the
// toString method using the method implied by the above method's names.
//
// ### Error handlers
//
// Within the context of this application it is assumed that fairly
// standard things will be done with error responces so they are left
// alone.

// Variable Definitions

var store = require('./store.js').store;
var form = require('./form.js');

// Exported functions

// Object initialization
var path = function (source, category, section)
{
  'use strict';

  var mod = {};
  var prefix;

  if (category)
  {
    prefix = category + '-';
  }
  else
  {
    prefix = '';
  }

  if (section)
  {
    mod.string = section + '/';
  }
  else
  {
    mod.string = '';
  }

  mod.category = category;
  mod.origin = source;
  mod.type = prefix + 'path';
  mod.valid_components = ['doctype', 'fieldset', 'field'];
  var s = store(mod.origin);

  mod.valid_components.forEach(function (item)
  {
    mod[item] = (function ()
    {
      var value = s.get(prefix + item);
      return value;
    })();
  });

  mod.rev = s.get(prefix + 'rev');

  mod.doctype = s.get(prefix + 'doctype');

  mod.send = function (object, method, callback, context)
  {
    form.send(mod.toString(), object, method, callback, context);
    return mod;
  };

  mod.put = function (object, callback, context)
  {
    mod.send(object, 'PUT', callback, context);
    return mod;
  };

  mod.post = function (object, callback, context)
  {
    mod.send(object, 'POST', callback, context);
    return mod;
  };

  mod.del = function (callback, context)
  {
    mod.send(
    {}, 'DELETE', callback, context);
    return mod;
  };

  mod.toString = function ()
  {
    var rev;

    var pathString =
      mod.string.concat(
      mod.valid_components.map(

    function (item)
    {
      var plural = item + 's';
      var value = mod[item];
      var retval = null;

      if (value)
      {
        retval = plural + '/' + value;
      }
      else if (item === mod.category)
      {
        retval = plural;
      }

      return retval;
    }).filter(

    function (item)
    {
      return (typeof item === 'string' && !item.isBlank());
    }).join('/'));

    if (mod.rev)
    {
      pathString = pathString.concat('?rev=' + mod.rev);
    }

    return pathString;
  };

  return mod;
};

exports.path = path;

},{"./form.js":32,"./store.js":57}],49:[function(require,module,exports){
// # The project manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with projects.

// Variable Definitions

var form = require('../form.js');
var init;

// Internal functions

// Delete the project with the given ID.
var deleteProject = function (id)
{
  'use strict';

  if (window.confirm('Are you sure? This is permanent.'))
  {
    $.ajax(
    {
      type: 'DELETE',
      url: '/projects/' + id,
      dataType: 'json',
      contentType: 'application/json',
      complete: function (req, status)
      {
        if (req.status === 204)
        {
          init();
        }
        else
        {
          window.alert('An error occurred' + req.status);
        }
      }
    });
  }
};

// Exported functions

// Add a project.
var add = function ()
{
  'use strict';

  var projectName = $('#project-name');
  var projectDescription = $('#project-description');
  var tips = $('.validate-tips');
  var allFields = $([]).add(projectName).add(projectDescription);

  var dialog = $('#add-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Add project': function ()
      {
        allFields.removeClass('ui-state-error');
        $('.validation-error-message').remove();

        var checkResult = form.checkLength(projectName, 'project name', 1, 50, tips);

        if (checkResult)
        {
          $.ajax(
          {
            type: 'POST',
            url: 'projects/index',
            dataType: 'json',
            contentType: 'application/json',
            processData: false,
            data: JSON.stringify(
            {
              name: projectName.val(),
              description: projectDescription.val()
            }),
            complete: function (req, status)
            {
              if (req.status === 201)
              {
                init();
              }
              else
              {
                window.alert('An error occurred ' + req.status);
              }
            }
          });
          $(this).dialog('close');
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      allFields.val('').removeClass('ui-state-error');
    }
  });

  return dialog;
};

// Add a project.
var del = function (target)
{
  'use strict';

  var id = $(target).attr('id');
  deleteProject(id);

  return true;
};

// Initialize the interface.
init = function ()
{
  'use strict';

  var url = '/projects/index';

  $.get(url, function (projects)
  {
    $('tbody').empty();
    $('tbody').html(projects);
  });
};

exports.add = add;
exports.del = del;
exports.init = init;

},{"../form.js":32}],50:[function(require,module,exports){
// # Set operations
//
// The 'set' is a one dimensional Array by default but by replacing the
// `member` function, other types of Arrays may be used.

// Exported functions

// Determine membership of item in the set.
var member = function (arr, x)
{
  'use strict';

  var memb = arr.some(function (y)
  {
    return x === y;
  });
  return memb;
};

// Rebuild the array so that all values are unique. This is kind of a
// 'clean up' function used to work around the differences between arrays
// and sets.
var unique = function (x, mem)
{
  'use strict';

  if (!mem)
  {
    mem = member;
  }
  var uniq = x.reduce(function (acc, curr)
  {
    if (mem(acc, curr))
    {
      return acc;
    }
    else
    {
      return acc.concat([curr]);
    }
  }, []);
  return uniq;
};

// Return the union of two sets.
var union = function (xs, ys, mem)
{
  'use strict';

  if (!mem)
  {
    mem = member;
  }
  var uni = unique(xs.concat(ys), mem);
  return uni;
};

// Return the intersection of two sets.
var intersection = function (xs, ys, mem)
{
  'use strict';

  if (!mem)
  {
    mem = member;
  }
  var inter = xs.filter(function (x)
  {
    return mem(ys, x);
  });
  return inter;
};

// Return the relative complement of two sets.
var relativeComplement = function (xs, ys, mem)
{
  'use strict';

  if (!mem)
  {
    mem = member;
  }
  var comp = xs.filter(function (x)
  {
    return !mem(ys, x);
  });
  return comp;
};

// Return the symmetric difference of two sets.
var symmetricDifference = function (xs, ys, mem)
{
  'use strict';

  if (!mem)
  {
    mem = member;
  }
  var comp1 = relativeComplement(xs, ys, mem);
  var comp2 = relativeComplement(ys, xs, mem);
  var uni = union(comp1, comp2, mem);
  return uni;
};

exports.member = member;
exports.unique = unique;
exports.union = union;
exports.intersection = intersection;
exports.relativeComplement = relativeComplement;
exports.symmetricDifference = symmetricDifference;

},{}],"hogan.js":[function(require,module,exports){
module.exports=require('nLm5Ax');
},{}],"Bacon.js":[function(require,module,exports){
module.exports=require('EMvo/m');
},{}],53:[function(require,module,exports){
// # Recursion
//
// Tail call optimization taken from Spencer Tipping's Javascript in Ten
// Minutes.
//
// For more information see:
// <https://github.com/spencertipping/js-in-ten-minutes>

// ## Exported Functions

// Identity function
var identity = function (x)
{
  'use strict';

  return x;
};

// Adds the prototype functions
(function ()
{
  'use strict';

  // Return the values to apply
  Function.prototype.r = function ()
  {
    return [this, arguments];
  };

  // Tail call function
  Function.prototype.t = function ()
  {
    var c = [this, arguments];
    var escape = arguments[arguments.length - 1];
    while (c[0] !== escape)
    {
      c = c[0].apply(this, c[1]);
    }
    return escape.apply(this, c[1]);
  };

  return true;
})();

exports.identity = identity;

},{}],"templates.js":[function(require,module,exports){
module.exports=require('e8H8MT');
},{}],55:[function(require,module,exports){
// # Take actions depending on reported state.
//
// This is essentially and experiment in attempting to perform actions
// based on the state of the application. It is an idea that I'm still
// working on but the idea is to avoid having functions directly call
// other functions to initiate new actions but to instead simply report
// their state and have some central authority decide what to do next.

// Variable Definitions

var commands = require('./documents/commands.js');
var documents = require('./documents/documents.js');
var editui = require('./documents/editui.js');
var searchui = require('./documents/searchui.js');
var setsui = require('./documents/setsui.js');
var worksheetui = require('./documents/worksheetui.js');

// Exported functions

// This is called by functions when the actions they have performed
// result in a paticular state.
var sender = function (message, arg)
{
  'use strict';

  var retval;

  switch (message)
  {
  case 'bad-session-state':
    retval = documents.clearSession();
    break;
  case 'doctype-info-ready':
    retval = documents.makeLabels();
    break;
  case 'labels-ready':
    retval = searchui.loadSearchVals();
    worksheetui.buildTemplate();
    break;
  case 'new-set-form-submit':
    retval = setsui.saveSelected();
    break;
  case 'sets-changed':
    retval = setsui.updateSelection();
    break;
  case 'sets-form-submit':
    retval = setsui.performOp();
    break;
  case 'session-cleared':
    documents.setVersion();
    retval = documents.loadDoctype();
    break;
  case 'worksheet-form-submit':
    retval = worksheetui.fillWorksheet();
    break;
  case 'initiated-command':
    retval = commands.dialogOpen(arg);
    break;
  case 'executed-command':
    retval = commands.dialogClose();
    break;
  case 'submitted-command':
    retval = commands.execute(arg);
    break;
  case 'lost-focus':
    retval = editui.selectInput();
    break;
  }

  return retval;
};

exports.sender = sender;

},{"./documents/commands.js":21,"./documents/documents.js":22,"./documents/editui.js":23,"./documents/searchui.js":26,"./documents/setsui.js":27,"./documents/worksheetui.js":29}],56:[function(require,module,exports){
// # Session storage helpers
//
// *Implicit depends:* DOM
//
// This is primarily used to store and retrieve items with a structure
// similar to a CouchDB document.

// Exported functions

// If the item is not already in the session storage, convert it to JSON
// and store it by `_id`. Return the `_id` of the document.
var put = function (doc)
{
  'use strict';

  if (!window.sessionStorage[doc._id])
  {
    window.sessionStorage[doc._id] = JSON.stringify(doc);
  }

  return doc._id;
};

// Retrieve the document, which is stored as JSON, by its `_id` and
// return the parsed item. If the item does not exist, return `null`.
var get = function (docId)
{
  'use strict';

  var doc = window.sessionStorage[docId];

  if (doc)
  {
    return JSON.parse(doc);
  }
  else
  {
    return null;
  }
};

exports.put = put;
exports.get = get;

},{}],57:[function(require,module,exports){
// # Data Attribute Storage and Retrieval Helpers
//
// *Implicit depends:* DOM
//
// It is likely that this mechanism will be replaced with a superior
// mechanism for storing data on the client about documents.

// ## Variables

var utils = require('./utils.js');
var r = require('./recurse.js');

// ## Internal functions

// Camel case a string
var cc = function (str)
{
  'use strict';

  return str.replace(/-./, function (substr)
  {
    return substr.toUpperCase()[1];
  });
};

// ## External functions

// Takes a JQuery element and returns an object with helper methods for
// getting and putting custom data attribute values.
var store = function (elem)
{
  'use strict';

  var mod = {};

  // TODO: Remove this when fieldsets.js has JQuery dependency removed
  if (elem.dataset === undefined)
  {
    elem = elem[0];
  }

  // This funtion takes a key that corresponds to the name of the data
  // attribute without the `data-` prefix. The element is expected to have
  // an attribute data-group-id with a value that is the id of the
  // element actually holding the data.
  //
  // ### Examples
  //
  // Given the following HTML:
  //
  //     <div
  //       id='someid'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  // This HTML contains a level of indirection and demonstrates the use
  // of the `data-group-id`:
  //
  //     <div
  //       id='someid2'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //       id='someid'
  //       data-group-id='someid2'
  //       data-fieldset-fieldset='fsid'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'></div>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  mod.get = function (key)
  {
    var keycc = cc(key);
    var prelim = elem.dataset[keycc];

    if (prelim)
    {
      return prelim;
    }

    var getValue1 = function (key, elem, id)
    {
      var gid = elem.dataset.groupId;
      var store = document.getElementById(gid);
      var val = store.dataset[key];
      var next = store.dataset.groupId;

      if (val === undefined && next !== undefined && gid !== next)
      {
        return getValue1.r(key, store, id);
      }

      return id.r(val);
    };

    return getValue1.t(keycc, elem, r.identity);
  };

  // Like 'get' but will decode base64 encoded values.
  mod.get64 = function (key)
  {
    var retval = mod.get(key);
    retval = utils.Base64.decode(retval.replace(/'/g, '')).replace(/(^'|'$)/g, '');
    return retval;
  };

  //  This function will set an attribute at the target with a name
  //  corresponding to key and a value of value.
  mod.put = function (key, value)
  {
    var keycc = cc(key);
    var dataElem = elem.dataset.groupId;
    document.getElementById(dataElem).dataset[keycc] = value;
  };

  //  Helper function for attributes that begin with `data-fieldset`.
  mod.fs = function (key)
  {
    return mod.get('fieldset-' + key);
  };

  //  Helper function for attributes that begin with `data-field`.
  mod.f = function (key)
  {
    return mod.get('field-' + key);
  };

  //  Helper function for attributes that begin with `data-document`.
  mod.d = function (key)
  {
    return mod.get('document-' + key);
  };

  return mod;
};

exports.store = store;

},{"./recurse.js":53,"./utils.js":58}],58:[function(require,module,exports){
// # Misc

// Exported functions

// safer(ish) string to number. The difference is that in this app
// I am using '' if the string isn't a valid number.
var stringToNumber = function (string)
{
  'use strict';

  if (typeof string === 'string' && !isNaN(string) && string !== '')
  {
    return string * 1;
  }
  else
  {
    return '';
  }
};

// A predicate function to detect blankness of various object types.
var isBlank = function (value)
{
  'use strict';

  return (((/^\s*$/).test(value)) || (value === null) || (value === undefined) || (typeof value === 'number' && isNaN(value)) || (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
};

// A predicate to test if the input is a string containing 32 characters
// limited to hexidecimal digits.
var validID = function (id)
{
  'use strict';

  return !!id.match(/^[a-f0-9]{32}$/);
};

// Base64 encode / decode
// Taken from <http://www.webtoolkit.info/>
var Base64 = {
  // private property
  _keyStr: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',

  // public method for encoding
  encode: function (input)
  {
    'use strict';

    var output = '';
    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
    var i = 0;

    input = Base64._utf8_encode(input);

    while (i < input.length)
    {

      chr1 = input.charCodeAt(i++);
      chr2 = input.charCodeAt(i++);
      chr3 = input.charCodeAt(i++);

      enc1 = chr1 >> 2;
      enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
      enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
      enc4 = chr3 & 63;

      if (isNaN(chr2))
      {
        enc3 = enc4 = 64;
      }
      else if (isNaN(chr3))
      {
        enc4 = 64;
      }

      output = output + this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) + this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);

    }

    return output;
  },

  // public method for decoding
  decode: function (input)
  {
    'use strict';

    var output = '';
    var chr1, chr2, chr3;
    var enc1, enc2, enc3, enc4;
    var i = 0;

    input = input.replace(/[^A-Za-z0-9\+\/\=]/g, '');

    while (i < input.length)
    {

      enc1 = this._keyStr.indexOf(input.charAt(i++));
      enc2 = this._keyStr.indexOf(input.charAt(i++));
      enc3 = this._keyStr.indexOf(input.charAt(i++));
      enc4 = this._keyStr.indexOf(input.charAt(i++));

      chr1 = (enc1 << 2) | (enc2 >> 4);
      chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
      chr3 = ((enc3 & 3) << 6) | enc4;

      output = output + String.fromCharCode(chr1);

      if (enc3 !== 64)
      {
        output = output + String.fromCharCode(chr2);
      }
      if (enc4 !== 64)
      {
        output = output + String.fromCharCode(chr3);
      }

    }

    output = Base64._utf8_decode(output);

    return output;

  },

  // private method for UTF-8 encoding
  _utf8_encode: function (string)
  {
    'use strict';

    string = string.replace(/\r\n/g, '\n');
    var utftext = '';

    for (var n = 0; n < string.length; n++)
    {

      var c = string.charCodeAt(n);

      if (c < 128)
      {
        utftext += String.fromCharCode(c);
      }
      else if ((c > 127) && (c < 2048))
      {
        utftext += String.fromCharCode((c >> 6) | 192);
        utftext += String.fromCharCode((c & 63) | 128);
      }
      else
      {
        utftext += String.fromCharCode((c >> 12) | 224);
        utftext += String.fromCharCode(((c >> 6) & 63) | 128);
        utftext += String.fromCharCode((c & 63) | 128);
      }

    }

    return utftext;
  },

  // private method for UTF-8 decoding
  _utf8_decode: function (utftext)
  {
    'use strict';

    var string = '';
    var i = 0;
    var c = 0;
    var c1 = 0;
    var c2 = 0;
    var c3 = 0;

    while (i < utftext.length)
    {

      c = utftext.charCodeAt(i);

      if (c < 128)
      {
        string += String.fromCharCode(c);
        i++;
      }
      else if ((c > 191) && (c < 224))
      {
        c2 = utftext.charCodeAt(i + 1);
        string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
        i += 2;
      }
      else
      {
        c2 = utftext.charCodeAt(i + 1);
        c3 = utftext.charCodeAt(i + 2);
        string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
        i += 3;
      }

    }

    return string;
  }

};

exports.stringToNumber = stringToNumber;
exports.isBlank = isBlank;
exports.validID = validID;
exports.Base64 = Base64;

},{}],"e8H8MT":[function(require,module,exports){
var Hogan = require('hogan.js');
var t = {
  'changelog-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doc",c,p,1),c,p,0,8,777,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,73,188,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a");_.b("\n" + i);_.b("      href=\"#");_.b(_.v(_.f("document_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"view-document-link\">");_.b("\n" + i);_.b("      ");if(_.s(_.f("head_values",c,p,1),c,p,0,298,305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.d(".",c,p,0)));});c.pop();}_.b("\n" + i);_.b("    </a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("change_type",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("user",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("timestamp",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n" + i);if(_.s(_.f("changes",c,p,1),c,p,0,459,764,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <tr class=\"change-change\">");_.b("\n" + i);_.b("    <th>");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("fieldsetLabel",c,p,0)));_.b(": ");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("\n" + i);_.b("    </th>");_.b("\n" + i);_.b("    <td colspan=3>");_.b("\n" + i);if(!_.s(_.f("originalValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\n" + i);_.b("      →");_.b("\n" + i);if(!_.s(_.f("newValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("newValue",c,p,0)));_.b("\n" + i);_.b("    </td>");_.b("\n" + i);_.b("  </tr>");_.b("\n");});c.pop();}});c.pop();}return _.fl();;}),
  'charseqs-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'config-maintenance' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div id=\"maintenance\">");_.b("\n" + i);_.b("  <h3>Upgrade Project</h3>");_.b("\n" + i);_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Clicking the button below will initiate an upgrade of the project");_.b("\n" + i);_.b("    core design document to the latest version available on your");_.b("\n" + i);_.b("    system.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Be aware that this may cause significant slowness on your system");_.b("\n" + i);_.b("    while view indexes are rebuilt.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("\n" + i);_.b("  <a id=\"maintenance-upgrade-button\" class=\"maintenance-upgrade-button link-button\">Upgrade</a>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'doctypes-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-view-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li ");_.b("\n" + i);_.b("  class=\"field-view ");_.b("\n" + i);_.b("    ");if(_.s(_.f("changed",c,p,1),c,p,0,42,49,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("  data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("instance",c,p,1),c,p,0,108,150,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b("  data-field-value=\"");_.b(_.v(_.f("json_value",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("changed",c,p,1),c,p,0,235,343,"{{ }}")){_.rs(c,p,function(c,p,_){if(!_.s(_.f("newfield",c,p,1),c,p,1,0,0,"")){_.b("<span class=\"small-control view-field-change\" title=\"");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\">→</span>");};});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("is_textarea",c,p,1),c,p,0,375,426,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <span class=\"retain-white\">");_.b(_.v(_.f("value",c,p,0)));_.b("</span>");_.b("\n");});c.pop();}if(!_.s(_.f("is_textarea",c,p,1),c,p,1,0,0,"")){_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n");};_.b("</li>");_.b("\n");return _.fl();;}),
  'document-view-tree' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("previous_revision",c,p,1),c,p,0,22,76,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"revision-message\">Previous Revision</div>");_.b("\n");});c.pop();}_.b("\n" + i);if(_.s(_.f("deleted_",c,p,1),c,p,0,113,163,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"deleted-message\"><b>Deleted</b></div>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<ul>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,197,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <li");_.b("\n" + i);_.b("    class=\"fieldset-view");_.b("\n" + i);_.b("      ");if(_.s(_.f("collapse",c,p,1),c,p,0,248,257,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapsed");});c.pop();}_.b("\n" + i);_.b("      ");if(_.s(_.f("altered",c,p,1),c,p,0,289,296,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("    data-fieldset-fieldset=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("addition",c,p,1),c,p,0,414,468,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset added\" class=\"addition\">+</span>");});c.pop();}if(_.s(_.f("removal",c,p,1),c,p,0,493,548,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset removed\" class=\"removal\">−</span>");});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,579,818,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <ol>");_.b("\n" + i);if(_.s(_.f("multifields",c,p,1),c,p,0,613,785,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <ul class=\"multifield\">");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,684,737,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"              "));});c.pop();}_.b("          </ul>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("      </ol>");_.b("\n");});c.pop();}if(!_.s(_.f("multiple",c,p,1),c,p,1,0,0,"")){_.b("      <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,880,925,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"          "));});c.pop();}_.b("      </ul>");_.b("\n");};_.b("  </li>");_.b("\n");});c.pop();}_.b("</ul>");_.b("\n" + i);_.b("\n" + i);_.b("<div class=\"timestamps\">");_.b("\n" + i);_.b("  <dl>");_.b("\n" + i);_.b("    <dt>Created At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("created_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Created By</dt><dd>");_.b(_.v(_.f("created_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("updated_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated By</dt><dd>");_.b(_.v(_.f("updated_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>ID</dt><dd>");_.b(_.v(_.f("_id",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("  </dl>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'document-view' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doctype_info",c,p,1),c,p,0,17,197,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <h2 class=\"header\">");_.b(_.v(_.f("_id",c,p,0)));_.b(" View</h2>");_.b("\n" + i);_.b("\n" + i);_.b("  <form id=\"view-jump\">");_.b("\n" + i);_.b("    <label for=\"view-jump-id\">Id</label>");_.b("\n" + i);_.b("    <input type=\"text\" id=\"view-jump-id\" name=\"view-jump-id\">");_.b("\n" + i);_.b("  </form>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<div id=\"document-view-info\"");_.b("\n" + i);_.b("     data-document-deleted=\"");_.b(_.v(_.f("deleted_",c,p,0)));_.b("\"");_.b("\n" + i);_.b("     data-document-document=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("     data-document-rev=\"");_.b(_.v(_.f("_rev",c,p,0)));_.b("\"></div>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-restore-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button hidden\">Restore</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-edit-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Edit</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-delete-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Delete</a>");_.b("\n" + i);_.b("\n" + i);_.b("<nav id=\"history\">");_.b("\n" + i);if(_.s(_.f("revs_info",c,p,1),c,p,0,726,1001,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("status",c,p,1),c,p,0,742,985,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("         class=\"revision-link\"");_.b("\n" + i);_.b("         data-group-id=\"document-view-info\"");_.b("\n" + i);if(_.s(_.f("first",c,p,1),c,p,0,864,910,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("         id=\"current-revision-link\"");_.b("\n");});c.pop();}_.b("         data-document-oldrev=\"");_.b(_.v(_.f("rev",c,p,0)));_.b("\">");_.b(_.v(_.f("count",c,p,0)));_.b("</a>");_.b("\n");});c.pop();}});c.pop();}_.b("</nav>");_.b("\n" + i);_.b("\n" + i);_.b("<div id=\"document-view-tree\">");_.b("\n" + i);_.b(_.rp("document-view-tree",c,p,"  "));_.b("</div>");_.b("\n");return _.fl();;}),
  'index-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,362,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("            class=\"view-document-link\">");_.b(_.v(_.d(".",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,455,487,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'index-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table>");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <th>Name</th>");_.b("\n" + i);_.b("    <th>Doctype</th>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,91,210,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <th><a href=\"#\" data-index-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</a></th> ");_.b("\n" + i);_.b("      <td>");_.b(_.v(_.d("key.0",c,p,0)));_.b("</td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");return _.fl();;}),
  'index-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,27,74,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'paged-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<nav class=\"pager\">");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\" ");_.b("\n" + i);_.b("  title=\"Previous Page\"");_.b("\n" + i);_.b("  id=\"previous-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b(">Prev</a> ");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\"");_.b("\n" + i);_.b("  title=\"Next Page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b("  id=\"next-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);if(_.s(_.f("lastpage",c,p,1),c,p,0,322,351,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-last-page=\"true\"");_.b("\n");});c.pop();}if(_.s(_.f("lastrow",c,p,1),c,p,0,379,448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-startkey=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-startid=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b(">Next</a>");_.b("\n" + i);_.b("</nav>");_.b("\n" + i);_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,567,595,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("listed-element",c,p,"    "));});c.pop();}_.b("</table>");_.b("\n");return _.fl();;}),
  'preview-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,279,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'search-field-item' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;}),
  'set-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: <span id=\"total-set-rows\">");_.b(_.v(_.f("total",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<div id=\"save-set-results\">");_.b("\n" + i);_.b("  <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table id=\"set-elements\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" id=\"select-all-set-elements\" title=\"Click to select or deselect all elements\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>");_.b("\n" + i);_.b("        Elements");_.b("\n" + i);_.b("      </th>");_.b("\n" + i);_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("elements",c,p,1),c,p,0,435,674,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" class=\"set-element-selection\" title=\"Click to select element\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <a class=\"view-document-link\" href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.f("context",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;}),
  'set-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("names",c,p,1),c,p,0,28,66,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.d(".",c,p,0)));_.b("\">");_.b(_.v(_.d(".",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'simple-to-form-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li>");_.b("\n" + i);_.b("  <label for=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("key",c,p,0)));_.b("</label>");_.b("\n" + i);if(!_.s(_.f("text",c,p,1),c,p,1,0,0,"")){_.b("  <input type=\"");if(_.s(_.f("string",c,p,1),c,p,0,86,90,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("text");});c.pop();}if(_.s(_.f("number",c,p,1),c,p,0,112,118,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("number");});c.pop();}_.b("\" name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\" value=\"");_.b(_.v(_.f("val",c,p,0)));_.b("\"/>");_.b("\n");};if(_.s(_.f("text",c,p,1),c,p,0,191,246,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <textarea name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("val",c,p,0)));_.b("</textarea>");_.b("\n");});c.pop();}_.b("</li>");_.b("\n");return _.fl();;}),
  'simple-to-form' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<form>");_.b("\n" + i);_.b("  <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,29,67,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"      "));});c.pop();}_.b("  </ul>");_.b("\n" + i);_.b("</form>");_.b("\n");return _.fl();;}),
  'worksheet' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table id=\"worksheet-table\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr class=\"header-row\">");_.b("\n" + i);_.b("      <td id=\"select-all-worksheet-rows-cell\"");_.b("\n" + i);_.b("        class=\"select-column\">");_.b("\n" + i);_.b("        <input ");_.b("\n" + i);_.b("          id=\"select-all-worksheet-rows\"");_.b("\n" + i);_.b("          type=\"checkbox\"");_.b("\n" + i);_.b("          title=\"Click to select all rows\">");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,303,1494,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <th ");_.b("\n" + i);_.b("          class=\"worksheet-handle-header fieldset handle-column ");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <div>");_.b("\n" + i);_.b("            <span>");_.b("\n" + i);_.b("              <a class=\"fieldset-handle\" ");_.b("\n" + i);_.b("                data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("        </th>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,634,1476,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <th");_.b("\n" + i);_.b("            class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,681,689,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,716,724,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" worksheet-handle-header field handle-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <div>");_.b("\n" + i);_.b("              <span>");_.b("\n" + i);_.b("                <a class=\"field-handle\" ");_.b("\n" + i);_.b("                  data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                  href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("          </th>");_.b("\n" + i);_.b("          <th");_.b("\n" + i);_.b("            class=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <a class=\"field-header\" ");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("            <input ");_.b("\n" + i);_.b("              class=\"select-worksheet-column\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              type=\"checkbox\"");_.b("\n" + i);_.b("              title=\"Click to select column\">");_.b("\n" + i);_.b("          </td>");_.b("\n");});c.pop();}});c.pop();}_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);_.b("    <%#rows%>");_.b("\n" + i);_.b("      <tr id=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("        class=\"body-row\">");_.b("\n" + i);_.b("        <td class=\"select-column\">");_.b("\n" + i);_.b("          <input ");_.b("\n" + i);_.b("            class=\"select-worksheet-row\"");_.b("\n" + i);_.b("            data-row=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("            type=\"checkbox\"");_.b("\n" + i);_.b("            title=\"Click to select row\">");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,1865,2890,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <td class=\"");_.b(_.v(_.f("_id",c,p,0)));_.b(" fieldset handle-column\"></td>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1948,2870,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("            <td class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,1985,1993,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2020,2028,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" field handle-column\"></td>");_.b("\n" + i);_.b("            <td");_.b("\n" + i);_.b("              class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,2144,2152,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2179,2187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("              data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              <%#");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("                <%#multiple%>");_.b("\n" + i);_.b("                <ol>");_.b("\n" + i);_.b("                  <%#items%>");_.b("\n" + i);_.b("                    <li");_.b("\n" + i);_.b("                      data-field-fieldset_instance=\"<% fieldset_instance %>\"");_.b("\n" + i);_.b("                      data-field-field_instance=\"<% field_instance %>\"><% value %></li>");_.b("\n" + i);_.b("                  <%/items%>");_.b("\n" + i);_.b("                </ol>");_.b("\n" + i);_.b("                <%/multiple%>");_.b("\n" + i);_.b("                <%#single%>");_.b("\n" + i);_.b("                  <span><% value %></span>");_.b("\n" + i);_.b("                <%/single%>");_.b("\n" + i);_.b("              <%/");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("            </td>");_.b("\n");});c.pop();}});c.pop();}_.b("      </tr>");_.b("\n" + i);_.b("    <%/rows%>");_.b("\n" + i);_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;})
},
r = function(n) {
  var tn = t[n];
  return function(c, p, i) {
    return tn.render(c, p || t, i);
  }
};
module.exports = {
  templates : t,
  'changelog-element' : r('changelog-element'),
  'charseqs-element' : r('charseqs-element'),
  'config-maintenance' : r('config-maintenance'),
  'doctypes-element' : r('doctypes-element'),
  'document-view-field' : r('document-view-field'),
  'document-view-tree' : r('document-view-tree'),
  'document-view' : r('document-view'),
  'index-element' : r('index-element'),
  'index-listing' : r('index-listing'),
  'index-options' : r('index-options'),
  'paged-listing' : r('paged-listing'),
  'preview-element' : r('preview-element'),
  'search-field-item' : r('search-field-item'),
  'set-listing' : r('set-listing'),
  'set-options' : r('set-options'),
  'simple-to-form-field' : r('simple-to-form-field'),
  'simple-to-form' : r('simple-to-form'),
  'worksheet' : r('worksheet')
};
},{"hogan.js":"nLm5Ax"}],"EMvo/m":[function(require,module,exports){
(function() {
  var Bacon, Bus, CompositeUnsubscribe, Dispatcher, End, Error, Event, EventStream, Initial, Next, None, Observable, Property, PropertyDispatcher, PropertyTransaction, Some, Source, addPropertyInitValueToStream, assert, assertArray, assertEvent, assertEventStream, assertFunction, assertNoArguments, assertString, cloneArray, compositeUnsubscribe, convertArgsToFunction, end, former, indexOf, initial, isFieldKey, isFunction, latterF, liftCallback, makeFunction, makeFunctionArgs, makeFunction_, makeSpawner, next, nop, partiallyApplied, sendWrapped, toCombinator, toEvent, toFieldExtractor, toFieldKey, toOption, toSimpleExtractor, withMethodCallSupport, _, _ref, _ref1, _ref2,
    __slice = [].slice,
    __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  Bacon = {};

  Bacon.fromBinder = function(binder, eventTransformer) {
    if (eventTransformer == null) {
      eventTransformer = _.id;
    }
    return new EventStream(function(sink) {
      var unbinder;
      return unbinder = binder(function() {
        var args, event, reply, value, _i, _len;
        args = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
        value = eventTransformer.apply(null, args);
        if (!(value instanceof Array && _.last(value) instanceof Event)) {
          value = [value];
        }
        reply = Bacon.more;
        for (_i = 0, _len = value.length; _i < _len; _i++) {
          event = value[_i];
          reply = sink(event = toEvent(event));
          if (reply === Bacon.noMore || event.isEnd()) {
            if (unbinder != null) {
              unbinder();
            } else {
              Bacon.scheduler.setTimeout((function() {
                return unbinder();
              }), 0);
            }
            return reply;
          }
        }
        return reply;
      });
    });
  };

  Bacon.$ = {
    asEventStream: function(eventName, selector, eventTransformer) {
      var _ref,
        _this = this;
      if (isFunction(selector)) {
        _ref = [selector, null], eventTransformer = _ref[0], selector = _ref[1];
      }
      return Bacon.fromBinder(function(handler) {
        _this.on(eventName, selector, handler);
        return function() {
          return _this.off(eventName, selector, handler);
        };
      }, eventTransformer);
    }
  };

  if ((_ref = typeof jQuery !== "undefined" && jQuery !== null ? jQuery : typeof Zepto !== "undefined" && Zepto !== null ? Zepto : null) != null) {
    _ref.fn.asEventStream = Bacon.$.asEventStream;
  }

  Bacon.fromEventTarget = function(target, eventName, eventTransformer) {
    var sub, unsub, _ref1, _ref2, _ref3, _ref4;
    sub = (_ref1 = target.addEventListener) != null ? _ref1 : (_ref2 = target.addListener) != null ? _ref2 : target.bind;
    unsub = (_ref3 = target.removeEventListener) != null ? _ref3 : (_ref4 = target.removeListener) != null ? _ref4 : target.unbind;
    return Bacon.fromBinder(function(handler) {
      sub.call(target, eventName, handler);
      return function() {
        return unsub.call(target, eventName, handler);
      };
    }, eventTransformer);
  };

  Bacon.fromPromise = function(promise, abort) {
    return Bacon.fromBinder(function(handler) {
      promise.then(handler, function(e) {
        return handler(new Error(e));
      });
      return function() {
        if (abort) {
          return typeof promise.abort === "function" ? promise.abort() : void 0;
        }
      };
    }, function(value) {
      return [value, end()];
    });
  };

  Bacon.noMore = ["<no-more>"];

  Bacon.more = ["<more>"];

  Bacon.later = function(delay, value) {
    return Bacon.sequentially(delay, [value]);
  };

  Bacon.sequentially = function(delay, values) {
    var index;
    index = 0;
    return Bacon.fromPoll(delay, function() {
      var value;
      value = values[index++];
      if (index < values.length) {
        return value;
      } else if (index === values.length) {
        return [value, end()];
      } else {
        return end();
      }
    });
  };

  Bacon.repeatedly = function(delay, values) {
    var index;
    index = 0;
    return Bacon.fromPoll(delay, function() {
      return values[index++ % values.length];
    });
  };

  withMethodCallSupport = function(wrapped) {
    return function() {
      var args, context, f, methodName;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      if (typeof f === "object" && args.length) {
        context = f;
        methodName = args[0];
        f = function() {
          return context[methodName].apply(context, arguments);
        };
        args = args.slice(1);
      }
      return wrapped.apply(null, [f].concat(__slice.call(args)));
    };
  };

  liftCallback = function(wrapped) {
    return withMethodCallSupport(function() {
      var args, f, stream;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      stream = partiallyApplied(wrapped, [
        function(values, callback) {
          return f.apply(null, __slice.call(values).concat([callback]));
        }
      ]);
      return Bacon.combineAsArray(args).flatMap(stream);
    });
  };

  Bacon.fromCallback = liftCallback(function() {
    var args, f;
    f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    return Bacon.fromBinder(function(handler) {
      makeFunction(f, args)(handler);
      return nop;
    }, function(value) {
      return [value, end()];
    });
  });

  Bacon.fromNodeCallback = liftCallback(function() {
    var args, f;
    f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    return Bacon.fromBinder(function(handler) {
      makeFunction(f, args)(handler);
      return nop;
    }, function(error, value) {
      if (error) {
        return [new Error(error), end()];
      }
      return [value, end()];
    });
  });

  Bacon.fromPoll = function(delay, poll) {
    return Bacon.fromBinder(function(handler) {
      var id;
      id = Bacon.scheduler.setInterval(handler, delay);
      return function() {
        return Bacon.scheduler.clearInterval(id);
      };
    }, poll);
  };

  Bacon.interval = function(delay, value) {
    if (value == null) {
      value = {};
    }
    return Bacon.fromPoll(delay, function() {
      return next(value);
    });
  };

  Bacon.constant = function(value) {
    return new Property(sendWrapped([value], initial), true);
  };

  Bacon.never = function() {
    return Bacon.fromArray([]);
  };

  Bacon.once = function(value) {
    return Bacon.fromArray([value]);
  };

  Bacon.fromArray = function(values) {
    assertArray(values);
    return new EventStream(sendWrapped(values, toEvent));
  };

  sendWrapped = function(values, wrapper) {
    return function(sink) {
      var value, _i, _len;
      for (_i = 0, _len = values.length; _i < _len; _i++) {
        value = values[_i];
        sink(wrapper(value));
      }
      sink(end());
      return nop;
    };
  };

  Bacon.mergeAll = function() {
    var streams;
    streams = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    if (streams[0] instanceof Array) {
      streams = streams[0];
    }
    return _.fold(streams, Bacon.never(), (function(a, b) {
      return a.merge(b);
    }));
  };

  Bacon.zipAsArray = function() {
    var streams;
    streams = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    if (streams[0] instanceof Array) {
      streams = streams[0];
    }
    return Bacon.zipWith(streams, function() {
      var xs;
      xs = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
      return xs;
    });
  };

  Bacon.zipWith = function() {
    var f, streams, _ref1;
    f = arguments[0], streams = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    if (!isFunction(f)) {
      _ref1 = [f, streams[0]], streams = _ref1[0], f = _ref1[1];
    }
    return Bacon.when(streams, f);
  };

  Bacon.combineAsArray = function() {
    var index, s, sources, stream, streams, _i, _len;
    streams = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    if (streams.length === 1 && streams[0] instanceof Array) {
      streams = streams[0];
    }
    for (index = _i = 0, _len = streams.length; _i < _len; index = ++_i) {
      stream = streams[index];
      if (!(stream instanceof Observable)) {
        streams[index] = Bacon.constant(stream);
      }
    }
    if (streams.length) {
      sources = (function() {
        var _j, _len1, _results;
        _results = [];
        for (_j = 0, _len1 = streams.length; _j < _len1; _j++) {
          s = streams[_j];
          _results.push(new Source(s, true, false, s.subscribeInternal));
        }
        return _results;
      })();
      return Bacon.when(sources, (function() {
        var xs;
        xs = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
        return xs;
      })).toProperty();
    } else {
      return Bacon.constant([]);
    }
  };

  Bacon.onValues = function() {
    var f, streams, _i;
    streams = 2 <= arguments.length ? __slice.call(arguments, 0, _i = arguments.length - 1) : (_i = 0, []), f = arguments[_i++];
    return Bacon.combineAsArray(streams).onValues(f);
  };

  Bacon.combineWith = function() {
    var f, streams;
    f = arguments[0], streams = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    return Bacon.combineAsArray(streams).map(function(values) {
      return f.apply(null, values);
    });
  };

  Bacon.combineTemplate = function(template) {
    var applyStreamValue, combinator, compile, compileTemplate, constantValue, current, funcs, mkContext, setValue, streams;
    funcs = [];
    streams = [];
    current = function(ctxStack) {
      return ctxStack[ctxStack.length - 1];
    };
    setValue = function(ctxStack, key, value) {
      return current(ctxStack)[key] = value;
    };
    applyStreamValue = function(key, index) {
      return function(ctxStack, values) {
        return setValue(ctxStack, key, values[index]);
      };
    };
    constantValue = function(key, value) {
      return function(ctxStack) {
        return setValue(ctxStack, key, value);
      };
    };
    mkContext = function(template) {
      if (template instanceof Array) {
        return [];
      } else {
        return {};
      }
    };
    compile = function(key, value) {
      var popContext, pushContext;
      if (value instanceof Observable) {
        streams.push(value);
        return funcs.push(applyStreamValue(key, streams.length - 1));
      } else if (value === Object(value) && typeof value !== "function") {
        pushContext = function(key) {
          return function(ctxStack) {
            var newContext;
            newContext = mkContext(value);
            setValue(ctxStack, key, newContext);
            return ctxStack.push(newContext);
          };
        };
        popContext = function(ctxStack) {
          return ctxStack.pop();
        };
        funcs.push(pushContext(key));
        compileTemplate(value);
        return funcs.push(popContext);
      } else {
        return funcs.push(constantValue(key, value));
      }
    };
    compileTemplate = function(template) {
      return _.each(template, compile);
    };
    compileTemplate(template);
    combinator = function(values) {
      var ctxStack, f, rootContext, _i, _len;
      rootContext = mkContext(template);
      ctxStack = [rootContext];
      for (_i = 0, _len = funcs.length; _i < _len; _i++) {
        f = funcs[_i];
        f(ctxStack, values);
      }
      return rootContext;
    };
    return Bacon.combineAsArray(streams).map(combinator);
  };

  Event = (function() {
    function Event() {}

    Event.prototype.isEvent = function() {
      return true;
    };

    Event.prototype.isEnd = function() {
      return false;
    };

    Event.prototype.isInitial = function() {
      return false;
    };

    Event.prototype.isNext = function() {
      return false;
    };

    Event.prototype.isError = function() {
      return false;
    };

    Event.prototype.hasValue = function() {
      return false;
    };

    Event.prototype.filter = function() {
      return true;
    };

    return Event;

  })();

  Next = (function(_super) {
    __extends(Next, _super);

    function Next(valueF) {
      if (isFunction(valueF)) {
        this.value = _.cached(valueF);
      } else {
        this.value = _.always(valueF);
      }
    }

    Next.prototype.isNext = function() {
      return true;
    };

    Next.prototype.hasValue = function() {
      return true;
    };

    Next.prototype.fmap = function(f) {
      var _this = this;
      return this.apply(function() {
        return f(_this.value());
      });
    };

    Next.prototype.apply = function(value) {
      return new Next(value);
    };

    Next.prototype.filter = function(f) {
      return f(this.value());
    };

    Next.prototype.describe = function() {
      return this.value();
    };

    return Next;

  })(Event);

  Initial = (function(_super) {
    __extends(Initial, _super);

    function Initial() {
      _ref1 = Initial.__super__.constructor.apply(this, arguments);
      return _ref1;
    }

    Initial.prototype.isInitial = function() {
      return true;
    };

    Initial.prototype.isNext = function() {
      return false;
    };

    Initial.prototype.apply = function(value) {
      return new Initial(value);
    };

    Initial.prototype.toNext = function() {
      return new Next(this.value);
    };

    return Initial;

  })(Next);

  End = (function(_super) {
    __extends(End, _super);

    function End() {
      _ref2 = End.__super__.constructor.apply(this, arguments);
      return _ref2;
    }

    End.prototype.isEnd = function() {
      return true;
    };

    End.prototype.fmap = function() {
      return this;
    };

    End.prototype.apply = function() {
      return this;
    };

    End.prototype.describe = function() {
      return "<end>";
    };

    return End;

  })(Event);

  Error = (function(_super) {
    __extends(Error, _super);

    function Error(error) {
      this.error = error;
    }

    Error.prototype.isError = function() {
      return true;
    };

    Error.prototype.fmap = function() {
      return this;
    };

    Error.prototype.apply = function() {
      return this;
    };

    Error.prototype.describe = function() {
      return "<error> " + this.error;
    };

    return Error;

  })(Event);

  Observable = (function() {
    function Observable() {
      this.combine = __bind(this.combine, this);
      this.flatMapLatest = __bind(this.flatMapLatest, this);
      this.fold = __bind(this.fold, this);
      this.scan = __bind(this.scan, this);
      this.assign = this.onValue;
    }

    Observable.prototype.onValue = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.subscribe(function(event) {
        if (event.hasValue()) {
          return f(event.value());
        }
      });
    };

    Observable.prototype.onValues = function(f) {
      return this.onValue(function(args) {
        return f.apply(null, args);
      });
    };

    Observable.prototype.onError = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.subscribe(function(event) {
        if (event.isError()) {
          return f(event.error);
        }
      });
    };

    Observable.prototype.onEnd = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.subscribe(function(event) {
        if (event.isEnd()) {
          return f();
        }
      });
    };

    Observable.prototype.errors = function() {
      return this.filter(function() {
        return false;
      });
    };

    Observable.prototype.filter = function() {
      var args, f;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      return convertArgsToFunction(this, f, args, function(f) {
        return this.withHandler(function(event) {
          if (event.filter(f)) {
            return this.push(event);
          } else {
            return Bacon.more;
          }
        });
      });
    };

    Observable.prototype.takeWhile = function() {
      var args, f;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      return convertArgsToFunction(this, f, args, function(f) {
        return this.withHandler(function(event) {
          if (event.filter(f)) {
            return this.push(event);
          } else {
            this.push(end());
            return Bacon.noMore;
          }
        });
      });
    };

    Observable.prototype.endOnError = function() {
      var args, f;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      if (f == null) {
        f = true;
      }
      return convertArgsToFunction(this, f, args, function(f) {
        return this.withHandler(function(event) {
          if (event.isError() && f(event.error)) {
            this.push(event);
            return this.push(end());
          } else {
            return this.push(event);
          }
        });
      });
    };

    Observable.prototype.take = function(count) {
      if (count <= 0) {
        return Bacon.never();
      }
      return this.withHandler(function(event) {
        if (!event.hasValue()) {
          return this.push(event);
        } else {
          count--;
          if (count > 0) {
            return this.push(event);
          } else {
            if (count === 0) {
              this.push(event);
            }
            this.push(end());
            return Bacon.noMore;
          }
        }
      });
    };

    Observable.prototype.map = function() {
      var args, p;
      p = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      if (p instanceof Property) {
        return p.sampledBy(this, former);
      } else {
        return convertArgsToFunction(this, p, args, function(f) {
          return this.withHandler(function(event) {
            return this.push(event.fmap(f));
          });
        });
      }
    };

    Observable.prototype.mapError = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.withHandler(function(event) {
        if (event.isError()) {
          return this.push(next(f(event.error)));
        } else {
          return this.push(event);
        }
      });
    };

    Observable.prototype.mapEnd = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.withHandler(function(event) {
        if (event.isEnd()) {
          this.push(next(f(event)));
          this.push(end());
          return Bacon.noMore;
        } else {
          return this.push(event);
        }
      });
    };

    Observable.prototype.doAction = function() {
      var f;
      f = makeFunctionArgs(arguments);
      return this.withHandler(function(event) {
        if (event.hasValue()) {
          f(event.value());
        }
        return this.push(event);
      });
    };

    Observable.prototype.skip = function(count) {
      return this.withHandler(function(event) {
        if (!event.hasValue()) {
          return this.push(event);
        } else if (count > 0) {
          count--;
          return Bacon.more;
        } else {
          return this.push(event);
        }
      });
    };

    Observable.prototype.skipDuplicates = function(isEqual) {
      if (isEqual == null) {
        isEqual = function(a, b) {
          return a === b;
        };
      }
      return this.withStateMachine(None, function(prev, event) {
        if (!event.hasValue()) {
          return [prev, [event]];
        } else if (event.isInitial() || prev === None || !isEqual(prev.get(), event.value())) {
          return [new Some(event.value()), [event]];
        } else {
          return [prev, []];
        }
      });
    };

    Observable.prototype.skipErrors = function() {
      return this.withHandler(function(event) {
        if (event.isError()) {
          return Bacon.more;
        } else {
          return this.push(event);
        }
      });
    };

    Observable.prototype.withStateMachine = function(initState, f) {
      var state;
      state = initState;
      return this.withHandler(function(event) {
        var fromF, newState, output, outputs, reply, _i, _len;
        fromF = f(state, event);
        newState = fromF[0], outputs = fromF[1];
        state = newState;
        reply = Bacon.more;
        for (_i = 0, _len = outputs.length; _i < _len; _i++) {
          output = outputs[_i];
          reply = this.push(output);
          if (reply === Bacon.noMore) {
            return reply;
          }
        }
        return reply;
      });
    };

    Observable.prototype.scan = function(seed, f, lazyF) {
      var acc, f_, subscribe,
        _this = this;
      f_ = toCombinator(f);
      f = lazyF ? f_ : function(x, y) {
        return f_(x(), y());
      };
      acc = toOption(seed).map(function(x) {
        return _.always(x);
      });
      subscribe = function(sink) {
        var initSent, reply, sendInit, unsub;
        initSent = false;
        unsub = nop;
        reply = Bacon.more;
        sendInit = function() {
          if (!initSent) {
            initSent = true;
            return acc.forEach(function(valueF) {
              reply = sink(new Initial(valueF));
              if (reply === Bacon.noMore) {
                unsub();
                return unsub = nop;
              }
            });
          }
        };
        unsub = _this.subscribe(function(event) {
          var next, prev;
          if (event.hasValue()) {
            if (initSent && event.isInitial()) {
              return Bacon.more;
            } else {
              if (!event.isInitial()) {
                sendInit();
              }
              initSent = true;
              prev = acc.getOrElse(function() {
                return void 0;
              });
              next = _.cached(function() {
                return f(prev, event.value);
              });
              acc = new Some(next);
              return sink(event.apply(next));
            }
          } else {
            if (event.isEnd()) {
              reply = sendInit();
            }
            if (reply !== Bacon.noMore) {
              return sink(event);
            }
          }
        });
        sendInit();
        return unsub;
      };
      return new Property(subscribe);
    };

    Observable.prototype.fold = function(seed, f) {
      return this.scan(seed, f).sampledBy(this.filter(false).mapEnd().toProperty());
    };

    Observable.prototype.zip = function(other, f) {
      if (f == null) {
        f = Array;
      }
      return Bacon.zipWith([this, other], f);
    };

    Observable.prototype.diff = function(start, f) {
      f = toCombinator(f);
      return this.scan([start], function(prevTuple, next) {
        return [next, f(prevTuple[0], next)];
      }).filter(function(tuple) {
        return tuple.length === 2;
      }).map(function(tuple) {
        return tuple[1];
      });
    };

    Observable.prototype.flatMap = function(f, firstOnly) {
      var root;
      f = makeSpawner(f);
      root = this;
      return new EventStream(function(sink) {
        var checkEnd, composite;
        composite = new CompositeUnsubscribe();
        checkEnd = function(unsub) {
          unsub();
          if (composite.empty()) {
            return sink(end());
          }
        };
        composite.add(function(__, unsubRoot) {
          return root.subscribe(function(event) {
            var child;
            if (event.isEnd()) {
              return checkEnd(unsubRoot);
            } else if (event.isError()) {
              return sink(event);
            } else if (firstOnly && composite.count() > 1) {
              return Bacon.more;
            } else {
              if (composite.unsubscribed) {
                return Bacon.noMore;
              }
              child = f(event.value());
              if (!(child instanceof Observable)) {
                child = Bacon.once(child);
              }
              return composite.add(function(unsubAll, unsubMe) {
                return child.subscribe(function(event) {
                  var reply;
                  if (event.isEnd()) {
                    checkEnd(unsubMe);
                    return Bacon.noMore;
                  } else {
                    if (event instanceof Initial) {
                      event = event.toNext();
                    }
                    reply = sink(event);
                    if (reply === Bacon.noMore) {
                      unsubAll();
                    }
                    return reply;
                  }
                });
              });
            }
          });
        });
        return composite.unsubscribe;
      });
    };

    Observable.prototype.flatMapFirst = function(f) {
      return this.flatMap(f, true);
    };

    Observable.prototype.flatMapLatest = function(f) {
      var stream,
        _this = this;
      f = makeSpawner(f);
      stream = this.toEventStream();
      return stream.flatMap(function(value) {
        return f(value).takeUntil(stream);
      });
    };

    Observable.prototype.not = function() {
      return this.map(function(x) {
        return !x;
      });
    };

    Observable.prototype.log = function() {
      var args;
      args = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
      this.subscribe(function(event) {
        return typeof console !== "undefined" && console !== null ? typeof console.log === "function" ? console.log.apply(console, __slice.call(args).concat([event.describe()])) : void 0 : void 0;
      });
      return this;
    };

    Observable.prototype.slidingWindow = function(n, minValues) {
      if (minValues == null) {
        minValues = 0;
      }
      return this.scan([], (function(window, value) {
        return window.concat([value]).slice(-n);
      })).filter((function(values) {
        return values.length >= minValues;
      }));
    };

    Observable.prototype.combine = function(other, f) {
      var combinator;
      combinator = toCombinator(f);
      return Bacon.combineAsArray(this, other).map(function(values) {
        return combinator(values[0], values[1]);
      });
    };

    Observable.prototype.decode = function(cases) {
      return this.combine(Bacon.combineTemplate(cases), function(key, values) {
        return values[key];
      });
    };

    Observable.prototype.awaiting = function(other) {
      return this.toEventStream().map(true).merge(other.toEventStream().map(false)).toProperty(false);
    };

    return Observable;

  })();

  Observable.prototype.reduce = Observable.prototype.fold;

  EventStream = (function(_super) {
    __extends(EventStream, _super);

    function EventStream(subscribe) {
      this.takeUntil = __bind(this.takeUntil, this);
      this.sampledBy = __bind(this.sampledBy, this);
      var dispatcher;
      EventStream.__super__.constructor.call(this);
      assertFunction(subscribe);
      dispatcher = new Dispatcher(subscribe);
      this.subscribe = dispatcher.subscribe;
      this.subscribeInternal = this.subscribe;
      this.hasSubscribers = dispatcher.hasSubscribers;
    }

    EventStream.prototype.delay = function(delay) {
      return this.flatMap(function(value) {
        return Bacon.later(delay, value);
      });
    };

    EventStream.prototype.debounce = function(delay) {
      return this.flatMapLatest(function(value) {
        return Bacon.later(delay, value);
      });
    };

    EventStream.prototype.debounceImmediate = function(delay) {
      return this.flatMapFirst(function(value) {
        return Bacon.once(value).concat(Bacon.later(delay).filter(false));
      });
    };

    EventStream.prototype.throttle = function(delay) {
      return this.bufferWithTime(delay).map(function(values) {
        return values[values.length - 1];
      });
    };

    EventStream.prototype.bufferWithTime = function(delay) {
      return this.bufferWithTimeOrCount(delay, Number.MAX_VALUE);
    };

    EventStream.prototype.bufferWithCount = function(count) {
      return this.bufferWithTimeOrCount(void 0, count);
    };

    EventStream.prototype.bufferWithTimeOrCount = function(delay, count) {
      var flushOrSchedule;
      flushOrSchedule = function(buffer) {
        if (buffer.values.length === count) {
          return buffer.flush();
        } else if (delay !== void 0) {
          return buffer.schedule();
        }
      };
      return this.buffer(delay, flushOrSchedule, flushOrSchedule);
    };

    EventStream.prototype.buffer = function(delay, onInput, onFlush) {
      var buffer, delayMs, reply;
      if (onInput == null) {
        onInput = (function() {});
      }
      if (onFlush == null) {
        onFlush = (function() {});
      }
      buffer = {
        scheduled: false,
        end: null,
        values: [],
        flush: function() {
          var reply;
          this.scheduled = false;
          if (this.values.length > 0) {
            reply = this.push(next(this.values));
            this.values = [];
            if (this.end != null) {
              return this.push(this.end);
            } else if (reply !== Bacon.noMore) {
              return onFlush(this);
            }
          } else {
            if (this.end != null) {
              return this.push(this.end);
            }
          }
        },
        schedule: function() {
          var _this = this;
          if (!this.scheduled) {
            this.scheduled = true;
            return delay(function() {
              return _this.flush();
            });
          }
        }
      };
      reply = Bacon.more;
      if (!isFunction(delay)) {
        delayMs = delay;
        delay = function(f) {
          return Bacon.scheduler.setTimeout(f, delayMs);
        };
      }
      return this.withHandler(function(event) {
        buffer.push = this.push;
        if (event.isError()) {
          reply = this.push(event);
        } else if (event.isEnd()) {
          buffer.end = event;
          if (!buffer.scheduled) {
            buffer.flush();
          }
        } else {
          buffer.values.push(event.value());
          onInput(buffer);
        }
        return reply;
      });
    };

    EventStream.prototype.merge = function(right) {
      var left;
      assertEventStream(right);
      left = this;
      return new EventStream(function(sink) {
        var ends, smartSink;
        ends = 0;
        smartSink = function(obs) {
          return function(unsubBoth) {
            return obs.subscribe(function(event) {
              var reply;
              if (event.isEnd()) {
                ends++;
                if (ends === 2) {
                  return sink(end());
                } else {
                  return Bacon.more;
                }
              } else {
                reply = sink(event);
                if (reply === Bacon.noMore) {
                  unsubBoth();
                }
                return reply;
              }
            });
          };
        };
        return compositeUnsubscribe(smartSink(left), smartSink(right));
      });
    };

    EventStream.prototype.toProperty = function(initValue) {
      if (arguments.length === 0) {
        initValue = None;
      }
      return this.scan(initValue, latterF, true);
    };

    EventStream.prototype.toEventStream = function() {
      return this;
    };

    EventStream.prototype.sampledBy = function(sampler, combinator) {
      return this.toProperty().sampledBy(sampler, combinator);
    };

    EventStream.prototype.concat = function(right) {
      var left;
      left = this;
      return new EventStream(function(sink) {
        var unsub;
        unsub = left.subscribe(function(e) {
          if (e.isEnd()) {
            return unsub = right.subscribe(sink);
          } else {
            return sink(e);
          }
        });
        return function() {
          return unsub();
        };
      });
    };

    EventStream.prototype.takeUntil = function(stopper) {
      var self;
      self = this;
      return new EventStream(function(sink) {
        var produce, stop;
        stop = function(unsubAll) {
          return stopper.onValue(function() {
            sink(end());
            unsubAll();
            return Bacon.noMore;
          });
        };
        produce = function(unsubAll) {
          return self.subscribe(function(x) {
            var reply;
            reply = sink(x);
            if (x.isEnd() || reply === Bacon.noMore) {
              unsubAll();
            }
            return reply;
          });
        };
        return compositeUnsubscribe(stop, produce);
      });
    };

    EventStream.prototype.skipUntil = function(starter) {
      var started;
      started = starter.take(1).map(true).toProperty(false);
      return this.filter(started);
    };

    EventStream.prototype.skipWhile = function() {
      var args, f, ok;
      f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
      ok = false;
      return convertArgsToFunction(this, f, args, function(f) {
        return this.withHandler(function(event) {
          if (ok || !event.hasValue() || !f(event.value())) {
            if (event.hasValue()) {
              ok = true;
            }
            return this.push(event);
          } else {
            return Bacon.more;
          }
        });
      });
    };

    EventStream.prototype.startWith = function(seed) {
      return Bacon.once(seed).concat(this);
    };

    EventStream.prototype.withHandler = function(handler) {
      var dispatcher;
      dispatcher = new Dispatcher(this.subscribe, handler);
      return new EventStream(dispatcher.subscribe);
    };

    EventStream.prototype.withSubscribe = function(subscribe) {
      return new EventStream(subscribe);
    };

    return EventStream;

  })(Observable);

  Property = (function(_super) {
    __extends(Property, _super);

    function Property(subscribe, handler) {
      this.toEventStream = __bind(this.toEventStream, this);
      this.toProperty = __bind(this.toProperty, this);
      this.changes = __bind(this.changes, this);
      this.sample = __bind(this.sample, this);
      var _this = this;
      Property.__super__.constructor.call(this);
      if (handler === true) {
        this.subscribeInternal = subscribe;
      } else {
        this.subscribeInternal = new PropertyDispatcher(subscribe, handler).subscribe;
      }
      this.sampledBy = function(sampler, combinator) {
        var lazy, samplerSource, stream, thisSource;
        if (combinator != null) {
          combinator = toCombinator(combinator);
        } else {
          lazy = true;
          combinator = function(f) {
            return f();
          };
        }
        thisSource = new Source(_this, false, false, _this.subscribeInternal, lazy);
        samplerSource = new Source(sampler, true, false, sampler.subscribe, lazy);
        stream = Bacon.when([thisSource, samplerSource], combinator);
        if (sampler instanceof Property) {
          return stream.toProperty();
        } else {
          return stream;
        }
      };
      this.subscribe = function(sink) {
        var LatestEvent, end, reply, unsub, value;
        reply = Bacon.more;
        LatestEvent = (function() {
          function LatestEvent() {}

          LatestEvent.prototype.set = function(event) {
            return this.event = event;
          };

          LatestEvent.prototype.send = function() {
            var event;
            event = this.event;
            this.event = null;
            if ((event != null) && reply !== Bacon.noMore) {
              reply = sink(event);
              if (reply === Bacon.noMore) {
                return unsub();
              }
            }
          };

          return LatestEvent;

        })();
        value = new LatestEvent();
        end = new LatestEvent();
        unsub = nop;
        unsub = _this.subscribeInternal(function(event) {
          if (event.isError()) {
            if (reply !== Bacon.noMore) {
              reply = sink(event);
            }
          } else {
            if (event.hasValue()) {
              value.set(event);
            } else if (event.isEnd()) {
              end.set(event);
            }
            PropertyTransaction.onDone(function() {
              value.send();
              return end.send();
            });
          }
          return reply;
        });
        return function() {
          reply = Bacon.noMore;
          return unsub();
        };
      };
    }

    Property.prototype.sample = function(interval) {
      return this.sampledBy(Bacon.interval(interval, {}));
    };

    Property.prototype.changes = function() {
      var _this = this;
      return new EventStream(function(sink) {
        return _this.subscribe(function(event) {
          if (!event.isInitial()) {
            return sink(event);
          }
        });
      });
    };

    Property.prototype.withHandler = function(handler) {
      return new Property(this.subscribeInternal, handler);
    };

    Property.prototype.withSubscribe = function(subscribe) {
      return new Property(subscribe);
    };

    Property.prototype.toProperty = function() {
      assertNoArguments(arguments);
      return this;
    };

    Property.prototype.toEventStream = function() {
      var _this = this;
      return new EventStream(function(sink) {
        return _this.subscribe(function(event) {
          if (event.isInitial()) {
            event = event.toNext();
          }
          return sink(event);
        });
      });
    };

    Property.prototype.and = function(other) {
      return this.combine(other, function(x, y) {
        return x && y;
      });
    };

    Property.prototype.or = function(other) {
      return this.combine(other, function(x, y) {
        return x || y;
      });
    };

    Property.prototype.delay = function(delay) {
      return this.delayChanges(function(changes) {
        return changes.delay(delay);
      });
    };

    Property.prototype.debounce = function(delay) {
      return this.delayChanges(function(changes) {
        return changes.debounce(delay);
      });
    };

    Property.prototype.throttle = function(delay) {
      return this.delayChanges(function(changes) {
        return changes.throttle(delay);
      });
    };

    Property.prototype.delayChanges = function(f) {
      return addPropertyInitValueToStream(this, f(this.changes()));
    };

    Property.prototype.takeUntil = function(stopper) {
      var changes;
      changes = this.changes().takeUntil(stopper);
      return addPropertyInitValueToStream(this, changes);
    };

    Property.prototype.startWith = function(value) {
      return this.scan(value, function(prev, next) {
        return next;
      });
    };

    return Property;

  })(Observable);

  convertArgsToFunction = function(obs, f, args, method) {
    var sampled;
    if (f instanceof Property) {
      sampled = f.sampledBy(obs, function(p, s) {
        return [p, s];
      });
      return method.apply(sampled, [
        function(_arg) {
          var p, s;
          p = _arg[0], s = _arg[1];
          return p;
        }
      ]).map(function(_arg) {
        var p, s;
        p = _arg[0], s = _arg[1];
        return s;
      });
    } else {
      f = makeFunction(f, args);
      return method.apply(obs, [f]);
    }
  };

  addPropertyInitValueToStream = function(property, stream) {
    var getInitValue;
    getInitValue = function(property) {
      var value;
      value = None;
      property.subscribe(function(event) {
        if (event.hasValue()) {
          value = new Some(event.value());
        }
        return Bacon.noMore;
      });
      return value;
    };
    return stream.toProperty(getInitValue(property));
  };

  Dispatcher = (function() {
    function Dispatcher(subscribe, handleEvent) {
      var done, ended, prevError, pushing, queue, removeSub, subscriptions, unsubscribeFromSource, waiters,
        _this = this;
      if (subscribe == null) {
        subscribe = function() {
          return nop;
        };
      }
      subscriptions = [];
      queue = null;
      pushing = false;
      ended = false;
      this.hasSubscribers = function() {
        return subscriptions.length > 0;
      };
      prevError = null;
      unsubscribeFromSource = nop;
      removeSub = function(subscription) {
        return subscriptions = _.without(subscription, subscriptions);
      };
      waiters = null;
      done = function() {
        var w, ws, _i, _len, _results;
        if (waiters != null) {
          ws = waiters;
          waiters = null;
          _results = [];
          for (_i = 0, _len = ws.length; _i < _len; _i++) {
            w = ws[_i];
            _results.push(w());
          }
          return _results;
        }
      };
      this.push = function(event) {
        var reply, sub, success, tmp, _i, _len;
        if (!pushing) {
          if (event === prevError) {
            return;
          }
          if (event.isError()) {
            prevError = event;
          }
          success = false;
          try {
            pushing = true;
            tmp = subscriptions;
            for (_i = 0, _len = tmp.length; _i < _len; _i++) {
              sub = tmp[_i];
              reply = sub.sink(event);
              if (reply === Bacon.noMore || event.isEnd()) {
                removeSub(sub);
              }
            }
            success = true;
          } finally {
            pushing = false;
            if (!success) {
              queue = null;
            }
          }
          success = true;
          while (queue != null ? queue.length : void 0) {
            event = _.head(queue);
            queue = _.tail(queue);
            _this.push(event);
          }
          done(event);
          if (_this.hasSubscribers()) {
            return Bacon.more;
          } else {
            return Bacon.noMore;
          }
        } else {
          queue = (queue || []).concat([event]);
          return Bacon.more;
        }
      };
      if (handleEvent == null) {
        handleEvent = function(event) {
          return this.push(event);
        };
      }
      this.handleEvent = function(event) {
        if (event.isEnd()) {
          ended = true;
        }
        return handleEvent.apply(_this, [event]);
      };
      this.subscribe = function(sink) {
        var subscription;
        if (ended) {
          sink(end());
          return nop;
        } else {
          assertFunction(sink);
          subscription = {
            sink: sink
          };
          subscriptions = subscriptions.concat(subscription);
          if (subscriptions.length === 1) {
            unsubscribeFromSource = subscribe(_this.handleEvent);
          }
          assertFunction(unsubscribeFromSource);
          return function() {
            removeSub(subscription);
            if (!_this.hasSubscribers()) {
              return unsubscribeFromSource();
            }
          };
        }
      };
    }

    return Dispatcher;

  })();

  PropertyDispatcher = (function(_super) {
    __extends(PropertyDispatcher, _super);

    function PropertyDispatcher(subscribe, handleEvent) {
      var current, ended, push,
        _this = this;
      PropertyDispatcher.__super__.constructor.call(this, subscribe, handleEvent);
      current = None;
      push = this.push;
      subscribe = this.subscribe;
      ended = false;
      this.push = function(event) {
        if (event.isEnd()) {
          ended = true;
        }
        if (event.hasValue()) {
          current = new Some(event.value);
        }
        return PropertyTransaction.inTransaction(function() {
          return push.apply(_this, [event]);
        });
      };
      this.subscribe = function(sink) {
        var initSent, reply, shouldBounceInitialValue;
        initSent = false;
        shouldBounceInitialValue = function() {
          return _this.hasSubscribers() || ended;
        };
        reply = current.filter(shouldBounceInitialValue).map(function(val) {
          return sink(initial(val()));
        });
        if (reply.getOrElse(Bacon.more) === Bacon.noMore) {
          return nop;
        } else if (ended) {
          sink(end());
          return nop;
        } else {
          return subscribe.apply(_this, [sink]);
        }
      };
    }

    return PropertyDispatcher;

  })(Dispatcher);

  PropertyTransaction = (function() {
    var inTransaction, onDone, tx, txListeners;
    txListeners = [];
    tx = false;
    onDone = function(f) {
      if (tx) {
        return txListeners.push(f);
      } else {
        return f();
      }
    };
    inTransaction = function(f) {
      var g, gs, result, _i, _len;
      if (tx) {
        return f();
      } else {
        tx = true;
        try {
          result = f();
        } finally {
          tx = false;
        }
        gs = txListeners;
        txListeners = [];
        for (_i = 0, _len = gs.length; _i < _len; _i++) {
          g = gs[_i];
          g();
        }
        return result;
      }
    };
    return {
      onDone: onDone,
      inTransaction: inTransaction
    };
  })();

  Bus = (function(_super) {
    __extends(Bus, _super);

    function Bus() {
      var ended, guardedSink, sink, subscribeAll, subscribeInput, subscriptions, unsubAll, unsubscribeInput,
        _this = this;
      sink = void 0;
      subscriptions = [];
      ended = false;
      guardedSink = function(input) {
        return function(event) {
          if (event.isEnd()) {
            unsubscribeInput(input);
            return Bacon.noMore;
          } else {
            return sink(event);
          }
        };
      };
      unsubAll = function() {
        var sub, _i, _len, _results;
        _results = [];
        for (_i = 0, _len = subscriptions.length; _i < _len; _i++) {
          sub = subscriptions[_i];
          _results.push(typeof sub.unsub === "function" ? sub.unsub() : void 0);
        }
        return _results;
      };
      subscribeInput = function(subscription) {
        return subscription.unsub = subscription.input.subscribe(guardedSink(subscription.input));
      };
      unsubscribeInput = function(input) {
        var i, sub, _i, _len;
        for (i = _i = 0, _len = subscriptions.length; _i < _len; i = ++_i) {
          sub = subscriptions[i];
          if (sub.input === input) {
            if (typeof sub.unsub === "function") {
              sub.unsub();
            }
            subscriptions.splice(i, 1);
            return;
          }
        }
      };
      subscribeAll = function(newSink) {
        var subscription, _i, _len, _ref3;
        sink = newSink;
        _ref3 = cloneArray(subscriptions);
        for (_i = 0, _len = _ref3.length; _i < _len; _i++) {
          subscription = _ref3[_i];
          subscribeInput(subscription);
        }
        return unsubAll;
      };
      Bus.__super__.constructor.call(this, subscribeAll);
      this.plug = function(input) {
        var sub;
        if (ended) {
          return;
        }
        sub = {
          input: input
        };
        subscriptions.push(sub);
        if ((sink != null)) {
          subscribeInput(sub);
        }
        return function() {
          return unsubscribeInput(input);
        };
      };
      this.push = function(value) {
        return typeof sink === "function" ? sink(next(value)) : void 0;
      };
      this.error = function(error) {
        return typeof sink === "function" ? sink(new Error(error)) : void 0;
      };
      this.end = function() {
        ended = true;
        unsubAll();
        return typeof sink === "function" ? sink(end()) : void 0;
      };
    }

    return Bus;

  })(EventStream);

  Source = (function() {
    function Source(s, sync, consume, subscribe, lazy) {
      var invoke, queue;
      this.sync = sync;
      this.subscribe = subscribe;
      if (lazy == null) {
        lazy = false;
      }
      queue = [];
      invoke = lazy ? _.id : function(f) {
        return f();
      };
      if (this.subscribe == null) {
        this.subscribe = s.subscribe;
      }
      this.markEnded = function() {
        return this.ended = true;
      };
      if (consume) {
        this.consume = function() {
          return invoke(queue.shift());
        };
        this.push = function(x) {
          return queue.push(x);
        };
        this.mayHave = function(c) {
          return !this.ended || queue.length >= c;
        };
        this.hasAtLeast = function(c) {
          return queue.length >= c;
        };
      } else {
        this.consume = function() {
          return invoke(queue[0]);
        };
        this.push = function(x) {
          return queue = [x];
        };
        this.mayHave = function() {
          return true;
        };
        this.hasAtLeast = function() {
          return queue.length;
        };
      }
    }

    return Source;

  })();

  Source.fromObservable = function(s) {
    if (s instanceof Source) {
      return s;
    } else if (s instanceof Property) {
      return new Source(s, false, false);
    } else {
      return new Source(s, true, true);
    }
  };

  Bacon.when = function() {
    var f, i, index, ix, len, pat, patSources, pats, patterns, s, sources, usage, _i, _j, _len, _len1, _ref3;
    patterns = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    if (patterns.length === 0) {
      return Bacon.never();
    }
    len = patterns.length;
    usage = "when: expecting arguments in the form (Observable+,function)+";
    assert(usage, len % 2 === 0);
    sources = [];
    pats = [];
    i = 0;
    while (i < len) {
      patSources = _.toArray(patterns[i]);
      f = patterns[i + 1];
      pat = {
        f: (isFunction(f) ? f : (function() {
          return f;
        })),
        ixs: []
      };
      for (_i = 0, _len = patSources.length; _i < _len; _i++) {
        s = patSources[_i];
        assert(s instanceof Observable, usage);
        index = sources.indexOf(s);
        if (index < 0) {
          sources.push(s);
          index = sources.length - 1;
        }
        _ref3 = pat.ixs;
        for (_j = 0, _len1 = _ref3.length; _j < _len1; _j++) {
          ix = _ref3[_j];
          if (ix.index === index) {
            ix.count++;
          }
        }
        pat.ixs.push({
          index: index,
          count: 1
        });
      }
      if (patSources.length > 0) {
        pats.push(pat);
      }
      i = i + 2;
    }
    if (!sources.length) {
      return Bacon.never();
    }
    sources = _.map(Source.fromObservable, sources);
    return new EventStream(function(sink) {
      var cannotMatch, cannotSync, match, part;
      match = function(p) {
        return _.all(p.ixs, function(i) {
          return sources[i.index].hasAtLeast(i.count);
        });
      };
      cannotSync = function(source) {
        return !source.sync || source.ended;
      };
      cannotMatch = function(p) {
        return _.any(p.ixs, function(i) {
          return !sources[i.index].mayHave(i.count);
        });
      };
      part = function(source) {
        return function(unsubAll) {
          return source.subscribe(function(e) {
            var p, reply, val, _k, _len2;
            if (e.isEnd()) {
              source.markEnded();
              if (_.all(sources, cannotSync) || _.all(pats, cannotMatch)) {
                reply = Bacon.noMore;
                sink(end());
              }
            } else if (e.isError()) {
              reply = sink(e);
            } else {
              source.push(e.value);
              if (source.sync) {
                for (_k = 0, _len2 = pats.length; _k < _len2; _k++) {
                  p = pats[_k];
                  if (match(p)) {
                    val = function() {
                      return p.f.apply(p, (function() {
                        var _l, _len3, _ref4, _results;
                        _ref4 = p.ixs;
                        _results = [];
                        for (_l = 0, _len3 = _ref4.length; _l < _len3; _l++) {
                          i = _ref4[_l];
                          _results.push(sources[i.index].consume());
                        }
                        return _results;
                      })());
                    };
                    reply = sink(e.apply(val));
                    break;
                  }
                }
              }
            }
            if (reply === Bacon.noMore) {
              unsubAll();
            }
            return reply || Bacon.more;
          });
        };
      };
      return compositeUnsubscribe.apply(null, (function() {
        var _k, _len2, _results;
        _results = [];
        for (i = _k = 0, _len2 = sources.length; _k < _len2; i = ++_k) {
          s = sources[i];
          _results.push(part(s, i));
        }
        return _results;
      })());
    });
  };

  Bacon.update = function() {
    var i, initial, lateBindFirst, patterns;
    initial = arguments[0], patterns = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    lateBindFirst = function(f) {
      return function() {
        var args;
        args = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
        return function(i) {
          return f.apply(null, [i].concat(args));
        };
      };
    };
    i = patterns.length - 1;
    while (i > 0) {
      if (!(patterns[i] instanceof Function)) {
        patterns[i] = (function(x) {
          return function() {
            return x;
          };
        })(patterns[i]);
      }
      patterns[i] = lateBindFirst(patterns[i]);
      i = i - 2;
    }
    return Bacon.when.apply(Bacon, patterns).scan(initial, (function(x, f) {
      return f(x);
    }));
  };

  compositeUnsubscribe = function() {
    var ss;
    ss = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    return new CompositeUnsubscribe(ss).unsubscribe;
  };

  CompositeUnsubscribe = (function() {
    function CompositeUnsubscribe(ss) {
      var s, _i, _len;
      if (ss == null) {
        ss = [];
      }
      this.empty = __bind(this.empty, this);
      this.count = __bind(this.count, this);
      this.unsubscribe = __bind(this.unsubscribe, this);
      this.add = __bind(this.add, this);
      this.unsubscribed = false;
      this.subscriptions = [];
      this.starting = [];
      for (_i = 0, _len = ss.length; _i < _len; _i++) {
        s = ss[_i];
        this.add(s);
      }
    }

    CompositeUnsubscribe.prototype.add = function(subscription) {
      var ended, unsub, unsubMe,
        _this = this;
      if (this.unsubscribed) {
        return;
      }
      ended = false;
      unsub = nop;
      this.starting.push(subscription);
      unsubMe = function() {
        if (_this.unsubscribed) {
          return;
        }
        unsub();
        ended = true;
        _this.remove(unsub);
        return _.remove(subscription, _this.starting);
      };
      unsub = subscription(this.unsubscribe, unsubMe);
      if (!(this.unsubscribed || ended)) {
        this.subscriptions.push(unsub);
      }
      _.remove(subscription, this.starting);
      return unsub;
    };

    CompositeUnsubscribe.prototype.remove = function(subscription) {
      if (this.unsubscribed) {
        return;
      }
      return _.remove(subscription, this.subscriptions);
    };

    CompositeUnsubscribe.prototype.unsubscribe = function() {
      var s, _i, _len, _ref3;
      if (this.unsubscribed) {
        return;
      }
      this.unsubscribed = true;
      _ref3 = this.subscriptions;
      for (_i = 0, _len = _ref3.length; _i < _len; _i++) {
        s = _ref3[_i];
        s();
      }
      this.subscriptions = [];
      return this.starting = [];
    };

    CompositeUnsubscribe.prototype.count = function() {
      if (this.unsubscribed) {
        return 0;
      }
      return this.subscriptions.length + this.starting.length;
    };

    CompositeUnsubscribe.prototype.empty = function() {
      return this.count() === 0;
    };

    return CompositeUnsubscribe;

  })();

  Some = (function() {
    function Some(value) {
      this.value = value;
    }

    Some.prototype.getOrElse = function() {
      return this.value;
    };

    Some.prototype.get = function() {
      return this.value;
    };

    Some.prototype.filter = function(f) {
      if (f(this.value)) {
        return new Some(this.value);
      } else {
        return None;
      }
    };

    Some.prototype.map = function(f) {
      return new Some(f(this.value));
    };

    Some.prototype.forEach = function(f) {
      return f(this.value);
    };

    Some.prototype.isDefined = true;

    Some.prototype.toArray = function() {
      return [this.value];
    };

    return Some;

  })();

  None = {
    getOrElse: function(value) {
      return value;
    },
    filter: function() {
      return None;
    },
    map: function() {
      return None;
    },
    forEach: function() {},
    isDefined: false,
    toArray: function() {
      return [];
    }
  };

  Bacon.EventStream = EventStream;

  Bacon.Property = Property;

  Bacon.Observable = Observable;

  Bacon.Bus = Bus;

  Bacon.Initial = Initial;

  Bacon.Next = Next;

  Bacon.End = End;

  Bacon.Error = Error;

  nop = function() {};

  latterF = function(_, x) {
    return x();
  };

  former = function(x, _) {
    return x;
  };

  initial = function(value) {
    return new Initial(_.always(value));
  };

  next = function(value) {
    return new Next(_.always(value));
  };

  end = function() {
    return new End();
  };

  toEvent = function(x) {
    if (x instanceof Event) {
      return x;
    } else {
      return next(x);
    }
  };

  cloneArray = function(xs) {
    return xs.slice(0);
  };

  indexOf = Array.prototype.indexOf ? function(xs, x) {
    return xs.indexOf(x);
  } : function(xs, x) {
    var i, y, _i, _len;
    for (i = _i = 0, _len = xs.length; _i < _len; i = ++_i) {
      y = xs[i];
      if (x === y) {
        return i;
      }
    }
    return -1;
  };

  assert = function(message, condition) {
    if (!condition) {
      throw message;
    }
  };

  assertEvent = function(event) {
    return assert("not an event : " + event, event instanceof Event && event.isEvent());
  };

  assertEventStream = function(event) {
    return assert("not an EventStream : " + event, event instanceof EventStream);
  };

  assertFunction = function(f) {
    return assert("not a function : " + f, isFunction(f));
  };

  isFunction = function(f) {
    return typeof f === "function";
  };

  assertArray = function(xs) {
    return assert("not an array : " + xs, xs instanceof Array);
  };

  assertNoArguments = function(args) {
    return assert("no arguments supported", args.length === 0);
  };

  assertString = function(x) {
    return assert("not a string : " + x, typeof x === "string");
  };

  partiallyApplied = function(f, applied) {
    return function() {
      var args;
      args = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
      return f.apply(null, applied.concat(args));
    };
  };

  makeSpawner = function(f) {
    if (f instanceof Observable) {
      f = _.always(f);
    }
    assertFunction(f);
    return f;
  };

  makeFunctionArgs = function(args) {
    args = Array.prototype.slice.call(args);
    return makeFunction_.apply(null, args);
  };

  makeFunction_ = withMethodCallSupport(function() {
    var args, f;
    f = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    if (isFunction(f)) {
      if (args.length) {
        return partiallyApplied(f, args);
      } else {
        return f;
      }
    } else if (isFieldKey(f)) {
      return toFieldExtractor(f, args);
    } else {
      return _.always(f);
    }
  });

  makeFunction = function(f, args) {
    return makeFunction_.apply(null, [f].concat(__slice.call(args)));
  };

  isFieldKey = function(f) {
    return (typeof f === "string") && f.length > 1 && f.charAt(0) === ".";
  };

  Bacon.isFieldKey = isFieldKey;

  toFieldExtractor = function(f, args) {
    var partFuncs, parts;
    parts = f.slice(1).split(".");
    partFuncs = _.map(toSimpleExtractor(args), parts);
    return function(value) {
      var _i, _len;
      for (_i = 0, _len = partFuncs.length; _i < _len; _i++) {
        f = partFuncs[_i];
        value = f(value);
      }
      return value;
    };
  };

  toSimpleExtractor = function(args) {
    return function(key) {
      return function(value) {
        var fieldValue;
        if (value == null) {
          return void 0;
        } else {
          fieldValue = value[key];
          if (isFunction(fieldValue)) {
            return fieldValue.apply(value, args);
          } else {
            return fieldValue;
          }
        }
      };
    };
  };

  toFieldKey = function(f) {
    return f.slice(1);
  };

  toCombinator = function(f) {
    var key;
    if (isFunction(f)) {
      return f;
    } else if (isFieldKey(f)) {
      key = toFieldKey(f);
      return function(left, right) {
        return left[key](right);
      };
    } else {
      return assert("not a function or a field key: " + f, false);
    }
  };

  toOption = function(v) {
    if (v instanceof Some || v === None) {
      return v;
    } else {
      return new Some(v);
    }
  };

  _ = {
    head: function(xs) {
      return xs[0];
    },
    always: function(x) {
      return function() {
        return x;
      };
    },
    negate: function(f) {
      return function(x) {
        return !f(x);
      };
    },
    empty: function(xs) {
      return xs.length === 0;
    },
    tail: function(xs) {
      return xs.slice(1, xs.length);
    },
    filter: function(f, xs) {
      var filtered, x, _i, _len;
      filtered = [];
      for (_i = 0, _len = xs.length; _i < _len; _i++) {
        x = xs[_i];
        if (f(x)) {
          filtered.push(x);
        }
      }
      return filtered;
    },
    map: function(f, xs) {
      var x, _i, _len, _results;
      _results = [];
      for (_i = 0, _len = xs.length; _i < _len; _i++) {
        x = xs[_i];
        _results.push(f(x));
      }
      return _results;
    },
    each: function(xs, f) {
      var key, value, _results;
      _results = [];
      for (key in xs) {
        value = xs[key];
        _results.push(f(key, value));
      }
      return _results;
    },
    toArray: function(xs) {
      if (xs instanceof Array) {
        return xs;
      } else {
        return [xs];
      }
    },
    contains: function(xs, x) {
      return indexOf(xs, x) !== -1;
    },
    id: function(x) {
      return x;
    },
    last: function(xs) {
      return xs[xs.length - 1];
    },
    all: function(xs, f) {
      var x, _i, _len;
      if (f == null) {
        f = _.id;
      }
      for (_i = 0, _len = xs.length; _i < _len; _i++) {
        x = xs[_i];
        if (!f(x)) {
          return false;
        }
      }
      return true;
    },
    any: function(xs, f) {
      var x, _i, _len;
      if (f == null) {
        f = _.id;
      }
      for (_i = 0, _len = xs.length; _i < _len; _i++) {
        x = xs[_i];
        if (f(x)) {
          return true;
        }
      }
      return false;
    },
    without: function(x, xs) {
      return _.filter((function(y) {
        return y !== x;
      }), xs);
    },
    remove: function(x, xs) {
      var i;
      i = indexOf(xs, x);
      if (i >= 0) {
        return xs.splice(i, 1);
      }
    },
    fold: function(xs, seed, f) {
      var x, _i, _len;
      for (_i = 0, _len = xs.length; _i < _len; _i++) {
        x = xs[_i];
        seed = f(seed, x);
      }
      return seed;
    },
    cached: function(f) {
      var value;
      value = None;
      return function() {
        if (value === None) {
          value = f();
          f = null;
        }
        return value;
      };
    }
  };

  Bacon._ = _;

  Bacon.scheduler = {
    setTimeout: function(f, d) {
      return setTimeout(f, d);
    },
    setInterval: function(f, i) {
      return setInterval(f, i);
    },
    clearInterval: function(id) {
      return clearInterval(id);
    },
    now: function() {
      return new Date().getTime();
    }
  };

  if (typeof module !== "undefined" && module !== null) {
    module.exports = Bacon;
    Bacon.Bacon = Bacon;
  } else {
    if ((typeof define !== "undefined" && define !== null) && (define.amd != null)) {
      define([], function() {
        return Bacon;
      });
    }
    this.Bacon = Bacon;
  }

}).call(this);

},{}],"nLm5Ax":[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */



var Hogan = {};

(function (Hogan, useArrayBuffer) {
  Hogan.Template = function (renderFunc, text, compiler, options) {
    this.r = renderFunc || this.r;
    this.c = compiler;
    this.options = options;
    this.text = text || '';
    this.buf = (useArrayBuffer) ? [] : '';
  }

  Hogan.Template.prototype = {
    // render: replaced by generated code.
    r: function (context, partials, indent) { return ''; },

    // variable escaping
    v: hoganEscape,

    // triple stache
    t: coerceToString,

    render: function render(context, partials, indent) {
      return this.ri([context], partials || {}, indent);
    },

    // render internal -- a hook for overrides that catches partials too
    ri: function (context, partials, indent) {
      return this.r(context, partials, indent);
    },

    // tries to find a partial in the curent scope and render it
    rp: function(name, context, partials, indent) {
      var partial = partials[name];

      if (!partial) {
        return '';
      }

      if (this.c && typeof partial == 'string') {
        partial = this.c.compile(partial, this.options);
      }

      return partial.ri(context, partials, indent);
    },

    // render a section
    rs: function(context, partials, section) {
      var tail = context[context.length - 1];

      if (!isArray(tail)) {
        section(context, partials, this);
        return;
      }

      for (var i = 0; i < tail.length; i++) {
        context.push(tail[i]);
        section(context, partials, this);
        context.pop();
      }
    },

    // maybe start a section
    s: function(val, ctx, partials, inverted, start, end, tags) {
      var pass;

      if (isArray(val) && val.length === 0) {
        return false;
      }

      if (typeof val == 'function') {
        val = this.ls(val, ctx, partials, inverted, start, end, tags);
      }

      pass = (val === '') || !!val;

      if (!inverted && pass && ctx) {
        ctx.push((typeof val == 'object') ? val : ctx[ctx.length - 1]);
      }

      return pass;
    },

    // find values with dotted names
    d: function(key, ctx, partials, returnFound) {
      var names = key.split('.'),
          val = this.f(names[0], ctx, partials, returnFound),
          cx = null;

      if (key === '.' && isArray(ctx[ctx.length - 2])) {
        return ctx[ctx.length - 1];
      }

      for (var i = 1; i < names.length; i++) {
        if (val && typeof val == 'object' && names[i] in val) {
          cx = val;
          val = val[names[i]];
        } else {
          val = '';
        }
      }

      if (returnFound && !val) {
        return false;
      }

      if (!returnFound && typeof val == 'function') {
        ctx.push(cx);
        val = this.lv(val, ctx, partials);
        ctx.pop();
      }

      return val;
    },

    // find values with normal names
    f: function(key, ctx, partials, returnFound) {
      var val = false,
          v = null,
          found = false;

      for (var i = ctx.length - 1; i >= 0; i--) {
        v = ctx[i];
        if (v && typeof v == 'object' && key in v) {
          val = v[key];
          found = true;
          break;
        }
      }

      if (!found) {
        return (returnFound) ? false : "";
      }

      if (!returnFound && typeof val == 'function') {
        val = this.lv(val, ctx, partials);
      }

      return val;
    },

    // higher order templates
    ho: function(val, cx, partials, text, tags) {
      var compiler = this.c;
      var options = this.options;
      options.delimiters = tags;
      var text = val.call(cx, text);
      text = (text == null) ? String(text) : text.toString();
      this.b(compiler.compile(text, options).render(cx, partials));
      return false;
    },

    // template result buffering
    b: (useArrayBuffer) ? function(s) { this.buf.push(s); } :
                          function(s) { this.buf += s; },
    fl: (useArrayBuffer) ? function() { var r = this.buf.join(''); this.buf = []; return r; } :
                           function() { var r = this.buf; this.buf = ''; return r; },

    // lambda replace section
    ls: function(val, ctx, partials, inverted, start, end, tags) {
      var cx = ctx[ctx.length - 1],
          t = null;

      if (!inverted && this.c && val.length > 0) {
        return this.ho(val, cx, partials, this.text.substring(start, end), tags);
      }

      t = val.call(cx);

      if (typeof t == 'function') {
        if (inverted) {
          return true;
        } else if (this.c) {
          return this.ho(t, cx, partials, this.text.substring(start, end), tags);
        }
      }

      return t;
    },

    // lambda replace variable
    lv: function(val, ctx, partials) {
      var cx = ctx[ctx.length - 1];
      var result = val.call(cx);

      if (typeof result == 'function') {
        result = coerceToString(result.call(cx));
        if (this.c && ~result.indexOf("{\u007B")) {
          return this.c.compile(result, this.options).render(cx, partials);
        }
      }

      return coerceToString(result);
    }

  };

  var rAmp = /&/g,
      rLt = /</g,
      rGt = />/g,
      rApos =/\'/g,
      rQuot = /\"/g,
      hChars =/[&<>\"\']/;


  function coerceToString(val) {
    return String((val === null || val === undefined) ? '' : val);
  }

  function hoganEscape(str) {
    str = coerceToString(str);
    return hChars.test(str) ?
      str
        .replace(rAmp,'&amp;')
        .replace(rLt,'&lt;')
        .replace(rGt,'&gt;')
        .replace(rApos,'&#39;')
        .replace(rQuot, '&quot;') :
      str;
  }

  var isArray = Array.isArray || function(a) {
    return Object.prototype.toString.call(a) === '[object Array]';
  };

})(typeof exports !== 'undefined' ? exports : Hogan);




(function (Hogan) {
  // Setup regex  assignments
  // remove whitespace according to Mustache spec
  var rIsWhitespace = /\S/,
      rQuot = /\"/g,
      rNewline =  /\n/g,
      rCr = /\r/g,
      rSlash = /\\/g,
      tagTypes = {
        '#': 1, '^': 2, '/': 3,  '!': 4, '>': 5,
        '<': 6, '=': 7, '_v': 8, '{': 9, '&': 10
      };

  Hogan.scan = function scan(text, delimiters) {
    var len = text.length,
        IN_TEXT = 0,
        IN_TAG_TYPE = 1,
        IN_TAG = 2,
        state = IN_TEXT,
        tagType = null,
        tag = null,
        buf = '',
        tokens = [],
        seenTag = false,
        i = 0,
        lineStart = 0,
        otag = '{{',
        ctag = '}}';

    function addBuf() {
      if (buf.length > 0) {
        tokens.push(new String(buf));
        buf = '';
      }
    }

    function lineIsWhitespace() {
      var isAllWhitespace = true;
      for (var j = lineStart; j < tokens.length; j++) {
        isAllWhitespace =
          (tokens[j].tag && tagTypes[tokens[j].tag] < tagTypes['_v']) ||
          (!tokens[j].tag && tokens[j].match(rIsWhitespace) === null);
        if (!isAllWhitespace) {
          return false;
        }
      }

      return isAllWhitespace;
    }

    function filterLine(haveSeenTag, noNewLine) {
      addBuf();

      if (haveSeenTag && lineIsWhitespace()) {
        for (var j = lineStart, next; j < tokens.length; j++) {
          if (!tokens[j].tag) {
            if ((next = tokens[j+1]) && next.tag == '>') {
              // set indent to token value
              next.indent = tokens[j].toString()
            }
            tokens.splice(j, 1);
          }
        }
      } else if (!noNewLine) {
        tokens.push({tag:'\n'});
      }

      seenTag = false;
      lineStart = tokens.length;
    }

    function changeDelimiters(text, index) {
      var close = '=' + ctag,
          closeIndex = text.indexOf(close, index),
          delimiters = trim(
            text.substring(text.indexOf('=', index) + 1, closeIndex)
          ).split(' ');

      otag = delimiters[0];
      ctag = delimiters[1];

      return closeIndex + close.length - 1;
    }

    if (delimiters) {
      delimiters = delimiters.split(' ');
      otag = delimiters[0];
      ctag = delimiters[1];
    }

    for (i = 0; i < len; i++) {
      if (state == IN_TEXT) {
        if (tagChange(otag, text, i)) {
          --i;
          addBuf();
          state = IN_TAG_TYPE;
        } else {
          if (text.charAt(i) == '\n') {
            filterLine(seenTag);
          } else {
            buf += text.charAt(i);
          }
        }
      } else if (state == IN_TAG_TYPE) {
        i += otag.length - 1;
        tag = tagTypes[text.charAt(i + 1)];
        tagType = tag ? text.charAt(i + 1) : '_v';
        if (tagType == '=') {
          i = changeDelimiters(text, i);
          state = IN_TEXT;
        } else {
          if (tag) {
            i++;
          }
          state = IN_TAG;
        }
        seenTag = i;
      } else {
        if (tagChange(ctag, text, i)) {
          tokens.push({tag: tagType, n: trim(buf), otag: otag, ctag: ctag,
                       i: (tagType == '/') ? seenTag - ctag.length : i + otag.length});
          buf = '';
          i += ctag.length - 1;
          state = IN_TEXT;
          if (tagType == '{') {
            if (ctag == '}}') {
              i++;
            } else {
              cleanTripleStache(tokens[tokens.length - 1]);
            }
          }
        } else {
          buf += text.charAt(i);
        }
      }
    }

    filterLine(seenTag, true);

    return tokens;
  }

  function cleanTripleStache(token) {
    if (token.n.substr(token.n.length - 1) === '}') {
      token.n = token.n.substring(0, token.n.length - 1);
    }
  }

  function trim(s) {
    if (s.trim) {
      return s.trim();
    }

    return s.replace(/^\s*|\s*$/g, '');
  }

  function tagChange(tag, text, index) {
    if (text.charAt(index) != tag.charAt(0)) {
      return false;
    }

    for (var i = 1, l = tag.length; i < l; i++) {
      if (text.charAt(index + i) != tag.charAt(i)) {
        return false;
      }
    }

    return true;
  }

  function buildTree(tokens, kind, stack, customTags) {
    var instructions = [],
        opener = null,
        token = null;

    while (tokens.length > 0) {
      token = tokens.shift();
      if (token.tag == '#' || token.tag == '^' || isOpener(token, customTags)) {
        stack.push(token);
        token.nodes = buildTree(tokens, token.tag, stack, customTags);
        instructions.push(token);
      } else if (token.tag == '/') {
        if (stack.length === 0) {
          throw new Error('Closing tag without opener: /' + token.n);
        }
        opener = stack.pop();
        if (token.n != opener.n && !isCloser(token.n, opener.n, customTags)) {
          throw new Error('Nesting error: ' + opener.n + ' vs. ' + token.n);
        }
        opener.end = token.i;
        return instructions;
      } else {
        instructions.push(token);
      }
    }

    if (stack.length > 0) {
      throw new Error('missing closing tag: ' + stack.pop().n);
    }

    return instructions;
  }

  function isOpener(token, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].o == token.n) {
        token.tag = '#';
        return true;
      }
    }
  }

  function isCloser(close, open, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].c == close && tags[i].o == open) {
        return true;
      }
    }
  }

  Hogan.generate = function (tree, text, options) {
    var code = 'var _=this;_.b(i=i||"");' + walk(tree) + 'return _.fl();';
    if (options.asString) {
      return 'function(c,p,i){' + code + ';}';
    }

    return new Hogan.Template(new Function('c', 'p', 'i', code), text, Hogan, options);
  }

  function esc(s) {
    return s.replace(rSlash, '\\\\')
            .replace(rQuot, '\\\"')
            .replace(rNewline, '\\n')
            .replace(rCr, '\\r');
  }

  function chooseMethod(s) {
    return (~s.indexOf('.')) ? 'd' : 'f';
  }

  function walk(tree) {
    var code = '';
    for (var i = 0, l = tree.length; i < l; i++) {
      var tag = tree[i].tag;
      if (tag == '#') {
        code += section(tree[i].nodes, tree[i].n, chooseMethod(tree[i].n),
                        tree[i].i, tree[i].end, tree[i].otag + " " + tree[i].ctag);
      } else if (tag == '^') {
        code += invertedSection(tree[i].nodes, tree[i].n,
                                chooseMethod(tree[i].n));
      } else if (tag == '<' || tag == '>') {
        code += partial(tree[i]);
      } else if (tag == '{' || tag == '&') {
        code += tripleStache(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag == '\n') {
        code += text('"\\n"' + (tree.length-1 == i ? '' : ' + i'));
      } else if (tag == '_v') {
        code += variable(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag === undefined) {
        code += text('"' + esc(tree[i]) + '"');
      }
    }
    return code;
  }

  function section(nodes, id, method, start, end, tags) {
    return 'if(_.s(_.' + method + '("' + esc(id) + '",c,p,1),' +
           'c,p,0,' + start + ',' + end + ',"' + tags + '")){' +
           '_.rs(c,p,' +
           'function(c,p,_){' +
           walk(nodes) +
           '});c.pop();}';
  }

  function invertedSection(nodes, id, method) {
    return 'if(!_.s(_.' + method + '("' + esc(id) + '",c,p,1),c,p,1,0,0,"")){' +
           walk(nodes) +
           '};';
  }

  function partial(tok) {
    return '_.b(_.rp("' +  esc(tok.n) + '",c,p,"' + (tok.indent || '') + '"));';
  }

  function tripleStache(id, method) {
    return '_.b(_.t(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function variable(id, method) {
    return '_.b(_.v(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function text(id) {
    return '_.b(' + id + ');';
  }

  Hogan.parse = function(tokens, text, options) {
    options = options || {};
    return buildTree(tokens, '', [], options.sectionTags || []);
  },

  Hogan.cache = {};

  Hogan.compile = function(text, options) {
    // options
    //
    // asString: false (default)
    //
    // sectionTags: [{o: '_foo', c: 'foo'}]
    // An array of object with o and c fields that indicate names for custom
    // section tags. The example above allows parsing of {{_foo}}{{/foo}}.
    //
    // delimiters: A string that overrides the default delimiters.
    // Example: "<% %>"
    //
    options = options || {};

    var key = text + '||' + !!options.asString;

    var t = this.cache[key];

    if (t) {
      return t;
    }

    t = this.generate(this.parse(this.scan(text, options.delimiters), text, options), text, options);
    return this.cache[key] = t;
  };
})(typeof exports !== 'undefined' ? exports : Hogan);


},{}]},{},[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,53,55,56,50,57,58])
;