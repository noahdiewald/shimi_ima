;(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
// # The Client Code Entry Point
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// This is the entry point for the client side code. This is where
// basic initializations take place and helper functions are added to
// JavaScript Objects. The 'onload' code is here.

// ## Variable Definitions

// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
var shimi = {};
shimi.globals = {};

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
  if ($('#configuration').length > 0)
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

},{"./changes.js":2,"./click-dispatch.js":3,"./config/config.js":7,"./dblclick-dispatch.js":15,"./documents/documents.js":19,"./file_manager/fm.js":27,"./form.js":29,"./index_tool/ilistingui.js":33,"./jquery-ui-input-state.js":37,"./keystrokes.js":39,"./projects/projectui.js":43}],2:[function(require,module,exports){
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

},{"./documents/searchui.js":23}],3:[function(require,module,exports){
// # Dispatching click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all click events that are handled by the system are listed
// here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var sender = require('./sender.js').sender;
var dispatcher = require('./dispatcher.js').dispatcher;
var panelToggler = require('./panel-toggle.js').panelToggler;
var doctypeTab = require('./config/doctype-tab.js').doctypeTab;
var charseqTab = require('./config/charseq-tab').charseqTab;
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
var config = require('./config/config.js');

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
      config.upgradeButton(t);
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
      sender('new-set-form-submit');
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

},{"./config/charseq-tab":6,"./config/config.js":7,"./config/doctype-tab.js":10,"./dispatcher.js":16,"./documents/editui.js":20,"./documents/fieldsets.js":21,"./documents/indexui.js":22,"./documents/searchui.js":23,"./documents/setsui.js":24,"./documents/viewui.js":25,"./documents/worksheetui.js":26,"./file_manager/fm.js":27,"./form.js":29,"./index_tool/ieditui.js":31,"./panel-toggle.js":41,"./projects/projectui.js":43,"./sender.js":44}],4:[function(require,module,exports){
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

},{"../form.js":29,"./charseq-elems.js":5,"./charseq-tab.js":6}],5:[function(require,module,exports){
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

},{"../form.js":29}],6:[function(require,module,exports){
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

},{"../form.js":29,"../store.js":47,"./charseq-dialog.js":4,"./charseq-elems.js":5}],7:[function(require,module,exports){
// # Config Sub-App Init
//
// *Implicit depends:* DOM, JQuery
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeTab = require('./doctype-tab.js').doctypeTab;
var charseqTab = require('./charseq-tab.js').charseqTab;

// ## Exported Functions

// When the upgrade button is pressed in the configuration UI, this
// will carry out the necessary action. It will make an empty `POST`
// to the upgrade path and alert the user that this was done.
//
// TODO: This is basically here until the upgrad functionality is fleshed
// out and gets its own module.
var upgradeButton = function ()
{
  'use strict';

  $.post('config/upgrade');
  window.alert('Upgrade In Progress');
};

// Run initialization code for the configuration sub-application.
var init = function ()
{
  'use strict';

  doctypeTab.init();
  $('#main-tabs').tabs();
  charseqTab.init();
  $('.simple-tabs').tabs();

  return true;
};

exports.upgradeButton = upgradeButton;
exports.init = init;

},{"./charseq-tab.js":6,"./doctype-tab.js":10}],8:[function(require,module,exports){
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

},{"./doctype-elems.js":9,"./doctype-tab.js":10}],9:[function(require,module,exports){
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

},{"../form.js":29}],10:[function(require,module,exports){
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

// Exported functions

// Object containing initialization and other functions.
var doctypeTab = (function ()
{
  'use strict';

  var mod = {};

  var cpath = function (source, category)
  {
    return path(source, category, 'config');
  };

  // Populate the listing of fields
  mod.initFields = function (path)
  {
    path.field = false;

    $.get(path.toString(), function (fields)
    {
      var fieldContainer = $('#fields-' + path.fieldset);
      fieldContainer.empty();
      fieldContainer.html(fields);
    });

    return mod;
  };

  // Populate the listing of fieldsets
  mod.initFieldsets = function (url)
  {
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
  mod.init = function ()
  {
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
        mod.initFieldsets(fieldsetsPath);
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
  mod.editField = function (target)
  {
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
  mod.deleteField = function (target)
  {
    var answer = window.confirm('Are you sure? This is permanent.');

    if (answer)
    {
      var url = cpath(target, 'field');
      var complete = function ()
      {
        url.field = false;
        url.rev = false;

        mod.initFields(url);
      };
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a field
  mod.addField = function (target)
  {
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
  mod.editFieldset = function (target)
  {
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
  mod.deleteFieldset = function (target)
  {
    var url = cpath(target, 'fieldset');

    var complete = function ()
    {
      url.fieldset = false;
      url.rev = false;
      mod.initFieldsets(url);
    };

    if (window.confirm('Are you sure? This is permanent.'))
    {
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a fieldset
  mod.addFieldset = function (target)
  {
    var url = cpath(target, 'fieldset');
    fieldsetDialog(url,
    {
      doctype: url.doctype
    }).dialog('open');
  };

  mod.editDoctype = function (target)
  {
    var url = cpath(target, 'doctype');
    var oldobj = {};
    var attrs = doctypeElems.attrs;

    attrs.forEach(function (item)
    {
      oldobj[item] = store(target).get('doctype-' + item);
    });
    doctypeDialog(url, oldobj).dialog('open');
  };

  mod.touchDoctype = function (target)
  {
    var docid = store(target).get('doctype-doctype');
    $.post('config/doctypes/' + docid + '/touch');
    window.alert('Touch In Progress');
  };

  mod.deleteDoctype = function (target)
  {
    var url = cpath(target, 'doctype');
    var complete = function ()
    {
      url.doctype = false;
      url.rev = false;
      mod.init();
    };

    if (window.confirm('Are you sure? This is permanent.'))
    {
      url.del(complete, this);
    }
  };

  mod.addDoctype = function (target)
  {
    var url = cpath(target, 'doctype');
    doctypeDialog(url,
    {}).dialog('open');
  };

  return mod;
})();

exports.doctypeTab = doctypeTab;

},{"../path.js":42,"../store.js":47,"./doctype-dialog.js":8,"./doctype-elems.js":9,"./field-dialog.js":11,"./field-elems.js":12,"./fieldset-dialog.js":13,"./fieldset-elems.js":14}],11:[function(require,module,exports){
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

},{"./doctype-tab.js":10,"./field-elems.js":12}],12:[function(require,module,exports){
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

},{"../form.js":29,"../utils.js":48}],13:[function(require,module,exports){
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

},{"./doctype-tab.js":10,"./fieldset-elems.js":14}],14:[function(require,module,exports){
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

},{"../form.js":29}],15:[function(require,module,exports){
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

},{"./dispatcher.js":16,"./documents/searchui.js":23,"./documents/worksheetui.js":26,"./panel-toggle.js":41}],16:[function(require,module,exports){
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
},{}],17:[function(require,module,exports){
// # Paging For Changes Listing
//
// *Implicit depends:* DOM, JQuery
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

  var prefix = prefix();
  var url = prefix;
  var target = $('#' + prefix + '-listing');

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
    prefix: 'changelog',
    url: url,
    format: format,
    target: target
  }).get();

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"../pager.js":40}],18:[function(require,module,exports){
// # Keyboard shortcuts
//
// *Implicit depends:* DOM, JQuery
//
// Handles the input area and command execution. Keyboard events are
// handled in [keystrokes.js](./keystrokes.html).

// Variable Definitions

var editui = require('./editui.js');
var sender = require('../sender.js').sender;

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
    sender('lost-focus');
  }

  sender('executed-command');
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

},{"../sender.js":44,"./editui.js":20}],19:[function(require,module,exports){
// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// Variable Definitions

var setsui = require('./setsui.js');
var editui = require('./editui.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var changeui = require('./changeui.js');
var sender = require('../sender.js').sender;
var store = require('../store.js').store;
var identifier;

// Internal functions

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

var allDocContainer = function ()
{
  'use strict';

  return $('#all-document-container');
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
  sender('doctype-info-ready');

  return true;
};

// Exported functions

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

// Reset the doctype version
var setVersion = function ()
{
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  sender('version-set');

  return true;
};

// Clear the session storage
var clearSession = function ()
{
  'use strict';

  sessionStorage.clear();
  sender('session-cleared');

  return true;
};

// Check if the doctype version stored is current and report the current
// state based on the result.
var checkVersion = function ()
{
  'use strict';

  if (isCurrentVersionStored())
  {
    sender('labels-ready');
  }
  else
  {
    sender('bad-session-state');
  }

  return true;
};

// Get the doctype name
var dname = function ()
{
  'use strict';

  return store($('#all-document-container')).d('doctype');
};

// Get the project id
var project = function ()
{
  'use strict';

  return store($('#container')).get('project-id');
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

  var info = info();
  var labels = {};

  info.fieldsets.forEach(function (fieldset)
  {
    fieldset.fields.forEach(function (field)
    {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));
  sender('labels-ready');

  return true;
};

// Initialize the documents sub-application.
var init = function ()
{
  'use strict';

  $('form').on('submit', function ()
  {
    return false;
  });
  checkVersion();
  setsui.updateSelection();
  indexui.iOpts();
  indexui.get();
  indexForm();
  editui.init();
  loadHash($(location)[0].hash.split('#')[1]);
  changeui.get();
};

exports.getVersion = getVersion;
exports.getCurrentVersion = getCurrentVersion;
exports.isCurrentVersionStored = isCurrentVersionStored;
exports.setVersion = setVersion;
exports.clearSession = clearSession;
exports.checkVersion = checkVersion;
exports.dname = dname;
exports.project = project;
exports.identifier = identifier;
exports.info = info;
exports.loadDoctype = loadDoctype;
exports.makeLabels = makeLabels;
exports.init = init;

},{"../sender.js":44,"../store.js":47,"./changeui.js":17,"./editui.js":20,"./indexui.js":22,"./setsui.js":24,"./viewui.js":25}],20:[function(require,module,exports){
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

},{"../flash.js":28,"../form.js":29,"../store.js":47,"./fieldsets.js":21,"./indexui.js":22,"./viewui.js":25}],21:[function(require,module,exports){
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

},{"../path.js":42,"../store.js":47,"../utils.js":48,"./editui.js":20}],22:[function(require,module,exports){
// # Paging For Index Listing
//
// *Implicit depends:* DOM, JQuery
//
// Loads index based on user suplied values. It also loads some other
// preliminary data, such as the listing of user created indexes. The
// `load()` function performs some initialization.

// Variable Definitions
var pager = require('../pager.js').pager;

// Exported Functions

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

  var prefix = prefix();
  var url = 'documents/' + prefix;
  var indexId = $('#index-' + prefix + '-input').val();
  var target = $('#' + prefix + '-listing');

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
    prefix: prefix,
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
    options = templates['index-options'].render(data);
    $('#index-index-input').html(options);
  });

  return true;
};

// This is the entry point that loads the data for this section of
// the application.
var load = function (target)
{
  'use strict';

  var id = $(target).attr('href').slice(1);
  $('#document-view').html('<em>Loading...</em>');
  shimi.editui.clear();
  shimi.viewui.get(id);

  return true;
};

exports.prefix = prefix;
exports.get = get;
exports.iOpts = iOpts;
exports.load = load;

},{"../pager.js":40}],23:[function(require,module,exports){
// # The search user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the search user interface.

// Variable Definitions

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

  return templates['search-field-item'].render(
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

},{"../sets.js":46,"../utils.js":48,"./documents.js":19,"./setsui.js":24}],24:[function(require,module,exports){
// # The sets user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the sets user interface.

// Variable Definitions

var sender = require('../sender.js').sender;
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
  sender('sets-changed');

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
  var listing = templates['set-listing'].render(
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
  var newOptions = templates['set-options'].render(
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
    sender('sets-changed');
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

},{"../flash.js":28,"../sender.js":44,"../sets.js":46,"../utils.js":48,"./documents.js":19}],25:[function(require,module,exports){
// # The view user interface
//
// *Implicit depends:* DOM, JQuery, Hogan, templates
//
// View pane UI elements.
//
// *TODO* I may be exporting more than needed.

// Variable Definitions

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
      return templates['document-view-tree'].render(docJson,
      {
        'document-view-field': templates['document-view-field']
      });
    };
  }
  else
  {
    tmpl = function (docJson)
    {
      return templates['document-view'].render(docJson,
      {
        'document-view-tree': templates['document-view-tree'],
        'document-view-field': templates['document-view-field']
      });
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

},{"../flash.js":28,"../store.js":47,"./editui.js":20,"./fieldsets.js":21,"./indexui.js":22}],26:[function(require,module,exports){
// # The worksheet user interface
//
// *Implicit depends:* DOM, JQuery, Hogan, templates, shimi.globals
// ([application.js](./application.html))
//
// Worksheet pane UI elements.

// Variable Definitions

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
  var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'].render(doctypeInfo);
  shimi.globals[worksheetName()] = Hogan.compile(metaTemp);

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
    var ws = shimi.globals[worksheetName()].render(data);
    worksheetsArea().html(ws);
  };

  if (!setName.isBlank())
  {
    var thisSet = setsui.getSet(setName)[1];

    if (thisSet.lenght <= 250)
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

},{"../flash.js":28,"../form.js":29,"./documents.js":19,"./setsui.js":24}],27:[function(require,module,exports){
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

},{"../flash.js":28,"../form.js":29}],28:[function(require,module,exports){
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

},{}],29:[function(require,module,exports){
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

},{"./flash.js":28}],30:[function(require,module,exports){
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
  var evs = ihelpers.evs();

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

},{"../form.js":29,"../jquery-ui-input-state.js":37,"./ihelpers.js":32}],31:[function(require,module,exports){
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

},{"../flash.js":28,"../form.js":29,"./builder-dialog.js":30,"./ihelpers.js":32,"./ilistingui.js":33,"./ipreviewui.js":34,"./new-dialog.js":35,"./replace-dialog.js":36}],32:[function(require,module,exports){
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

  var mod;

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

},{"../sess.js":45}],33:[function(require,module,exports){
// # Index listing.
//
// *Implicit depends:* DOM, JQuery, Hogan, templates
//
// Displays a listing of user created indexes.

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
    listing = templates['index-listing'].render(data);
    target.html(listing);
  });

  return true;
};

exports.init = init;

},{}],34:[function(require,module,exports){
// # Paging For Index Listing
//
// *Implicit depends:* DOM, JQuery
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

  var prefix = prefix();
  var indexId = $('#index-editing-data').attr('data-index-id');
  var url = 'indexes/' + indexId + '/preview';
  var target = $('#' + prefix + '-list-view');

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
      prefix: prefix,
      format: format,
      url: url,
      target: target
    }).get();//.get(startkey, startid, prevkeys, previds);
  }

  return true;
};

exports.prefix = prefix;
exports.get = get;

},{"../pager.js":40}],35:[function(require,module,exports){
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
  var evs = ihelpers.evs();

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

},{"../form.js":29,"../jquery-ui-input-state.js":37,"./ihelpers.js":32,"./ilistingui.js":33}],36:[function(require,module,exports){
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

},{"../form.js":29,"./ihelpers.js":32}],37:[function(require,module,exports){
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

},{}],38:[function(require,module,exports){
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

},{}],39:[function(require,module,exports){
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
var sender = require('./sender.js');
var ipreviewui = require('./index_tool/ipreviewui.js');
var indexui = require('./documents/indexui.js');
var changeui = require('./documents/changeui.js');
var editui = require('./documents/editui.js');
var viewui = require('./documents/viewui.js');
var searchui = require('./documents/searchui.js');

// # Exported Functions

// All this does is register a bunch of event handlers.
var keystrokes = function ()
{
  'use strict';

  [ipreviewui, indexui, changeui].forEach(function (mod)
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
      sender('worksheet-form-submit');
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-sets-form', function (e)
  {
    if (e.which === 13)
    {
      sender('sets-form-submit');
      return false;
    }
    return true;
  });

  $('#new-set-form').on('keydown', function (e)
  {
    if (e.which === 13)
    {
      sender('new-set-form-submit');
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
      sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', 0);
      sender('lost-focus');
    }

    return false;
  });

  $(document).bind('keydown', 'Alt+c', function (e)
  {
    var active = $(document.activeElement).attr('id');
    sender('initiated-command', active);
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
      sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', totaltabs - 1);
      sender('lost-focus');
    }

    return false;
  });


  $(document).on('keydown', '#edit-command-input', function (e)
  {
    if (e.which === 13)
    {
      var command = $('#edit-command-input').val();
      sender('submitted-command', command);
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

},{"./documents/changeui.js":17,"./documents/editui.js":20,"./documents/indexui.js":22,"./documents/searchui.js":23,"./documents/viewui.js":25,"./index_tool/ipreviewui.js":34,"./jquery.hotkeys.js":38,"./sender.js":44}],40:[function(require,module,exports){
// # Paging List-like Info
//
// *Implicit depends:* DOM, JQuery
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
    return $('#' + prefix + '-limit');
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
    var limit = limitField().val() * 1;
    // Where the next page will be displayed.
    var target = args.target;
    // The filter is used to constrain the values listed.
    var filterVal = $('#' + prefix + '-filter').val();
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
      limitField().val(25);
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
    var limit = limitField().val() * 1;

    var respJSON;
    var lastrow;
    var newRows;

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

    target.html(templates['paged-listing'].render(respJSON,
    {
      'listed-element': templates[prefix + '-element']
    }));

    $('#previous-' + prefix + '-page').click(function ()
    {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    });

    $('#next-' + prefix + '-page').click(function ()
    {
      var nextkey = $('#next-' + prefix + '-page').attr('data-startkey');
      var nextid = $('#next-' + prefix + '-page').attr('data-startid');
      var prevkey = $('#first-' + prefix + '-element').attr('data-first-key');
      var previd = $('#first-' + prefix + '-element').attr('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    });

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0)
    {
      $('#previous-' + prefix + '-page').hide();
    }

    // Disable the next button if we're at the end
    if ($('#next-' + prefix + '-page').attr('data-last-page'))
    {
      $('#next-' + prefix + '-page').hide();
    }

    return mod;
  };

  return mod;
};

exports.pager = pager;

},{"./form.js":29}],41:[function(require,module,exports){
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

},{}],42:[function(require,module,exports){
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
    shimi.form.send(mod.toString(), object, method, callback, context);
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

},{"./store.js":47}],43:[function(require,module,exports){
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

},{"../form.js":29}],44:[function(require,module,exports){
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

  switch (message)
  {
  case 'bad-session-state':
    documents.clearSession();
    break;
  case 'doctype-info-ready':
    documents.makeLabels();
    break;
  case 'labels-ready':
    searchui.loadSearchVals();
    worksheetui.buildTemplate();
    break;
  case 'new-set-form-submit':
    setsui.saveSelected();
    break;
  case 'sets-changed':
    setsui.updateSelection();
    break;
  case 'sets-form-submit':
    setsui.performOp();
    break;
  case 'session-cleared':
    documents.setVersion();
    documents.loadDoctype();
    break;
  case 'worksheet-form-submit':
    worksheetui.fillWorksheet();
    break;
  case 'initiated-command':
    commands.dialogOpen(arg);
    break;
  case 'executed-command':
    commands.dialogClose();
    break;
  case 'submitted-command':
    commands.execute(arg);
    break;
  case 'lost-focus':
    editui.selectInput();
    break;
  }

  return false;
};

exports.sender = sender;
},{"./documents/commands.js":18,"./documents/documents.js":19,"./documents/editui.js":20,"./documents/searchui.js":23,"./documents/setsui.js":24,"./documents/worksheetui.js":26}],45:[function(require,module,exports){
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

},{}],46:[function(require,module,exports){
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

},{}],47:[function(require,module,exports){
// # Data Attribute Storage and Retrieval Helpers
//
// It is likely that this mechanism will be replaced with a superior
// mechanism for storing data on the client about documents.

// ## Internal functions

// Identity function for tail recursion
var identity = function (x)
{
  'use strict';

  return x;
};

// Part of the tail call optimization code
Function.prototype.r = function ()
{
  'use strict';

  return [this, arguments];
};

// Tail call optimization taken from Spencer Tipping's Javascript in Ten
// Minutes.
//
// For more information see:
// <https://github.com/spencertipping/js-in-ten-minutes>
Function.prototype.t = function ()
{
  'use strict';

  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape)
  {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
};

// ## External functions

// Takes a JQuery element and returns an object with helper methods for
// getting and putting custom data attribute values.
var store = function (elem)
{
  'use strict';

  var mod = {};

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
  //     store($('#thisid')).get('fieldset-doctype') == 'did';
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
  //     store($('#thisid')).get('fieldset-doctype') == 'did';
  mod.get = function (key)
  {
    var prelim = elem.attr('data-' + key);

    if (prelim)
    {
      return prelim;
    }

    var getValue1 = function (key, elem, id)
    {
      var gid = elem.attr('data-group-id');
      var store = $('#' + gid);
      var val = store.attr('data-' + key);
      var next = store.attr('data-group-id');

      if (val === undefined && next !== undefined && gid !== next)
      {
        return getValue1.r(key, store, id);
      }

      return id.r(val);
    };

    return getValue1.t(key, elem, identity);
  };

  // Like 'get' but will decode base64 encoded values.
  mod.get64 = function (key)
  {
    var retval = mod.get(key);
    retval = shimi.utils().Base64.decode(retval.replace(/'/g, '')).replace(/(^'|'$)/g, '');
    return retval;
  };

  //  This function will set an attribute at the target with a name
  //  corresponding to key and a value of value.
  mod.put = function (key, value)
  {
    var dataElem = elem.attr('data-group-id');
    $('#' + dataElem).attr('data-' + key, value);
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

},{}],48:[function(require,module,exports){
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

},{}]},{},[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48])
;