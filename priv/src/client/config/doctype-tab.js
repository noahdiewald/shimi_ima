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

var cpath = function (source, category) {
  'use strict';

  return path(source, category, 'config');
};

// Exported functions

// Populate the listing of fields
var initFields = function (path) {
  'use strict';

  path.field = false;

  $.get(path.toString(), function (fields) {
    var fieldContainer = $('#fields-' + path.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
  });

  return true;
};

// Populate the listing of fieldsets
var initFieldsets = function (url) {
  'use strict';

  $.get(url.toString(), function (fieldsets) {
    var fieldsetContainer = $('#fieldsets-' + url.doctype);

    fieldsetContainer.empty();
    fieldsetContainer.accordion();
    fieldsetContainer.accordion('destroy');
    fieldsetContainer.html(fieldsets);

    fieldsetContainer.accordion({
      autoHeight: false,
      collapsible: true,
      active: false
    });
  });
};

// populate the tabs listing the doctypes
var init = function () {
  'use strict';

  var url = 'config/doctypes';

  $('#doctype-tabs').tabs();

  $.get(url, function (doctypes) {
    var fieldsetDoctype = $('#fieldset-doctype-input');

    $('#doctype-tabs-headings').empty();
    $('#doctype-tabs-headings + .ui-tabs-panel').remove();
    $('#doctype-tabs').tabs('destroy');
    $('#doctype-tabs-headings').html(doctypes);

    var loadFun = function (event, ui) {
      var source = $(ui.panel).children('div[data-fieldset-doctype]');
      var fieldsetsPath = path(source, 'fieldset', 'config');
      initFieldsets(fieldsetsPath);
    };

    $('#doctype-tabs').tabs({
      load: function (e, ui) {
        loadFun(e, ui);
      }
    });
  });
};

// Button that opens a dialog for editing a field
var editField = function (target) {
  'use strict';

  var url = cpath(target, 'field');
  var oldobj = {};
  var attrs = fieldElems.attrs;
  var charseqUrl = 'config/charseqs?as=options';

  $.get(charseqUrl, function (charseqs) {
    $('#field-charseq-input').html(charseqs);
    attrs.forEach(function (item) {
      oldobj[item] = store(target).get('field-' + item);
    });
    fieldDialog(url, oldobj).dialog('open');
  });
};

// Button that opens a dialog for deleting a field
var deleteField = function (target) {
  'use strict';

  var answer = window.confirm('Are you sure? This is permanent.');

  if (answer) {
    var url = cpath(target, 'field');
    var complete = function () {
      url.field = false;
      url.rev = false;

      initFields(url);
    };
    url.del(complete, this);
  }
};

// Button that opens a dialog for adding a field
var addField = function (target) {
  'use strict';

  var url = cpath(target, 'field');
  var charseqUrl = 'config/charseqs?as=options';

  $.get(charseqUrl, function (charseqs) {
    $('#field-charseq-input').html(charseqs);
    fieldDialog(url, {
      fieldset: url.fieldset,
      doctype: url.doctype
    }).dialog('open');
  });
};

// Button that opens a dialog for editing a fieldset
var editFieldset = function (target) {
  'use strict';

  var url = cpath(target, 'fieldset');
  var oldobj = {};
  var attrs = fieldsetElems.attrs;

  attrs.forEach(function (item) {
    oldobj[item] = store(target).get('fieldset-' + item);
  });

  fieldsetDialog(url, oldobj).dialog('open');
};

// Button that opens a dialog for deleting a fieldset
var deleteFieldset = function (target) {
  'use strict';

  var url = cpath(target, 'fieldset');

  var complete = function () {
    url.fieldset = false;
    url.rev = false;
    initFieldsets(url);
  };

  if (window.confirm('Are you sure? This is permanent.')) {
    url.del(complete, this);
  }
};

// Button that opens a dialog for adding a fieldset.
var addFieldset = function (target) {
  'use strict';

  var url = cpath(target, 'fieldset');
  fieldsetDialog(url, {
    doctype: url.doctype
  }).dialog('open');
};

// Button that opens a dialog for editing a doctype.
var editDoctype = function (target) {
  'use strict';

  var url = cpath(target, 'doctype');
  var oldobj = {};
  var attrs = doctypeElems.attrs;

  attrs.forEach(function (item) {
    oldobj[item] = store(target).get('doctype-' + item);
  });
  doctypeDialog(url, oldobj).dialog('open');
};

// Button for initiating the touch operation.
var touchDoctype = function (target) {
  'use strict';

  var docid = store(target).get('doctype-doctype');
  $.post('config/doctypes/' + docid + '/touch');
  window.alert('Touch In Progress');
};

// Button for deleting a doctype.
var deleteDoctype = function (target) {
  'use strict';

  var url = cpath(target, 'doctype');
  var complete = function () {
    url.doctype = false;
    url.rev = false;
    init();
  };

  if (window.confirm('Are you sure? This is permanent.')) {
    url.del(complete, this);
  }
};

// Button for adding a doctype.
var addDoctype = function (target) {
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
