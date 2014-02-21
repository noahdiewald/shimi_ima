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

var commandInput = function () {
  'use strict';

  return document.getElementById('edit-command-input');
};

var commandDialog = function () {
  'use strict';

  return $('#command-dialog');
};

var setContext = function (elem, context) {
  'use strict';

  return elem.attr('data-last-active', context);
};

var getContext = function (elem) {
  'use strict';

  return elem.attr('data-last-active');
};

// Exported functions

// Lookup the command and perform an action.
var execute = function (command) {
  'use strict';

  var restoreFocus = true;

  switch (command) {
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
    if ($('#document-delete-button').css('display') !== 'none') {
      $('#document-delete-button').click();
    }
    break;
  case 'e':
  case 'edit':
    $('#document-view').show();
    if ($('#document-edit-button').css('display') !== 'none') {
      $('#document-edit-button').click();
      restoreFocus = false;
    }
    break;
  case 'r':
  case 'restore':
    $('#document-view').show();
    if ($('#document-restore-button').css('display') !== 'none') {
      $('#document-restore-button').click();
    }
    break;
  }

  if (restoreFocus) {
    var cdialog = commandDialog();
    var context = getContext(cdialog);
    $('#' + context).focus();
  } else {
    S.sender('lost-focus');
  }

  S.sender('executed-command');
  return true;
};

// Open the command dialog
var dialogOpen = function (context) {
  'use strict';

  var cinput = commandInput();
  var cdialog = commandDialog();
  cinput.value = '';
  setContext(cdialog, context).show();
  cinput.focus();
  return true;
};

// Close the command dialog
var dialogClose = function () {
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
