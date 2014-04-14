// # Take actions depending on reported state.
//
// This is essentially an experiment in attempting to perform actions
// based on the state of the application. It is an idea that I'm still
// working on but the idea is to avoid having functions directly call
// other functions to initiate new actions but to instead simply report
// their state and have some central authority decide what to do next.
//
// The idea is to make something like this into a worker to achieve
// concurrency.

// Variable Definitions

var commands = require('./documents/commands.js');
var documents = require('./documents/documents.js');
var editui = require('./documents/editui.js');
var searchui = require('./documents/searchui.js');
var setsui = require('./documents/setsui.js');
var worksheetui = require('./documents/worksheetui.js');
var ceditui = require('./config/editui.js');
var cdoctypeui = require('./config/doctypeui.js');

// Exported functions

// This is called by functions when the actions they have performed
// result in a paticular state.
var sender = function (message, arg) {
  'use strict';

  var retval;

  switch (message) {
  case 'bad-session-state':
    retval = documents.clearSession();
    break;
  case 'doctype-info-ready':
    retval = documents.makeLabels();
    break;
  case 'labels-ready':
    documents.init2();
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
    // Config messages
  case 'doctypes-add':
    retval = cdoctypeui.addDoctype();
    break;
  case 'new-doctype-built':
    retval = ceditui.init(arg);
    break;
  case 'edit-doctype-requested':
    retval = ceditui.get(arg);
    break;
  case 'config-save':
    retval = ceditui.update();
    break;
  case 'config-create':
    retval = ceditui.create();
    break;
  case 'config-doctype-created':
  case 'config-doctype-deleted':
    ceditui.fresh();
    /* falls through */
  case 'config-doctype-updated':
    retval = cdoctypeui.init();
    break;
  case 'config-delete':
    retval = ceditui.remove();
    break;
  case 'config-move-up':
    retval = ceditui.elementUp();
    break;
  case 'config-move-down':
    retval = ceditui.elementDown();
    break;
  case 'config-remove-element':
    retval = ceditui.elementDelete();
    break;
  case 'config-add-text':
    retval = ceditui.addTextElement();
    break;
  case 'config-add-array':
    retval = ceditui.addArrayElement();
    break;
  case 'config-add-object':
    retval = ceditui.addObjectElement();
    break;
  case 'config-add-child-text':
    retval = ceditui.addChildTextElement();
    break;
  case 'config-add-child-array':
    retval = ceditui.addChildArrayElement();
    break;
  case 'config-add-child-object':
    retval = ceditui.addChildObjectElement();
    break;
  case 'config-clear-form':
    retval = ceditui.init();
    break;
  case 'config-copy':
    retval = ceditui.copy();
    break;
  case 'config-cut':
    retval = ceditui.cut();
    break;
  case 'config-paste':
    retval = ceditui.paste();
    break;
  case 'config-paste-child':
    retval = ceditui.pasteChild();
    break;
  case 'config-promote':
    retval = ceditui.promote();
    break;
  case 'config-demote':
    retval = ceditui.demote();
    break;
  case 'config-mark-line':
    retval = ceditui.markLine(arg);
    break;
  }

  return retval;
};

exports.sender = sender;
