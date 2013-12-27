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
  case 'new-doctype-requested':
    retval = cdoctypeui.addDoctype();
    break;
  case 'new-doctype-built':
    retval = ceditui.init(arg);
    break;
  case 'edit-doctype-requested':
    retval = ceditui.get(arg);
    break;
  case 'save-config-document-request':
    retval = ceditui.update();
    break;
  case 'create-config-document-request':
    retval = ceditui.create();
    break;
  case 'config-doctype-created':
  case 'config-doctype-deleted':
  case 'config-doctype-updated':
    retval = cdoctypeui.init();
    break;
  case 'delete-config-document-request':
    retval = ceditui.remove();
    break;
  case 'move-config-element-up-request':
    retval = ceditui.elementUp(arg);
    break;
  case 'move-config-element-down-request':
    retval = ceditui.elementDown(arg);
    break;
  }

  return retval;
};

exports.sender = sender;
