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