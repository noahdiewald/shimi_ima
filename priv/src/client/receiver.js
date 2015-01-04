// # Take actions depending on reported state.
//
// This provides the onmessage function for the `reporter.js` worker. See
// also `sender.js`.

// ## Variable Definitions

var commands = require('documents/commands');
var documents = require('documents/documents');
var dinfo = require('documents/information');
var editui = require('documents/editui');
var dindexui = require('documents/indexui');
var searchui = require('documents/searchui');
var setsui = require('documents/setsui');
var worksheetui = require('documents/worksheetui');
var ceditui = require('config/editui');
var cdoctypeui = require('config/doctypeui');

// ## External Functions

var receiver = function (message, arg) {
  'use strict';

  var retval;

  switch (message) {
  case 'documents-ready':
    dindexui.iOpts();
    retval = dindexui.get();
    break;
  case 'documents-altered':
    //retval = dindexui.get(arg[0], arg[1]);
    retval = dindexui.get();
    break;
  case 'document-init-stage-1':
    retval = dinfo.checkState();
    break;
  case 'bad-session-state':
    retval = dinfo.clearSession();
    break;
  case 'doctype-info-ready':
    retval = dinfo.makeFieldsetLookup();
    break;
  case 'fieldset-lookup-ready':
    retval = dinfo.makeLabels();
    break;
  case 'doctype-cached-info-ready':
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
    dinfo.setVersion();
    retval = dinfo.loadDoctype();
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

exports.receiver = receiver;
