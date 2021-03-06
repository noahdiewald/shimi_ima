// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// ## Variable Definitions

var ui = require('documents/ui-shared');
var info = require('documents/information');
var setsui = require('documents/setsui');
var editui = require('./editui.js');
var viewui = require('documents/viewui');
var changeui = require('documents/changeui');
var S = require('sender');
var ajax = require('ajax');
var identifier;

// ## Internal functions

// If there is a hash at the end of the URL with a document ID specified,
// this will pass the information on the correct funciont in `viewui`.
var loadHash = function (urlHash) {
  'use strict';

  if (urlHash) {
    viewui.get(urlHash);
  }

  return true;
};

// ## Exported functions

// Initialize the documents sub-application.
var init = function () {
  'use strict';

  // TODO: there should be a better place for this.
  document.onsubmit = function () {
    return false;
  };

  S.sender('document-init-stage-1');

  return true;
};

// Initialization dependent on init.
var init2 = function () {
  'use strict';

  setsui.updateSelection();
  S.sender('documents-ready');
  editui.init();
  loadHash(window.location.hash.split('#')[1]);
  changeui.get();
};

exports.init = init;
exports.init2 = init2;
