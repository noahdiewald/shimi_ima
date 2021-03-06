// # Dispatching double click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all double click events that are handled by the system are
// listed here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var dispatcher = require('dispatcher').dispatcher;
var panelToggler = require('panel-toggle').panelToggler;
var searchui = require('documents/searchui');
var ceditui = require('config/editui');
var worksheetui = require('documents/worksheetui');

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var dblclickDispatch = function (e) {
  'use strict';

  var action = dispatcher({
    '#edit-form span.span-title': function (t) {
      ceditui.toggle('title', t);
    },
    '#edit-form ol > li': function (t) {
      ceditui.toggle('array-elem', t);
    },
    '.search-result-field-id a': function (t) {
      searchui.addField(t.parentElement);
    },
    '.field-view b': function (t) {
      searchui.addField(t.parentElement);
    },
    '.field-container label span': function (t) {
      searchui.addField(t.parentElement.parentElement);
    },
    '#index-index-input-label': function () {
      searchui.addIndex();
    },
    '.panel > h2': function (t) {
      panelToggler(t);
    },
    '#toggle-handles': function (t) {
      worksheetui.hideHandles();
    },
    '.fieldset-handle': function (t) {
      worksheetui.hideFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t) {
      worksheetui.hideField($(t).attr('data-field-field'));
    }
  });

  action(e);
};

exports.dblclickDispatch = dblclickDispatch;
