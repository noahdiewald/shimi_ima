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
var clickDispatch = function (e) {
  'use strict';

  var action = dispatcher({
    // ### Config

    '.edit-doctype-link': function (t) {
      return S.sender('edit-doctype-requested', 'doctypes/' + t.getAttribute('href').slice(1));
    },
    '.touch-doctype-button': function (t) {
      doctypeTab.touchDoctype(t);
    },
    '#doctypes-add-button': function () {
      return S.sender('new-doctype-requested');
    },
    '.delete-charseq-button': function (t) {
      charseqTab.del(t);
    },
    '#charseq-add-button': function (t) {
      charseqTab.add();
    },
    '#maintenance-upgrade-button': function (t) {
      return maintenanceui.upgradeButton(t);
    },
    '#config-save-button': function () {
      return S.sender('save-config-document-request');
    },
    '#config-delete-button': function () {
      return S.sender('delete-config-document-request');
    },
    '#config-create-button': function () {
      return S.sender('create-config-document-request');
    },
    '#config-edit a.up': function (t) {
      return S.sender('move-config-element-up-request', t.dataset.target);
    },
    '#config-edit a.down': function (t) {
      return S.sender('move-config-element-down-request', t.dataset.target);
    },
    '#config-edit a.delete': function (t) {
      return S.sender('config-element-delete-request', t.dataset.target);
    },
    '#config-add-object-button': function () {
      return S.sender('add-config-object-request');
    },
    '#config-add-array-button': function () {
      return S.sender('add-config-array-request');
    },
    '#config-add-text-button': function () {
      return S.sender('add-config-text-request');
    },

    // ### Documents

    '.add-button': function (t) {
      fieldsets.initFieldset(t, false, true);
    },
    '.remove-button': function (t) {
      fieldsets.removeFieldset(t);
    },
    '#save-document-button': function (t) {
      editui.save();
    },
    '#create-document-button': function (t) {
      editui.create();
    },
    '#clear-document-button': function (t) {
      editui.clear();
    },
    '.expander': function (t) {
      editui.toggleTextarea(t);
    },
    'label span.ui-icon-help': function (t) {
      editui.showHelpDialog(t);
    },
    '#document-edit-button': function (t) {
      viewui.edit(t);
    },
    '#document-delete-button': function (t) {
      viewui.confirmDelete();
    },
    '#document-restore-button': function (t) {
      viewui.confirmRestore();
    },
    '#document-view-tree > ul > li > b': function (t) {
      viewui.collapseToggle(t);
    },
    '.revision-link': function (t) {
      viewui.fetchRevision(t);
    },
    '#search-all-fields-switch a': function () {
      searchui.allFields();
    },
    '.search-field-item': function (t) {
      searchui.removeField(t);
    },
    '.select-results': function (t) {
      searchui.toggleSelection(t);
    },
    '#save-search-results a': function () {
      $('#new-set-target-input').val('search');
      $('#new-set-dialog').show();
    },
    '#save-set-results a': function () {
      $('#new-set-target-input').val('sets');
      $('#new-set-dialog').show();
    },
    '#new-set-save-button': function () {
      S.sender('new-set-form-submit');
    },
    '#select-all-set-elements': function (t) {
      setsui.toggleSelectAll(t);
    },
    '.view-document-link span': function (t) {
      var parent = t[0].parentNode;
      indexui.load(parent);
    },
    '.view-document-link': function (t) {
      indexui.load(t);
    },
    '.select-worksheet-column': function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var field = target.attr('data-field-field');
      worksheetui.columnSelection(field, checked);
    },
    '.select-worksheet-row': function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var row = target.attr('data-row');
      worksheetui.rowSelection(row, checked);
    },
    '#select-all-worksheet-rows': function (t) {
      var checked = $(t).is(':checked');
      worksheetui.selectAllRows(checked);
    },
    '#toggle-handles': function (t) {
      worksheetui.showHandles();
    },
    '.fieldset-handle': function (t) {
      worksheetui.showFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t) {
      worksheetui.showField($(t).attr('data-field-field'));
    },
    '.field-header': function (t) {
      worksheetui.hideField($(t).attr('data-field-field'));
    },

    // ### Index Tool

    '#new-index-button': function (t) {
      ieditui.newCond();
    },
    '.remove-condition-button': function (t) {
      ieditui.remCond(t);
    },
    '#delete-index-button': function (t) {
      ieditui.del();
    },
    '#save-index-button': function (t) {
      ieditui.save();
    },
    '#replace-button': function (t) {
      ieditui.replace();
    },
    '#add-index-condition-button': function (t) {
      ieditui.addCond();
    },
    '#index-index-listing a': function (t) {
      ieditui.init(t);
    },

    // ### Project

    '#create-project': function () {
      projectui.add().dialog('open');
    },
    '.project-delete-button': function (t) {
      projectui.del(t);
    },

    // ### File Manager

    '#up-dir': function () {
      fm.upDir();
    },
    '#root-dir': function () {
      fm.rootDir();
    },
    '.dir': function (t) {
      fm.goDir(t);
    },
    '.delete-file-button': function (t) {
      fm.deleteFile(t);
    },
    '.edit-file-button': function (t) {
      fm.editFile(t);
    },

    // ### General

    '.toggler': function (t) {
      form.toggle(t);
    },
    '.cancel-dialog': function (t) {
      form.cancelDialog(t);
    },
    '#panel-toggle li': function (t) {
      panelToggler(t);
    }
  });

  action(e);
};

exports.clickDispatch = clickDispatch;
