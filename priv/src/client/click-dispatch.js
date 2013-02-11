/**
 WARNING: OUT OF DATE
 
 == Click Dispatcher
 
 Each section of the application calls this function with an object
 composed of keys of CSS patterns of elements which should have click
 event actions bound to them and values of functions that will be
 called if a click event occurs and the key pattern matches.
 
 @dispatcher(patterns)@
 
 *More to come*
 */


shimi.dispatcher = function (patterns) {
  var d = function (e) {
    var target = $(e.target);

    Object.keys(patterns).forEach(function (pattern) {
      if (target.is(pattern)) {
        var action = patterns[pattern];
        action(target);
      }
    });
  };

  return d;
};

shimi.dblclickDispatch = function (e) {
  var searchui = shimi.searchui;
  var worksheetui = shimi.worksheetui;

  var action = shimi.dispatcher({
    ".search-result-field-id a": function (t) {
      searchui.addField($(t).parent("h5"));
    },
    ".field-view b": function (t) {
      searchui.addField($(t).parent("li"));
    },
    ".field-container label span": function (t) {
      searchui.addField($(t).parent("label").parent("div"));
    },
    "#index-index-input-label": function () {
      searchui.addIndex();
    },
    ".panel > h2": function (t) {
      shimi.panelToggle.toggler(t);
    },
    "#toggle-handles": function (t) {
      worksheetui.hideHandles();
    },
    ".fieldset-handle": function (t) {
      worksheetui.hideFieldset($(t).attr("data-field-fieldset"));
    },
    ".field-handle": function (t) {
      worksheetui.hideField($(t).attr("data-field-field"));
    }
  });

  action(e);
};

shimi.clickDispatch = function (e) {
  var doctypeTab = shimi.doctypeTab;
  var charseqTab = shimi.charseqTab;
  var editui = shimi.editui;
  var viewui = shimi.viewui;
  var indexiu = shimi.indexiu;
  var setsui = shimi.setsui;
  var searchui = shimi.searchui;
  var worksheetui = shimi.worksheetui;
  var fieldsets = shimi.fieldsets;
  var ieditui = shimi.ieditui;
  var form = shimi.form;
  var projectui = shimi.projectui;
  var fm = shimi.fm;

  var action = shimi.dispatcher({
    // Config
    ".edit-field-button": function (t) {
      doctypeTab.editField(t);
    },
    ".delete-field-button": function (t) {
      doctypeTab.deleteField(t);
    },
    ".add-field-button": function (t) {
      doctypeTab.addField(t);
    },
    ".edit-fieldset-button": function (t) {
      doctypeTab.editFieldset(t);
    },
    ".delete-fieldset-button": function (t) {
      doctypeTab.deleteFieldset(t);
    },
    ".add-fieldset-button": function (t) {
      doctypeTab.addFieldset(t);
    },
    ".delete-doctype-button": function (t) {
      doctypeTab.deleteDoctype(t);
    },
    ".edit-doctype-button": function (t) {
      doctypeTab.editDoctype(t);
    },
    ".touch-doctype-button": function (t) {
      doctypeTab.touchDoctype(t);
    },
    "#doctype-add-button": function (t) {
      doctypeTab.addDoctype(t);
    },
    ".delete-charseq-button": function (t) {
      charseqTab.del(t);
    },
    ".edit-charseq-button": function (t) {
      charseqTab.edit(t);
    },
    "#charseq-add-button": function (t) {
      charseqTab.add();
    },
    "#maintenance-upgrade-button": function (t) {
      shimi.upgradeButton(t);
    },

    // Documents
    ".add-button": function (t) {
      fieldsets.initFieldset(t);
    },
    ".remove-button": function (t) {
      fieldsets.removeFieldset(t);
    },
    "#save-document-button": function (t) {
      editui.save();
    },
    "#create-document-button": function (t) {
      editui.create();
    },
    "#clear-document-button": function (t) {
      editui.clear();
    },
    ".expander": function (t) {
      editui.toggleTextarea(t);
    },
    "label span.ui-icon-help": function (t) {
      editui.showHelpDialog(t);
    },
    "#document-edit-button": function (t) {
      viewui.edit(t);
    },
    "#document-delete-button": function (t) {
      viewui.confirmDelete();
    },
    "#document-restore-button": function (t) {
      viewui.confirmRestore();
    },
    "#document-view-tree > ul > li > b": function (t) {
      viewui.collapseToggle(t);
    },
    ".revision-link": function (t) {
      viewui.fetchRevision(t);
    },
    "#search-all-fields-switch a": function () {
      searchui.allFields();
    },
    ".search-field-item": function (t) {
      searchui.removeField(t);
    },
    "#document-search-exclude": function () {
      searchui.toggleExclusion();
    },
    "#document-search-invert": function () {
      searchui.toggleInversion();
    },
    ".select-results": function (t) {
      searchui.toggleSelection(t);
    },
    "#save-search-results a": function () {
      $("#new-set-target-input").val("search");
      $("#new-set-dialog").show();
    },
    "#save-set-results a": function () {
      $("#new-set-target-input").val("sets");
      $("#new-set-dialog").show();
    },
    "#new-set-save-button": function () {
      shimi.dispatch.send("new-set-form-submit");
    },
    "#select-all-set-elements": function (t) {
      setsui.toggleSelectAll(t);
    },
    ".view-document-link": function (t) {
      indexiu.load(t);
    },
    ".select-worksheet-column": function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var field = target.attr("data-field-field");
      worksheetui.columnSelection(field, checked);
    },
    ".select-worksheet-row": function (t) {
      var target = $(t);
      var checked = target.is(':checked');
      var row = target.attr("data-row");
      worksheetui.rowSelection(row, checked);
    },
    "#select-all-worksheet-rows": function (t) {
      var checked = $(t).is(':checked');
      worksheetui.selectAllRows(checked);
    },
    "#toggle-handles": function (t) {
      worksheetui.showHandles();
    },
    ".fieldset-handle": function (t) {
      worksheetui.showFieldset($(t).attr("data-field-fieldset"));
    },
    ".field-handle": function (t) {
      worksheetui.showField($(t).attr("data-field-field"));
    },
    ".field-header": function (t) {
      worksheetui.hideField($(t).attr("data-field-field"));
    },

    // Index Tool
    "#new-index-button": function (t) {
      ieditui.newCond();
    },
    ".remove-condition-button": function (t) {
      ieditui.remCond(t);
    },
    "#delete-index-button": function (t) {
      ieditui.del();
    },
    "#save-index-button": function (t) {
      ieditui.save();
    },
    "#replace-button": function (t) {
      ieditui.replace();
    },
    "#add-index-condition-button": function (t) {
      ieditui.addCond();
    },
    "#index-index-listing ul li a": function (t) {
      ieditui.init(t);
    },

    // Project
    "#create-project": function () {
      projectui.add().dialog("open");
    },
    ".project-delete-button": function (t) {
      projectui.del(t);
    },

    // File Manager
    "#up-dir": function () {
      fm.upDir();
    },
    "#root-dir": function () {
      fm.rootDir();
    },
    ".dir": function (t) {
      fm.goDir(t);
    },
    ".delete-file-button": function (t) {
      fm.deleteFile(t);
    },
    ".edit-file-button": function (t) {
      fm.editFile(t);
    },

    // General
    ".toggler": function (t) {
      form.toggle(t);
    },
    ".cancel-dialog": function (t) {
      form.cancelDialog(t);
    },
    "#panel-toggle li": function (t) {
      shimi.panelToggle.toggler(t);
    }
    //".remove-button": function(t) {$(t).parent().remove();}
  });

  action(e);
};

$(function () {
  $('body').click(function (e) {
    shimi.clickDispatch(e);
  });
  $('body').dblclick(function (e) {
    shimi.dblclickDispatch(e);
  });
});