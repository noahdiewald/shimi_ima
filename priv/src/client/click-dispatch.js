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
  var sui = shimi.sui;

  var action = shimi.dispatcher({
    ".search-result-field-id a": function (t) {
      sui.addField($(t).parent("h5"));
    },
    ".field-view b": function (t) {
      sui.addField($(t).parent("li"));
    },
    ".field-container label span": function (t) {
      sui.addField($(t).parent("label").parent("div"));
    },
    "#index-index-input-label": function () {
      sui.addIndex();
    },
    ".panel > h2": function (t) {
      shimi.panelToggle.toggler(t);
    }
  });

  action(e);
};

shimi.clickDispatch = function (e) {
  var doctypeTab = shimi.doctypeTab;
  var charseqTab = shimi.charseqTab;
  var editui = shimi.editui;
  var vui = shimi.vui;
  var iui = shimi.iui;
  var setsui = shimi.setsui;
  var sui = shimi.sui;
  var efs = shimi.efs;
  var ieui = shimi.ieui;
  var form = shimi.form;
  var pui = shimi.pui;
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
      efs.initFieldset(t);
    },
    ".remove-button": function (t) {
      efs.removeFieldset(t);
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
      vui.edit(t);
    },
    "#document-delete-button": function (t) {
      vui.confirmDelete();
    },
    "#document-restore-button": function (t) {
      vui.confirmRestore();
    },
    "#document-view-tree > ul > li > b": function (t) {
      vui.collapseToggle(t);
    },
    ".revision-link": function (t) {
      vui.fetchRevision(t);
    },
    "#search-all-fields-switch a": function () {
      sui.allFields();
    },
    ".search-field-item": function (t) {
      sui.removeField(t);
    },
    "#document-search-exclude": function () {
      sui.toggleExclusion();
    },
    "#document-search-invert": function () {
      sui.toggleInversion();
    },
    ".select-results": function (t) {
      sui.toggleSelection(t);
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
      setsui.saveSelected($("#new-set-target-input").val());
    },
    "#select-all-set-elements": function (t) {
      setsui.toggleSelectAll(t);
    },
    ".view-document-link": function (t) {
      iui.load(t);
    },

    // Index Tool
    "#new-index-button": function (t) {
      ieui.newCond();
    },
    ".remove-condition-button": function (t) {
      ieui.remCond(t);
    },
    "#delete-index-button": function (t) {
      ieui.del();
    },
    "#save-index-button": function (t) {
      ieui.save();
    },
    "#replace-button": function (t) {
      ieui.replace();
    },
    "#add-index-condition-button": function (t) {
      ieui.addCond();
    },
    "#index-index-listing ul li a": function (t) {
      ieui.init(t);
    },

    // Project
    "#create-project": function () {
      pui.add().dialog("open");
    },
    ".project-delete-button": function (t) {
      pui.del(t);
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