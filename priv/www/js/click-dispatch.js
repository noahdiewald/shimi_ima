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


shimi.dispatcher = function(patterns) {
  var d = function(e) {
    var target = $(e.target);
    
    Object.keys(patterns).forEach(function(pattern) {
      if (target.is(pattern)) {
        var action = patterns[pattern];
        action(target);
      }
    });  
  };
  
  return d;
};

shimi.clickDispatch = function(e) {
  var dt = shimi.doctypeTab;
  var ct = shimi.charseqTab;
  var ed = shimi.eui();
  var vi = shimi.vui;
  var ii = shimi.iiui;
  var ie = shimi.ieui;
  var ip = shimi.ipui;
  var form = shimi.form;
  var pui = shimi.pui;
  
  var action = shimi.dispatcher({
    // Config
    ".edit-field-button": function(t) {dt.editField(t);},
    ".delete-field-button": function(t) {dt.deleteField(t);},
    ".add-field-button": function(t) {dt.addField(t);},
    ".edit-fieldset-button": function(t) {dt.editFieldset(t);},
    ".delete-fieldset-button": function(t) {dt.deleteFieldset(t);},
    ".add-fieldset-button": function(t) {dt.addFieldset(t);},
    ".delete-doctype-button": function(t) {dt.deleteDoctype(t);},
    ".edit-doctype-button": function(t) {dt.editDoctype(t);},
    ".touch-doctype-button": function(t) {dt.touchDoctype(t);},
    "#doctype-add-button": function(t) {dt.addDoctype(t);},
    ".delete-charseq-button": function(t) {ct.del(t);},
    ".edit-charseq-button": function(t) {ct.edit(t);},
    "#charseq-add-button": function(t) {ct.add();},
    "#maintenance-upgrade-button": function(t) {shimi.upgradeButton(t);},
    // Documents
    ".add-button": function(t) {ed({target: t.parent()}).initFieldset();},
    ".remove-button": function(t) {ed({target: t.parent()}).removeFieldset();},
    "#save-document-button": function(t) {ed.save();},
    "#create-document-button": function(t) {ed.create();},
    "#clear-document-button": function(t) {ed.clear();},
    "#document-edit-button": function(t) {vi({target: t.parent()}).edit();},
    "#document-delete-button": function(t) {vi({target: t.parent()}).confirmDelete();},
    "#document-restore-button": function(t) {vi({target: t.parent()}).confirmRestore();},
    "#document-view-tree > ul > li > b": function(t) {vi({target: t}).collapseToggle();},
    ".revision-link": function(t) {vi({target: t}).fetchRevision();},
    ".expander": function(t) {ed.toggleTextarea(t);},
    "label": function(t) {ed.showHelpDialog(t);},
    // Index Tool
    "#new-index-button": function(t) {ie().newCond();},
    ".remove-condition-button": function(t) {ie().remCond(t);},
    "#delete-index-button": function(t) {ie().del();},
    "#save-index-button": function(t) {ie().save();},
    "#replace-button": function(t) {ie().replace();},
    "#add-index-condition-button": function(t) {ie().addCond();},
    // Project
    "#create-project": function() {pui.add().dialog("open");},
    ".project-delete-button": function(t) {pui.del(t);},
    // General
    ".toggler": function(t) {form.toggle(t);}//,
    //".remove-button": function(t) {$(t).parent().remove();}
  });

  action(e);
};

$(function () {
    $('body').click(function(e) {shimi.clickDispatch(e);});
  });

