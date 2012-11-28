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
  
  var action = shimi.dispatcher({
    // Config
    ".edit-field-button span": function(t) {dt.editField(t.parent('a'));},
    ".delete-field-button span": function(t) {dt.deleteField(t.parent('a'));},
    ".add-field-button span": function(t) {dt.addField(t.parent('a'));},
    ".edit-fieldset-button span": function(t) {dt.editFieldset(t.parent('a'));},
    ".delete-fieldset-button span": function(t) {dt.deleteFieldset(t.parent('a'));},
    ".add-fieldset-button span": function(t) {dt.addFieldset(t.parent('a'));},
    ".delete-doctype-button span": function(t) {dt.deleteDoctype(t.parent('a'));},
    ".edit-doctype-button span": function(t) {dt.editDoctype(t.parent('a'));},
    ".touch-doctype-button span": function(t) {dt.touchDoctype(t.parent('a'));},
    "#doctype-add-button span": function(t) {dt.addDoctype(t.parent('a'));},
    ".delete-charseq-button span": function(t) {ct.del(t.parent('a'));},
    ".edit-charseq-button span": function(t) {ct.edit(t.parent('a'));},
    "#charseq-add-button span": function(t) {ct.add();},
    "#maintenance-upgrade-button span": function(t) {shimi.upgradeButton(t.parent('a'));},
    // Documents
    ".add-button span": function(t) {ed({target: t.parent()}).initFieldset();},
    ".remove-button span": function(t) {ed({target: t.parent()}).removeFieldset();},
    "#save-document-button span": function(t) {ed.save();},
    "#create-document-button span": function(t) {ed.create();},
    "#clear-document-button span": function(t) {ed.clear();},
    "#document-edit-button span": function(t) {vi({target: t.parent()}).edit();},
    "#document-delete-button span": function(t) {vi({target: t.parent()}).confirmDelete();},
    "#document-restore-button span": function(t) {vi({target: t.parent()}).confirmRestore();},
    "#document-view-tree > ul > li > b": function(t) {vi({target: t}).collapseToggle();},
    ".revision-link": function(t) {vi({target: t}).fetchRevision();},
    ".expander": function(t) {ed.toggleTextarea(t);},
    "label span": function(t) {ed.showHelpDialog(t);},
    // Index Tool
    "#new-index-button": function(t) {ie().newCond();},
    ".remove-condition-button": function(t) {ie().remCond(t);},
    "#delete-index-button": function(t) {ie().del();},
    "#save-index-button": function(t) {ie().save();},
    "#replace-button": function(t) {ie().replace();},
    "#add-index-condition-button": function(t) {ie().addCond();},
    // General
    ".toggler span": function(t) {form.toggle(t.parent('a'));}
  });

  action(e);
};

$(function () {
    $('body').click(function(e) {shimi.clickDispatch(e);});
  });

