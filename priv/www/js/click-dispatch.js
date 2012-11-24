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
  var ed = shimi.eui;
  var vi = shimi.vui;
  
  var action = shimi.dispatcher({
    // Config
    ".edit-field-button span": function(t) {dt(t.parent('a')).editField();},
    ".delete-field-button span": function(t) {dt(t.parent('a')).deleteField();},
    ".add-field-button span": function(t) {dt(t.parent('a')).addField();},
    ".edit-fieldset-button span": function(t) {dt(t.parent('a')).editFieldset();},
    ".delete-fieldset-button span": function(t) {dt(t.parent('a')).deleteFieldset();},
    ".add-fieldset-button span": function(t) {dt(t.parent('a')).addFieldset();},
    ".delete-doctype-button span": function(t) {dt(t.parent('a')).deleteDoctype();},
    ".edit-doctype-button span": function(t) {dt(t.parent('a')).editDoctype();},
    ".touch-doctype-button span": function(t) {dt(t.parent('a')).touchDoctype();},
    "#doctype-add-button span": function(t) {dt(t.parent('a')).addDoctype();},
    ".delete-charseq-button span": function(t) {ct(t.parent('a')).del();},
    ".edit-charseq-button span": function(t) {ct(t.parent('a')).edit();},
    "#charseq-add-button span": function(t) {ct().add();},
    "#maintenance-upgrade-button span": function(t) {shimi.upgradeButton(t.parent('a'));},
    // Documents
    ".add-button span": function(t) {ed({target: t.parent()}).initFieldset();},
    ".remove-button span": function(t) {ed({target: t.parent()}).removeFieldset();},
    "#save-document-button span": function(t) {ed({target: t.parent()}).save();},
    "#create-document-button span": function(t) {ed({target: t.parent()}).create();},
    "#clear-document-button span": function(t) {ed({target: t.parent()}).clear();},
    "#document-edit-button span": function(t) {vi({target: t.parent()}).edit();},
    "#document-delete-button span": function(t) {vi({target: t.parent()}).confirmDelete();},
    "#document-restore-button span": function(t) {vi({target: t.parent()}).confirmRestore();},
    "#document-view-tree > ul > li > b": function(t) {vi({target: t}).collapseToggle();},
    ".revision-link": function(t) {vi({target: t}).fetchRevision();},
    ".expander": function(t) {ed({target: t}).toggleTextarea();},
    "label span": function(t) {ed({target: t}).showHelpDialog();}
  });

  action(e);
};

$(function () {
    $('body').click(function(e) {shimi.clickDispatch(e);});
  });

