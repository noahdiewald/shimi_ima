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


function dispatcher(patterns) {
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
}

function clickDispatch(e) {
  var action = dispatcher({
    // Config
    ".edit-field-button span": function(t) {editFieldButton(t.parent('a'));},
    ".delete-field-button span": function(t) {deleteFieldButton(t.parent('a'));},
    ".add-field-button span": function(t) {addFieldButton(t.parent('a'));},
    ".edit-fieldset-button span": function(t) {editFieldsetButton(t.parent('a'));},
    ".delete-fieldset-button span": function(t) {deleteFieldsetButton(t.parent('a'));},
    ".add-fieldset-button span": function(t) {addFieldsetButton(t.parent('a'));},
    ".delete-doctype-button span": function(t) {deleteDoctypeButton(t.parent('a'));},
    ".edit-doctype-button span": function(t) {editDoctypeButton(t.parent('a'));},
    ".touch-doctype-button span": function(t) {touchDoctypeButton(t.parent('a'));},
    "#doctype-add-button span": function(t) {addDoctypeButton(t.parent('a'));},
    ".delete-charseq-button span": function(t) {deleteCharseqButton(t.parent('a'));},
    ".edit-charseq-button span": function(t) {editCharseqButton(t.parent('a'));},
    "#charseq-add-button span": function(t) {addCharseqButton(t.parent('a'));},
    "#maintenance-upgrade-button span": function(t) {upgradeButton(t.parent('a'));},
    // Documents
    ".add-button span": function(t) {eui({target: t.parent()}).initFieldset();},
    ".remove-button span": function(t) {eui({target: t.parent()}).removeFieldset();},
    "#save-document-button span": function(t) {eui({target: t.parent()}).save();},
    "#create-document-button span": function(t) {eui({target: t.parent()}).create();},
    "#clear-document-button span": function(t) {eui({target: t.parent()}).clear();},
    "#document-edit-button span": function(t) {vui({target: t.parent()}).edit();},
    "#document-delete-button span": function(t) {vui({target: t.parent()}).confirmDelete();},
    "#document-restore-button span": function(t) {vui({target: t.parent()}).confirmRestore();},
    "#document-view-tree > ul > li > b": function(t) {vui({target: t}).collapseToggle();},
    ".revision-link": function(t) {vui({target: t}).fetchRevision();},
    ".expander": function(t) {eui({target: t}).toggleTextarea();},
    "label span": function(t) {eui({target: t}).showHelpDialog();},
    
  });

  action(e);
}

$(function () {
    $('body').click(function(e) {clickDispatch(e);});
  });

