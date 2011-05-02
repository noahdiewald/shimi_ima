// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.
// There are duplicates here because Firefox 3.6 has the event target as
// the button while Google Chrome has it as the span around the button
// text.

function clickDispatch(e) {
  var action = dispatcher({
    ".add-button span": function(t) {addFieldset(t.parent())},
    ".add-button": function(t) {addFieldset(t)},
    ".remove-button span": function(t) {removeFieldset(t.parent())},
    ".remove-button": function(t) {removeFieldset(t)},
    "#save-document-button span": function(t) {saveDoc(t.parent())},
    "#save-document-button": function(t) {saveDoc(t)},
    "#create-document-button span": function(t) {createDoc(t.parent())},
    "#create-document-button": function(t) {createDoc(t)},
    "#clear-document-button span": function(t) {clearDoc(t.parent())},
    "#clear-document-button": function(t) {clearDoc(t)},
    "#document-edit-button span": function(t) {editDoc(t.parent())},
    "#document-edit-button": function(t) {editDoc(t)},
    "#document-delete-button span": function(t) {deleteDoc(t.parent())},
    "#document-delete-button": function(t) {deleteDoc(t)},
    "#document-view-list > li > b": function(t) {collapseToggle(t)},
    "#panel-toggle li": function(t) {panelToggle(t)}
  });

  action(e);
}

