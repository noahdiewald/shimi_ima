// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".add-button span": function(t) {initFieldset(t.parent())},
    ".remove-button span": function(t) {removeFieldset(t.parent())},
    "#save-document-button span": function(t) {saveDoc(t.parent())},
    "#create-document-button span": function(t) {createDoc(t.parent())},
    "#clear-document-button span": function(t) {clearDoc(t.parent())},
    "#document-edit-button span": function(t) {editDoc(t.parent())},
    "#document-delete-button span": function(t) {deleteDoc(t.parent())},
    "#document-view-list > li > b": function(t) {collapseToggle(t)},
    "#panel-toggle li": function(t) {panelToggle(t)},
    ".expander": function(t) {toggleTextarea(t)},
    "label span": function(t) {showHelpDialog(t)}
  });

  action(e);
}
