// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".add-button": function(t) {addFieldset(t)},
    ".remove-button": function(t) {removeFieldset(t)},
    "#save-document-button": function(t) {save(t)},
    "#create-document-button": function(t) {create(t)},
    "#clear-document-button": function(t) {clear(t)},
    "#document-edit-button": function(t) {edit(t)},
    "#document-delete-button": function(t) {delete(t)},
    "#document-view-list > li > b": function(t) {collapseToggle(t)},
    "#panel-toggle li": function(t) {panelToggle(t)}
  });

  action(e);
}

