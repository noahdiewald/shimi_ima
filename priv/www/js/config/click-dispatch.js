// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".edit-field-button span": function(t) {editFieldButton(t.parent('a'))},
    ".delete-field-button span": function(t) {deleteFieldButton(t.parent('a'))},
    ".add-field-button span": function(t) {addFieldButton(t.parent('a'))},
    ".edit-fieldset-button span": function(t) {editFieldsetButton(t.parent('a'))},
    ".delete-fieldset-button span": function(t) {deleteFieldsetButton(t.parent('a'))},
    ".add-fieldset-button span": function(t) {addFieldsetButton(t.parent('a'))},
    "h3.accordion-head a": function(t) {accordionHead(t)},
    ".delete-doctype-button span": function(t) {deleteDoctypeButton(t.parent('a'))},
    ".edit-doctype-button span": function(t) {editDoctypeButton(t.parent('a'))},
    "#doctype-add-button span": function(t) {addDoctypeButton(t.parent('a'))}
  });

  action(e);
}

