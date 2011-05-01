// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".edit-field-button span": function(t) {editFieldButton(t)},
    ".delete-field-button span": function(t) {deleteFieldButton(t)},
    ".add-field-button span": function(t) {addFieldButton(t)},
    ".edit-fieldset-button span": function(t) {editFieldsetButton(t)},
    ".delete-fieldset-button span": function(t) {deleteFieldsetButton(t)},
    ".add-fieldset-button span": function(t) {addFieldsetButton(t)},
    "h3.accordion-head a": function(t) {accordionHead(t)},
    ".delete-doctype-button span": function(t) {deleteDoctypeButton(t)},
    ".edit-doctype-button span": function(t) {editDoctypeButton(t)},
    "#doctype-add-button span": function(t) {addDoctypeButton(t)}
  });

  action(e);
}

