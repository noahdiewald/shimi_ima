// Depending on the css matcher, trigger the specified function on the
// event target. Takes an event object and performs an action.

function clickDispatch(e) {
  var action = dispatcher({
    ".edit-field-button": function(t) {editFieldButton(t)},
    ".delete-field-button": function(t) {deleteFieldButton(t)},
    ".add-field-button": function(t) {addFieldButton(t)},
    ".edit-fieldset-button": function(t) {editFieldsetButton(t)},
    ".delete-fieldset-button": function(t) {deleteFieldsetButton(t)},
    ".add-fieldset-button": function(t) {addFieldsetButton(t)},
    "h3.accordion-head a": function(t) {accordionHead(t)},
    "#doctype-add-button": function(t) {addDoctype(t)}
  });

  action(e);
}

