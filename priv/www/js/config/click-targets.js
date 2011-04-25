// Button that opens a dialog for editing a field

function editFieldButton(button) {
  var url = buildUrl(button, "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getData('field-' + item, button);
  });
  
  fieldDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a field

function deleteFieldButton(button) {
  var answer = confirm("Are you sure? This is permanent.");
  
  if (answer) {
    var url = buildUrl(button, "field");
    var complete = function() {populateFields(url.doctype, url.fieldset)};
    url.delete(complete, this);
  }
}

// Button that opens a dialog for adding a field

function addFieldButton(button) {
  var url = buildUrl(button, "field");
  fieldDialog(url, {fieldset: url.fieldset, doctype: url.doctype}).dialog("open");
}

// Button that opens a dialog for editing a fieldset

function editFieldsetButton(button) {
  var rev = getData('fieldset-rev', button);
  var doctype = getData('doctype-id', button);
  var fieldset = getData('fieldset-id', button);
  var oldobj = {
    name: getData('fieldset-name', button),
    label: getData('fieldset-label', button),
    order: getData('fieldset-order', button),
    multiple: getData('fieldset-multiple', button),
    collapse: getData('fieldset-collapse', button),
    description: getData('fieldset-description', button) 
  };
  
  initFieldsetEditDialog(fieldset, doctype, oldobj, rev).dialog("open");
}

// Button that opens a dialog for deleting a fieldset

function deleteFieldsetButton(button) {
  var rev = getData('fieldset-rev', button);
  var doctype = getData('doctype-id', button);
  var fieldset = getData('fieldset-id', button);
  var url = "config/doctypes/" + doctype + 
"/fieldsets/" + fieldset + "?rev=" + rev;
  var complete = function() {
    populateFieldsets(doctype);
  };
    
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
}

// Button that opens a dialog for adding a fieldset

function addFielsetButton(button) {
  fieldsetDoctype.val(getData('doctype-id', button));
  initFieldsetAddDialog().dialog("open");
}

function editDoctypeButton(button) {
  var description = getData('doctype-description', button);
  var name = getData('doctype-id', button);
  var rev = getData('doctype-rev', button);
  initDoctypeEditDialog(name, description, rev).dialog("open");
}

function deleteDoctypeButton(button) {
  var name = getData('doctype-id', button);
  var rev = getData('doctype-rev', button);
  var url = "config/doctypes/" + name + "?rev=" + rev;
  var complete = function() {populateDoctypeTabs()};
  
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
}

function addDoctypeButton(button) {
}

// Action for click event on accordion head

function accordionHead(head) {
  var doctype = getData('fieldset-doctype', head.parent('h3'));
  var fieldset = getData('fieldset-fieldset', head.parent('h3'));
  populateFields(doctype, fieldset);
}

