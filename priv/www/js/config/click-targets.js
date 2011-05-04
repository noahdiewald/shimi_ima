// Button that opens a dialog for adding a charsec

function addCharsec(button) {
  $("#charseq-add-dialog").dialog("open");
}

// Button that opens a dialog for adding a doctype

function addDoctype(target) {
  initDoctypeAddDialog().dialog("open");
}

// Button that opens a dialog for editing a field

function editFieldButton(target) {
  var url = cpath(target, "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('field-' + item, target);
  });
  
  fieldDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a field

function deleteFieldButton(target) {
  var answer = confirm("Are you sure? This is permanent.");
  
  if (answer) {
    var url = cpath(target, "field");
    var complete = function() {
      url.field = false;
      url.rev = false;
      
      populateFields(url)
    };
    url.delete(complete, this);
  }
}

// Button that opens a dialog for adding a field

function addFieldButton(target) {
  var url = cpath(target, "field");
  fieldDialog(url, {fieldset: url.fieldset, doctype: url.doctype}).dialog("open");
}

// Button that opens a dialog for editing a fieldset

function editFieldsetButton(target) {
  var url = cpath(target, "fieldset");
  var oldobj = {};
  var attrs = fieldsetElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('fieldset-' + item, target);
  });
  
  fieldsetDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a fieldset

function deleteFieldsetButton(target) {
  var url = cpath(target, "fieldset");
  
  var complete = function() {
    url.fieldset = false;
    url.rev = false;
    populateFieldsets(url);
  };
    
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
}

// Button that opens a dialog for adding a fieldset

function addFieldsetButton(target) {
  var url = cpath(target, "fieldset");
  fieldsetDialog(url, {doctype: url.doctype}).dialog("open");
}

function editDoctypeButton(target) {
  var url = cpath(target, "doctype");
  var oldobj = {};
  var attrs = doctypeElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('doctype-' + item, target);
  });
  doctypeDialog(url, oldobj).dialog("open");
}

function deleteDoctypeButton(target) {
  var url = cpath(target, "doctype");
  var complete = function() {
    url.doctype = false;
    url.rev = false;
    populateDoctypeTabs();
  };
  
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
}

function addDoctypeButton(target) {
  var url = cpath(target, "doctype");
  doctypeDialog(url, {}).dialog("open");
}

// Action for click event on accordion head

function accordionHead(target) {
  var url = cpath(target, "field");
  populateFields(url);
}

