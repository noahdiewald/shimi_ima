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
  var url = path(target.parent('a'), "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getData('field-' + item, target.parent('a'));
  });
  
  fieldDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a field

function deleteFieldButton(target) {
  var answer = confirm("Are you sure? This is permanent.");
  
  if (answer) {
    var url = path(target.parent('a'), "field");
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
  var url = path(target.parent('a'), "field");
  fieldDialog(url, {fieldset: url.fieldset, doctype: url.doctype}).dialog("open");
}

// Button that opens a dialog for editing a fieldset

function editFieldsetButton(target) {
  var url = path(target.parent('a'), "fieldset");
  var oldobj = {};
  var attrs = fieldsetElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getData('fieldset-' + item, target.parent('a'));
  });
  
  fieldsetDialog(url, oldobj).dialog("open");
}

// Button that opens a dialog for deleting a fieldset

function deleteFieldsetButton(target) {
  var url = path(target.parent('a'), "fieldset");
  
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
  var url = path(target.parent('a'), "fieldset");
  fieldsetDialog(url, {doctype: url.doctype}).dialog("open");
}

function editDoctypeButton(target) {
  var url = path(target.parent('a'), "doctype");
  var oldobj = {};
  var attrs = doctypeElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getData('doctype-' + item, target.parent('a'));
  });
  doctypeDialog(url, oldobj).dialog("open");
}

function deleteDoctypeButton(target) {
  var url = path(target.parent('a'), "doctype");
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
  var url = path(target.parent('a'), "doctype");
  doctypeDialog(url, {}).dialog("open");
}

// Action for click event on accordion head

function accordionHead(button) {
  var url = path(button, "field");
  populateFields(url);
}

