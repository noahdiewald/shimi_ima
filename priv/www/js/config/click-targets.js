// Button that opens a dialog for adding a charsec

var addCharsec = function(button) {
  $("#charseq-add-dialog").dialog("open");
};

// Button that opens a dialog for adding a doctype

var addDoctype = function(target) {
  initDoctypeAddDialog().dialog("open");
};

// Button that opens a dialog for editing a field

var editFieldButton = function(target) {
  var url = cpath(target, "field");
  var oldobj = {};
  var attrs = fieldElems().attrs;
  var charseqUrl = "config/charseqs?as=options";
  
  $.get(charseqUrl, function(charseqs) {
    $("#field-charseq-input").html(charseqs);
    attrs.forEach(function(item) {
      oldobj[item] = getValue('field-' + item, target);
    });
    fieldDialog(url, oldobj).dialog("open");
  });
};

// Button that opens a dialog for deleting a field

var deleteFieldButton = function(target) {
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
};

// Button that opens a dialog for adding a field

var addFieldButton = function(target) {
  var url = cpath(target, "field");
  var charseqUrl = "config/charseqs?as=options";
  
  $.get(charseqUrl, function(charseqs) {
    $("#field-charseq-input").html(charseqs);
    fieldDialog(url, {fieldset: url.fieldset, doctype: url.doctype}).dialog("open");
  });
};

// Button that opens a dialog for editing a fieldset

var editFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  var oldobj = {};
  var attrs = fieldsetElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('fieldset-' + item, target);
  });
  
  fieldsetDialog(url, oldobj).dialog("open");
};

// Button that opens a dialog for deleting a fieldset

var deleteFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  
  var complete = function() {
    url.fieldset = false;
    url.rev = false;
    populateFieldsets(url);
  };
    
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
};

// Button that opens a dialog for adding a fieldset

var addFieldsetButton = function(target) {
  var url = cpath(target, "fieldset");
  fieldsetDialog(url, {doctype: url.doctype}).dialog("open");
};

var editCharseqButton = function(target) {
  var oldobj = {};
  var attrs = charseqElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('charseq-' + item, target);
  });
  charseqDialog(oldobj).dialog("open");
};

var deleteCharseqButton = function(target) {
  var id = getValue('charseq-charseq', target);
  var rev = getValue('charseq-rev', target);
  var url = 'config/charseqs/' + id + '?rev=' + rev;
  var complete = function() {
    populateCharseqTabs();
  };
  
  if (confirm("Are you sure? This is permanent.")) {
    sendConfigDoc(url, {}, 'DELETE', complete, this);
  }
};

var editDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  var oldobj = {};
  var attrs = doctypeElems().attrs;
   
  attrs.forEach(function(item) {
    oldobj[item] = getValue('doctype-' + item, target);
  });
  doctypeDialog(url, oldobj).dialog("open");
};

var deleteDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  var complete = function() {
    url.doctype = false;
    url.rev = false;
    populateDoctypeTabs();
  };
  
  if (confirm("Are you sure? This is permanent.")) {
    url.delete(complete, this);
  }
};

var addDoctypeButton = function(target) {
  var url = cpath(target, "doctype");
  doctypeDialog(url, {}).dialog("open");
};

var addCharseqButton = function(target) {
  charseqDialog({}).dialog("open");
};

// Action for click event on accordion head

var accordionHead = function(target) {
  var url = cpath(target, "field");
  populateFields(url);
};

