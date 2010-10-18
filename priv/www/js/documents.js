// Helper for building the url to access a fieldset for a document
function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
}

// Form filling functions used to push the data in the center column into the
// right column's form.

// Fill the form fields with a document's values
function fillFieldsets() {
  $('.fieldset-view').each(function(fieldsetViewIndex, fieldsetView) {
    if ($(fieldsetView).attr('data-multiple') == "true") {
      fillMultiFieldsets(fieldsetView);
    } else {
      fillNormalFieldsets(fieldsetView);
    }
  });
  
  afterEditRefresh();

  return true;
}

// Fill fields in multiple fieldsets
function fillMultiFieldsets(fieldsetView) {
  var fieldsetId = $(fieldsetView).attr('data-fieldset-view-id');
  var fieldsetContainer = $('.fieldset-container[data-fieldset-id=' + fieldsetId + ']');
  var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                     fieldsetContainer.attr('data-doctype-id'),
                     fieldsetContainer.attr('data-fieldset-id'));
  
  // Clear the container
  fieldsetContainer.html('');
  
  $(fieldsetView).children('.multifield').each(function(multifieldIndex, multifield) {
    // Initialize the fieldset with a callback function that will fill the fields
    initFieldset(fieldsetContainer, url, function(fieldset) {
      fillFields($(multifield), fieldset);
    });
  });
  
  return true;
}

// Fill fields in normal fieldset
function fillNormalFieldsets(fieldsetView) {
  fillFields($(fieldsetView));
  
  return true;
}

// Fill fields, optionally a context is used to fill fields for only
// a particular fieldset, useful when used as a callback
function fillFields(container, context) {
  container.find('.field-view').each(function(fieldIndex, field) {
    var value = $(field).attr('data-field-view-value');
    var fieldId = $(field).attr('data-field-view-id');
    
    if (!context) context = $('body');
    
    setFieldValue(context.find('.field[data-field-id=' + fieldId + ']'), value);
  });
  
  return true;
}

function setFieldValue(field, value) {
  if (field.is('input.boolean')) {
    field.attr("checked", value == "true");
  } else if (field.is('select.multiselect')) {
    field.val(value.split(","));
  } else {
    field.val(value);
  }
  
  return true;
}

// Functions to run after the form is refreshed or filled.

// This is run after the form is refreshed with new values inserted.
function afterEditRefresh() {
  $('#save-document-button').attr('data-document-id', $('#document-edit-button').attr('data-document-id'));
  $('#save-document-button').attr('data-document-rev', $('#document-edit-button').attr('data-document-rev'));
  $('#save-document-button').show();
  
  afterRefresh();
  
  return true;
}

// This is run after the form is cleared or created empty
function afterFreshRefresh() {
  initRemoveButton();
  afterRefresh();
  return true;
}

// This should be run after either afterFreshRefresh() or afterEditRefresh()
function afterRefresh() {
  initDateFields();
  return true;
}

// Functions for building view via ajax calls. Functions preceded by "get"
// are generally pulling in data. Functions preceded by "init" are generally
// generating parts of a form.

// Get a document and display it in the middle column
function getDocument(id, runAfterEditRefresh) {
  var url = "documents/" + id;
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);
    
    if (runAfterEditRefresh) afterEditRefresh();
    
    $('#document-edit-button').button().click(function() {
      $('.fields').val('');
      $('.fields').attr('checked', 'false');
      fillFieldsets();
    });
  });
}

// Get the index that is displayed in the left column 
function getIndex() {
  var url = "documents/index";
  
  $.get(url, function(documentIndexHtml) {
    $('#document-index').html(documentIndexHtml);

    $('.view-document-link').click(function() {
      getDocument(this.hash.slice(1));
    });
  });
}

// Initialize the form in the right column
function initEdit() {
  var url = "documents/edit";
  
  $.get(url, function(documentEditHtml) {
    $('#document-edit').html(documentEditHtml);
    initFieldsets();
    initEditButtons();
  });
  
  return true;
}

// Functions for initializing buttons

// Initialize buttons for form in right column
function initEditButtons() {
  initAddButton();
  initSaveButton();
  initCreateButton();
  initClearButton();
  return true;
}

// Initialize the button that will add additional fieldsets to
// a multiple fieldset 
function initAddButton() {
  $(".add-button").button({
    icons: {primary: "ui-icon-plus"},
    text: false
  }).click(function() {
    var fieldsetContainer = $("#container-" + $(this).attr('data-fieldset-id'));
    var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                       fieldsetContainer.attr('data-doctype-id'),
                       fieldsetContainer.attr('data-fieldset-id'));
     
    initFieldset(fieldsetContainer, url);
  });
  
  return true;
}

// Initialize the button that removes a fieldset from a multiple fieldset
function initRemoveButton() {  
  $(".remove-button").button({
    icons: {primary: "ui-icon-minus"},
    text: false
  }).click(function() {
    $(this).parent().remove()
  });
  
  return true;
}

// Initialize the save button that is used for already existing documents.
// Hide the button until the form has been filled by an already existing
// document's values. See the after functions above.
function initSaveButton() {
  $('#save-document-button').button({
    icons: {primary: "ui-icon-disk"}
  }).click(function() {
    var saveButton = $(this);
    var root = $('#edit-document-form');
    var documentId = saveButton.attr('data-document-id');
    var documentRev = saveButton.attr('data-document-rev');
    var url = "./documents/" + documentId + "?rev=" + documentRev;
    var obj = {
      doctype: saveButton.attr('data-doctype-id'),
      description: saveButton.attr('data-doctype-description')
    };
    
    saveButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
    $.ajax({
      type: "PUT",
      url: url,
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function(req, status) {
        if (req.status == 204) {
          var title = "Success";
          var body = "Your document was saved.";
          getDocument(documentId, true);
          getIndex();
          flashHighlight(title, body);
          saveButton.button('enable');
        } else {
          var title = "Failure";
          var body = "Server returned " + req.status;
          
          flashError(title, body);
          saveButton.button('enable');
        }
      }
    });
    
  });
  
  $('#save-document-button').hide();
  
  return true;
}

// Initialize the create as new button used to create a new document based
// on filled in values.
function initCreateButton() {
  $("#create-document-button").button({
    icons: {primary: "ui-icon-document"}
  }).click(function() {
    var createButton = $(this);
    var root = $('#edit-document-form');
    var obj = {
      doctype: createButton.attr('data-doctype-id'),
      description: createButton.attr('data-doctype-description')
    };
    
    createButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
    $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function(req, status) {
        if (req.status == 201) {
          var title = "Success";
          var body = "Your document was created.";
          
          $('.fields').remove();
          initFieldsets();
          getIndex();
          flashHighlight(title, body);
          createButton.button('enable');
        } else {
          var title = "Failure";
          var body = "Server returned " + req.status;
          
          flashError(title, body);
          createButton.button('enable');
        }
      }
    });
    
  });

  return true;
}

// Initialize the clear button that will rebuild the form fresh.
function initClearButton() {
  $('#clear-document-button').button({
    icons: {primary: "ui-icon-refresh"}
  }).click(function() {
    $('#save-document-button').hide();
    $('.fields').remove();
    initFieldsets();
  });
  
  return true;
}

// Given the context of a fieldset in the HTML and a URL to dowload the
// fields from the database, prepend the fields to the HTML in the fieldset.
// Prepending avoids overwriting or putting fields below the remove button
// that may appear in multiple fieldsets. When filling the form fields of
// a multiple fieldset, a callback is also sent that has the correct context
// for that purpose.
function initFields(fieldset, url, fieldsetCallback) {
  $.get(url, function(fields) {
    fieldset.prepend(fields);
    if (fieldsetCallback) {
      fieldsetCallback(fieldset);
    }
    
    afterFreshRefresh();
  });
  
  return true;
}

// Retrieve a fieldset rendering and request the field renderings. When this
// fieldset is in a multiple fieldset, a callback may be supplied when filling
// the fields to provide the correct context.
function initFieldset(fieldsetContainer, url, fieldsetCallback) {
  $.get(url, function(newFieldset) {
    fieldsetContainer.append(newFieldset);
    
    var fieldset = fieldsetContainer.children().last();
    
    initFields(fieldset, url + "/fields", fieldsetCallback);
  });
  
  return true;
}


// Initialize all the fieldsets. 
function initFieldsets() {
  $('fieldset').each(function(index) {
    var fieldsetContainer = $(this).children('.fieldset-container').first();
    var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                       fieldsetContainer.attr('data-doctype-id'),
                       fieldsetContainer.attr('data-fieldset-id'));
    
    initFieldset(fieldsetContainer, url);
  });
  
  return true;
}

// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets. 
function fieldsetsToObject(root) {
  var obj = {fieldsets:[]};
  
  root.children('fieldset').each(function(index) {
    var fieldset = $(this);
    var fieldsetId = fieldset.attr('data-fieldset-id');
    var fieldsetMultiple = fieldset.attr('data-fieldset-multiple') == "true";
    var fieldsContainers = $('#container-' + fieldsetId).children('.fields');
    var fieldsetObj = {
      id: fieldsetId,
      multiple: fieldsetMultiple,
      name: fieldset.attr('data-fieldset-name'),
      label: fieldset.attr('data-fieldset-label'),
      order: fieldset.attr('data-fieldset-order') * 1
    }

    if (!fieldsetMultiple) {
      $.extend(fieldsetObj, fieldsToObject(fieldsContainers.first()));
    } else {
      fieldsetObj.multifields = []
      
      fieldsContainers.each(function(index1) {
        var fieldContainer = $(this);
        
        fieldsetObj.multifields[index1] = fieldsToObject(fieldContainer, index1);
      });
    }
    
    obj.fieldsets[index] = fieldsetObj;
  });
  
  return obj;
}

// Convert field values to an object that can be converted to JSON
function fieldsToObject(fieldsContainer, fieldsIndex) {
  var fields = fieldsContainer.children('.field-container').children('.field');
  var obj = {fields:[]};
  
  fields.each(function(index) {
    var field = $(this);
    var fieldId = field.attr('data-field-id');
    var fieldName = field.attr('name');
    var fieldValue = getFieldValue(field);
    
    obj.fields[index] = {
      id: fieldId,
      name: fieldName,
      label: field.attr('data-field-label'),
      head: field.attr('data-field-head') == "true",
      reversal: field.attr('data-field-reversal') == "true",
      order: field.attr('data-field-order') * 1,
      subcategory: field.attr('data-field-subcategory'),
      value: fieldValue
    }
    
    if (fieldsIndex >= 0) {
      obj.fields[index].index = fieldsIndex;
    }
  })
  
  return obj;
}

// Get the value from a field that not much is known about.
function getFieldValue(field) {
  var fieldValue;
  
  if (field.is('input.boolean')) {
    fieldValue = field.is('input:checkbox:checked');
  } else if (field.is('input.number')) {
    fieldValue = field.val() * 1;
  } else {
    fieldValue = field.val();
  }
  
  return fieldValue;
}

$(function () {
  getIndex();
  initEdit();
});
