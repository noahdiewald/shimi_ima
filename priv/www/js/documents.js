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
  
  $(fieldsetView).find('.multifield').each(function(multifieldIndex, multifield) {
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
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').removeAttr('disabled');
  
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
  } else if (value && field.is('select.multiselect')) {
    field.val(value.split(","));
  } else {
    field.val(value);
  }
  
  return true;
}

// Functions to run after the form is refreshed or filled.

// This is run after the form is refreshed with new values inserted.
function afterEditRefresh() {
  var saveBttn = $('#save-document-button');
  var editBttn = $('#document-edit-button');
  var sharedAttrs = ['data-document-id', 'data-document-rev'];
  
  _(sharedAttrs).each(function(elem) {
    saveBttn.attr(elem, editBttn.attr(elem));
  });
  
  saveBttn.show();
  
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
  
  $('#document-view').fadeTo('slow', 1);
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);
    
    if (runAfterEditRefresh) afterEditRefresh();
    
    $('#document-edit-button').button().click(function() {
      resetFields();
      fillFieldsets();
    });
    
    $('#document-delete-button').button().click(function() {
      if (confirm("Are you sure?")) {
        var docid = $(this).attr('data-document-id');
        var docrev = $(this).attr('data-document-rev');
        
        deleteDocument(docid, docrev);
      }
    });
    
    $('#document-view-list > li > b').click(function() {
      $(this).parent('li').toggleClass('collapsed');
    });
  });
}

function deleteDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 204) {
        var title = "Success";
        var body = "Your document was deleted. You may undo this by clicking Edit and then Create as New.";
        
        $('#document-delete-button').hide();
        $('#document-view h2').text("Deleted Document");
        $('#document-view').fadeTo('slow', 0.5);
        
        getIndex();
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Document appears to have been deleted already.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });
}

function resetFields() {
  $('.field').each(function(index) {
    field = $(this);
    thedefault = field.attr('data-field-default');
    
    if (thedefault && thedefault != '') {
      if (field.is('select.multiselect')) {
        field.val(thedefault.split(","));
      } else if (field.is('input.boolean')) {
        field.attr('checked', thedefault == true);
      } else {
        field.val(thedefault);
      }
    } else {
      field.val('');
      field.removeAttr('checked');
    }
  });
}

// Get the index that is displayed in the index pane 
function getIndex(startkey, startid, endkey, endid) {
  var descending = $('#index-descending').val() != "true";
  var url = "documents/index?descending=" + descending;
  var limit = $('#index-limit').val() * 1;
  startkey = startkey || $('#index-startkey').val();
  endkey = endkey || $('#index-endkey').val();
  
  if (startkey) {
    url = url + '&startkey=' + JSON.stringify([startkey]);
    
    if (startid) {
      url = url + '&startkey_id=' + startid;
    }
  }
  
  if (endkey) {
    url = url + '&endkey=' + JSON.stringify([endkey]);
    
    if (endid) {
      url = url + '&endid_id=' + endid;
    }
  }
  
  if (limit) {
    url = url + '&limit=' + (limit + 1);
  }
  
  $.get(url, function(documentIndexHtml) {
    $('#document-index div').html(documentIndexHtml);

    $('#previous-index-page').click(function() {
      var prevkey = $(this).attr('data-endkey');
      var previd = $(this).attr('data-endid');
      
      getIndex(false, false, prevkey, previd);
    });
    
    $('#next-index-page').click(function() {
      var nextkey = $(this).attr('data-startkey');
      var nextid = $(this).attr('data-startid');
      
      getIndex(nextkey, nextid, false, false);
    });
    
    $('.view-document-link').click(function() {
      getDocument(this.hash.slice(1));
    });
  });
}

// Initialize the form in the edit pane
function initEdit() {
  var url = "documents/edit";
  
  $.get(url, function(documentEditHtml) {

    $('#document-edit').html(documentEditHtml);
    $('#edit-tabs').tabs();
    setKeyboardEvents();
    arrangeTabBar();
    initFieldsets();
    initEditButtons();
  });
  
  return true;
}

// This function initializes all the keyboard events for the
// edit pane
function setKeyboardEvents() {
  var isMod = false;
  var modKey = '18';
  var nextKey = '78';
  var prevKey = '80';
  var inputable = 'input, select';
  var t = $('#edit-tabs');
  
  selectInput = function() {
    var cur = t.find('.ui-tabs-selected a').attr('href');
    $(cur).find(inputable + ", textarea").first().focus();
  };
  
  t.keypress(function(event) {
    if (event.keyCode == '13' && $(event.target).is(inputable)) {
      if ($('#save-document-button').attr('disabled')) {
        $('#create-document-button').click();
      } else {
        $('#save-document-button').click();
      }
    }
  });

  $(document).keyup(function(event) {
    if (event.keyCode == modKey) isMod = false;
  }).keydown(function(event) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (event.keyCode == modKey) isMod = true;
    if (event.keyCode == nextKey && isMod) { 
      if (selected < totaltabs - 1) {
        t.tabs('select', selected + 1);
        selectInput();
      } else {
        t.tabs('select', 0);
        selectInput();
      }
    }
    if (event.keyCode == prevKey && isMod) { 
      if (selected != 0) {
        t.tabs('select', selected - 1);
        selectInput();
      } else {
        t.tabs('select', totaltabs - 1);
        selectInput();
      }
    }
  });
}

// This reshapes the tab bar in the edit pane to better
// fit in the available space
function arrangeTabBar() {
  var tabs = $('#tab-list li');
  var containerWidth = $('#tabs-container').width();
  var tabsWidth;
  
  tabsWidth = _(tabs).reduce(function(acc, elem) {
    var width = elem.offsetWidth;
    
    if (isNaN(width)) {
      return acc;
    } else {
      return acc + elem.offsetWidth;
    }
  }, 50);
  
  if (containerWidth < tabsWidth) {
    tabs.switchClass('ui-corner-top', 'ui-corner-all');
    tabs.children().css('padding', '2px');
    tabs.children().css('font-weight', 'normal');
    tabs.parent().css('padding-bottom', '4px');
  }
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
    icons: {primary: "ui-icon-plus"}
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
    icons: {primary: "ui-icon-minus"}
  }).click(function() {
    $(this).parent().remove();
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
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
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
        } else if (req.status == 403) {
          setInvalidError(req);
          saveButton.button('enable');
        } else if (req.status == 409) {
          var body = JSON.parse(req.responseText);
          var title = req.statusText;
          
          flashError(title, body.message);
          saveButton.button('enable');
        }
      }
    });
    
  });
  
  $('#save-document-button').hide();
  $('#save-document-button').attr('disabled', 'true');
  
  return true;
}

// When a form validation error has occurred, this will cause it to
// be displayed
function setInvalidError(req) {
  var body = JSON.parse(req.responseText);
  var title = req.statusText;
  
  var invalid = $('[name=' + body.fieldname + ']');
  var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');
  
  invalidTab.addClass('ui-state-error');
  invalid.addClass('ui-state-error');
  
  flashError(title, body.fieldname + " " + body.message);
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
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    createButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
    postUrl = $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function(req, status) {
        if (req.status == 201) {
          var title = "Success";
          var body = "Your document was created.";
          var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);
          
          $('#save-document-button').hide();
          $('#save-document-button').attr('disabled','true');
          $('.fields').remove();
          initFieldsets();
          getDocument(documentId);
          getIndex();
          flashHighlight(title, body);
          createButton.button('enable');
        } else if (req.status == 403) {
          setInvalidError(req);
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
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    $('#save-document-button').hide();
    $('#save-document-button').attr('disabled','true');
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
  
  root.find('fieldset').each(function(index) {
    var fieldset = $(this);
    var fieldsetId = fieldset.attr('data-fieldset-id');
    var fieldsetMultiple = fieldset.attr('data-fieldset-multiple') == "true";
    var fieldsetCollapse = fieldset.attr('data-fieldset-collapse') == "true";
    var fieldsContainers = $('#container-' + fieldsetId).children('.fields');
    var fieldsetObj = {
      id: fieldsetId,
      multiple: fieldsetMultiple,
      collapse: fieldsetCollapse,
      name: fieldset.attr('data-fieldset-name'),
      label: fieldset.attr('data-fieldset-label'),
      order: fieldset.attr('data-fieldset-order') * 1
    };

    if (!fieldsetMultiple) {
      $.extend(fieldsetObj, fieldsToObject(fieldsContainers.first()));
    } else {
      fieldsetObj.multifields = [];
      
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
      required: field.attr('data-field-required') == "true",
      min: field.attr('data-field-min'),
      max: field.attr('data-field-max'),
      regex: field.attr('data-field-regex'),
      order: field.attr('data-field-order') * 1,
      subcategory: field.attr('data-field-subcategory'),
      value: fieldValue
    };
    
    if (fieldsIndex >= 0) {
      obj.fields[index].index = fieldsIndex;
    }
  });
  
  return obj;
}

// Get the value from a field that not much is known about.
function getFieldValue(field) {
  var fieldValue;
  
  if (field.is('input.boolean')) {
    fieldValue = field.is('input:checkbox:checked');
  } else if (field.is('input.openboolean')) {
    if (field.val() == "true") {
      fieldValue = true;
    } else if (field.val() == "false") {
      fieldValue = false;
    } else {
      fieldValue = null;
    }
  } else if (field.is('input.number')) {
    if (field.val() == '') {
      fieldValue = '';
    } else {
      fieldValue = field.val() * 1;
    }
  } else {
    fieldValue = field.val();
  }
  
  return fieldValue;
}

$(function () {
  getIndex();
  initEdit();
  
  $('#index-filter-form input').keyup(function() {
    getIndex();
  });
  
  $('#index-filter-form select').change(function() {
    getIndex();
  });
  
  $('#panel-toggle li').click(function(e) {
    var panel = '#' + $(this).attr('data-panel');
    
    $(panel).toggle();
  });
});
