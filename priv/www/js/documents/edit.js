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
  var inputable = 'input, select';
  var t = $('#edit-tabs');
  
  selectInput = function() {
    var cur = t.find('.ui-tabs-selected a').attr('href');
    $(cur).find(inputable + ", textarea").first().focus();
  };
  
  $(document).bind('keydown', 'Alt+p', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected != 0) {
      t.tabs('select', selected - 1);
      selectInput();
    } else {
      t.tabs('select', totaltabs - 1);
      selectInput();
    }
    
    return false;
  });
  
  $(document).bind('keydown', 'Alt+n', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected < totaltabs - 1) {
      t.tabs('select', selected + 1);
      selectInput();
    } else {
      t.tabs('select', 0);
      selectInput();
    }
    
    return false;
  });
}

// This reshapes the tab bar in the edit pane to better
// fit in the available space
function arrangeTabBar() {
  var tabs = $('#tab-list li');
  var containerWidth = $('#tabs-container').width();
  var tabsWidth;
  
  tabsWidth = tabs.toArray().reduce(function(acc, elem) {
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
  $(".add-button").button({icons: {primary: "ui-icon-plus"}});
  
  return true;
}

// Initialize the button that removes a fieldset from a multiple fieldset
function initRemoveButton() {  
  return $(".remove-button").button({icons: {primary: "ui-icon-minus"}});
}

// Initialize the save button that is used for already existing documents.
// Hide the button until the form has been filled by an already existing
// document's values. See the after functions above.
function initSaveButton() {
  $('#save-document-button').button({ icons: {primary: "ui-icon-disk"}});
  $('#save-document-button').hide();
  return $('#save-document-button').attr('disabled', 'disabled');
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
  return $("#create-document-button").button({icons: {primary: "ui-icon-document"}});
}

// Initialize the clear button that will rebuild the form fresh.
function initClearButton() {
  return $('#clear-document-button').button({icons: {primary: "ui-icon-refresh"}});
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
  } else if (field.val() && field.is('select.multiselect')) {
    fieldValue = field.val().map(function(v) {
      return decodeURIComponent(v.replace(/\+/g," "));
    });
  } else if (field.val() && field.is('select.select')) {
    fieldValue = decodeURIComponent(field.val().replace(/\+/g," "));
  } else {
    fieldValue = field.val();
  }
  
  return fieldValue;
}

// Functions to run after the form is refreshed or filled.

// This is run after the form is refreshed with new values inserted.
function afterEditRefresh() {
  var saveBttn = $('#save-document-button');
  var editBttn = $('#document-edit-button');
  var sharedAttrs = ['data-document-id', 'data-document-rev'];
  
  sharedAttrs.forEach(function(elem) {
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
