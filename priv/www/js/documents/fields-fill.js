function fillFieldsets() {
  $('.fieldset-view').each(function(i, fieldset) {
    if (fsInfo("multiple", $(fieldset))) {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });
  
  afterEditRefresh();

  return true;
}

function fillMultiFieldsets(vfieldset) {
  vfieldset = $(vfieldset);
  var id = fsInfo("fieldset", vfieldset);
  var container = $('.fieldset-container[data-fieldset-id=' + id + ']');
  var url = dpath(vfieldset, "fieldset");
  
  // Clear the container
  container.html('');
  
  fieldset.find('.multifield').each(function(i, multifield) {
    initFieldset(container, url, function(fieldset) {
      fillFields($(multifield), fieldset);
    });
  });
  
  return true;
}

function fillNormalFieldsets(vfieldset) {
  fillFields($(vfieldset));
  
  return true;
}

function fillFields(container, context) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').removeAttr('disabled');
  
  container.find('.field-view').each(function(i, field) {
    var value = $(field).attr('data-field-view-value');
    var id = $(field).attr('data-field-view-id');
    
    if (!context) context = $('body');
    
    setFieldValue(context.find('.field[data-field-field=' + id + ']'), value);
  });
  
  return true;
}

function setFieldValue(field, value) {
  if (field.is('input.boolean')) {
    field.attr("checked", value == "true");
  } else if (value && field.is('select.multiselect')) {
    field.val(value.split(","));
  } else if (value && (field.is('input.text')))  {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else if (field.is('textarea.textarea')) {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else {
    field.val(value);
  }
  
  return true;
}
