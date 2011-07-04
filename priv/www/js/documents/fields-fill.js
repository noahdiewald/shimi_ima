function fillFieldsets() {
  $('.fieldset-view').each(function(i, fieldset) {
    if (fsInfo("multiple", $(fieldset)) == "true") {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });
  
  afterEditRefresh();
}

function fillMultiFieldsets(vfieldset) {
  vfieldset = $(vfieldset);
  var id = fsInfo("fieldset", vfieldset);
  var container = $('#container-' + id);
  var url = dpath(vfieldset, "fieldset");
  
  container.html('');
  
  vfieldset.find('.multifield').each(function(i, multifield) {
    initFieldset(container, function(fieldset) {
      fillFields($(multifield), fieldset);
    });
  });
}

function fillNormalFieldsets(vfieldset) {
  fillFields($(vfieldset));
}

function fillFields(container, context) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').removeAttr('disabled');
  
  container.find('.field-view').each(function(i, field) {
    var value = $(field).attr('data-field-value');
    var id = $(field).attr('data-field-field');
    
    if (!context) context = $('body');
    
    setFieldValue(context.find('.field[data-field-field=' + id + ']'), value);
  });
}

function setFieldValue(field, value) {
  if (field.is('input.boolean')) {
    field.attr("checked", value == "true");
  } else if (value && field.is('select.multiselect')) {
    field.val(value.split(","));
  } else if (value && (field.is('input.text') || field.is('select.file')))  {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else if (field.is('textarea.textarea')) {
    field.val(decodeURIComponent(value.replace(/\+/g," ")));
  } else {
    field.val(value);
  }
}
