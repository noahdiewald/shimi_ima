function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
}

function initFields(fieldset, url) {
  $.get(url, function(fields) {
    fieldset.prepend(fields);
    initRemoveButton();
  });
}

function initFieldset(fieldsetContainer, url) {
  $.get(url, function(newFieldset) {
    fieldsetContainer.append(newFieldset);
    
    var fieldset = fieldsetContainer.children().last();
    
    initFields(fieldset, url + "/fields");
  });
}

function initFieldsets() {
  $('fieldset').each(function(index) {
    var fieldsetContainer = $(this).children('.fieldset-container').first();
    var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                       fieldsetContainer.attr('data-doctype-id'),
                       fieldsetContainer.attr('data-fieldset-id'));
    
    initFieldset(fieldsetContainer, url);
  });
}

function initRemoveButton() {  
  $(".remove-button").button({
    icons: {primary: "ui-icon-minus"},
    text: false
  }).click(function() {
    $(this).parent().remove()
  });
}

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
        
        fieldsetObj.multifields[index1] = fieldsToObject(fieldContainer);
      });
    }
    
    obj.fieldsets[index] = fieldsetObj;
  });
  
  return obj;
}

function fieldsToObject(fieldsContainer) {
  var fields = fieldsContainer.children('.field-container').children('.field');
  var obj = {fields:[]};
  
  fields.each(function(index) {
    var field = $(this);
    var fieldId = field.attr('data-field-id')
    
    obj.fields[index] = {
      id: fieldId,
      name: field.attr('name'),
      label: field.attr('data-field-label'),
      head: field.attr('data-field-head') == "true",
      reversal: field.attr('data-field-reversal') == "true",
      order: field.attr('data-field-order') * 1,
      subcategory: field.attr('data-field-subcategory'),
      value: field.val()
    }
  })
  
  return obj;
}

$(function () {

  initFieldsets();
  
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
  
  $(".create-button").button({
    icons: {primary: "ui-icon-disk"}
  }).click(function() {
    var createButton = $(this);
    var action = createButton.attr('data-action');
    var root = $('#new-document');
    var obj = {
      doctype: createButton.attr('data-doctype-id'),
      description: createButton.attr('data-doctype-description')
    };
    
    createButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
    $.ajax({
      type: "POST",
      url: action,
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
  
});