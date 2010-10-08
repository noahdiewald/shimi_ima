function fillFields() {
  $('.fieldset-view').each(function(fieldsetViewIndex, fieldsetView) {
  
    if ($(fieldsetView).attr('data-multiple') == "true") {
    
      var fieldsetId = $(fieldsetView).attr('data-fieldset-view-id');
      var fieldsetContainer = $('.fieldset-container[data-fieldset-id=' + fieldsetId + ']');
      var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                         fieldsetContainer.attr('data-doctype-id'),
                         fieldsetContainer.attr('data-fieldset-id'));
      
      fieldsetContainer.html('');
      
      $(fieldsetView).children('.multifield').each(function(multifieldIndex, multifield) {
        
        initFieldset(fieldsetContainer, url, function(fieldset) {
        
          $(multifield).children().each(function(fieldIndex, field) {
            var value = $(field).attr('data-field-view-value');
            var fieldId = $(field).attr('data-field-view-id');
            
            fieldset.find('[data-field-id=' + fieldId + ']').last().val(value);
          });
        });
      });
      
    } else {
      $(fieldsetView).find('.field-view').each(function(fieldIndex, field) {
        var value = $(field).attr('data-field-view-value');
        var fieldId = $(field).attr('data-field-view-id');
        
        $('[data-field-id=' + fieldId + ']').val(value);
      });
    }
  });
  
  $('#save-document-button').attr('data-document-id', $('#document-edit-button').attr('data-document-id'));
  $('#save-document-button').attr('data-document-rev', $('#document-edit-button').attr('data-document-rev'));
  $('#save-document-button').show();
}

function getDocument(id) {
  var url = "documents/" + id;
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);
    
    $('#document-edit-button').button().click(function() {
      fillFields();
    });
  });
}

function initIndex() {
  var url = "documents/index";
  
  $.get(url, function(documentIndexHtml) {
    $('#document-index').html(documentIndexHtml);

    $('.view-document-link').click(function() {
      getDocument(this.hash.slice(1));
    });
  });
}

function initEdit() {
  var url = "documents/edit";
  
  $.get(url, function(documentEditHtml) {
    $('#document-edit').html(documentEditHtml);

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
            getDocument(documentId);
            initIndex();
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
            initIndex();
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
    
    $('#clear-document-button').button({
      icons: {primary: "ui-icon-refresh"}
    }).click(function() {
      $('#save-document-button').hide();
      $('.fields').remove();
      initFieldsets();
    });
  });
}

function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
}

function initFields(fieldset, url, fieldsetCallback) {
  $.get(url, function(fields) {
    fieldset.prepend(fields);
    if (fieldsetCallback) {
      fieldsetCallback(fieldset);
    }
    initRemoveButton();
  });
}

function initFieldset(fieldsetContainer, url, fieldsetCallback) {
  $.get(url, function(newFieldset) {
    fieldsetContainer.append(newFieldset);
    
    var fieldset = fieldsetContainer.children().last();
    
    initFields(fieldset, url + "/fields", fieldsetCallback);
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
        
        fieldsetObj.multifields[index1] = fieldsToObject(fieldContainer, index1);
      });
    }
    
    obj.fieldsets[index] = fieldsetObj;
  });
  
  return obj;
}

function fieldsToObject(fieldsContainer, fieldsIndex) {
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
    
    if (fieldsIndex >= 0) {
      obj.fields[index].index = fieldsIndex;
    }
  })
  
  return obj;
}

$(function () {
  initIndex();
  initEdit();
});
