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
  
});