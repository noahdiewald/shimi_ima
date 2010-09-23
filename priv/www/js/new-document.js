function getFields() {
  $('fieldset').each(function(index) {
    var fieldset = $(this);
    var url = fieldset.children('div').first().attr('data-target-path');
    
    $.get(url, function(fields) {
      fieldset.children('.fields').prepend(fields);
    });
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

function refreshFields() {
  getFields();
  initRemoveButton();
}

$(function () {

  refreshFields();
  
  $(".add-button").button({
    icons: {primary: "ui-icon-plus"},
    text: false
  }).click(function() {
    var url = $(this).attr('data-target-path');
    thisButton = $(this);
     
    $.get(url, function(fieldset) {
      thisButton.before(fieldset);
      refreshFields();
    });
  });
  
});