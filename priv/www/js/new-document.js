$(function () {
  $('fieldset').each(function(index) {
    var fieldset = $(this);
    var fieldsetId = fieldset.attr('data-fieldset-id');
    
    $.get("../fieldsets/" + fieldsetId + "/fields", function(fields) {
      fieldset.children('.fields').html(fields);
    });
  });
});