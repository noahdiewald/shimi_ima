// Validation
  
function updateTips(t, tips) {
  tips.text(t).addClass('ui-state-highlight');
  setTimeout(function() {
    tips.removeClass('ui-state-highlight', 1500);
  }, 500);
}

function checkLength(o, n, min, max, tips) {
  if ( o.val().length > max || o.val().length < min ) {
    o.addClass('ui-state-error');
    updateTips("Length of " + n + " must be between " + min + " and " + max + ".", tips);
    return false;
  } else {
    return true;
  }
}

function checkRegexp(o, regexp, n, tips) {
  if ( !( regexp.test( o.val() ) ) ) {
    o.addClass('ui-state-error');
    updateTips(n, tips);
    return false;
  } else {
    return true;
  }
}
  
$(function () {
  // Buttons
  
  $(".help-button").button({
    icons: {primary: "ui-icon-help"},
    text: false
  });
  
  $(".link-button").button({
    icons: {primary: "ui-icon-link"}
  });
  
  $(".edit-button").button({
    icons: {primary: "ui-icon-pencil"}
  });
  
  // Date Picker
  
  $(".date").datepicker();
  
  // Sortable
  
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      lis.forEach(function(li) {$(li).text($(li).text() + " " + x);});
    }
  });
});