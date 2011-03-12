// Base64 encoding for browsers withough btoa() and atob() (IE, maybe Opera)

if (!window.btoa) window.btoa = base64.encode
if (!window.atob) window.atob = base64.decode


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

// Date Picker

function initDateFields() {
  $(".date").datepicker({dateFormat: "yy-mm-dd"});
}

// Display notifications

function flashError(title, body) {
  $('#notifications-main .ui-state-error .notification-summary').text(title + ": ");
  $('#notifications-main .ui-state-error .notification-message').text(body);
  $('#notifications-main .ui-state-error').fadeIn().delay(7000).fadeOut('slow');
}

function flashHighlight(title, body) {
  $('#notifications-main .ui-state-highlight .notification-summary').text(title + ": ");
  $('#notifications-main .ui-state-highlight .notification-message').text(body);
  $('#notifications-main .ui-state-highlight').fadeIn('slow').delay(7000).fadeOut('slow');
}
 
$(function () {
  $('.notification').hide();
  
  $('#loading').hide().ajaxStart(function() {
    $(this).show();
  }).ajaxStop(function() {
    $(this).hide();
  });
  
  // Buttons
  
  $(".remove-button").button({
    icons: {primary: "ui-icon-minus"},
    text: false
  }).click(function() {
    $(this).parent().remove();
  });
  
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
  
  $(".create-continue-button").button({
    icons: {
      primary: "ui-icon-disk",
      secondary: "ui-icon-arrowthick-1-e"
    }
  });
  
  initDateFields();
  
  // Sortable
  
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      lis.forEach(function(li) {$(li).text($(li).text() + " " + x);});
    }
  });
});