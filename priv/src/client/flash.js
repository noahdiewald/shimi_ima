// # Brief Notification Messages
//
// *Implicit depends:* DOM, JQuery
//
// Helpers to display notifications.

// ## Internal Functions

// Helper function that handles the displaying and fading of the flashed
// notification.
var f = function (flasher, title, body)
{
  'use strict';

  var fadeout = function ()
  {
    flasher.fadeOut();
  };
  flasher.find('.notification-summary').text(title + ': ');
  flasher.find('.notification-message').text(body);
  var timeout = window.setTimeout(fadeout, 7000);
  flasher.fadeIn();
  flasher.find('.close').click(function ()
  {
    window.clearTimeout(timeout);
    flasher.hide();
  });
};

// # Exported Functions

// Display an error.
var error = function (title, body)
{
  'use strict';

  f($('#notifications-main .ui-state-error'), title, body);

  return true;
};

// Display a notification.
var highlight = function (title, body)
{
  'use strict';

  f($('#notifications-main .ui-state-highlight'), title, body);

  return true;
};

exports.error = error;
exports.highlight = highlight;
