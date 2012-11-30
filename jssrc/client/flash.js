shimi.flash = function (title, body) {
  var mod = {};

  var f = function (flasher, title, body) {
    var fadeout = function () {
      flasher.fadeOut();
    };
    flasher.find('.notification-summary').text(title + ": ");
    flasher.find('.notification-message').text(body);
    var timeout = window.setTimeout(fadeout, 7000);
    flasher.fadeIn();
    flasher.find('.close').click(function () {
      window.clearTimeout(timeout);
      flasher.hide();
    });
  };

  mod.error = function () {
    f($('#notifications-main .ui-state-error'), title, body);

    return mod;
  };

  mod.highlight = function () {
    f($('#notifications-main .ui-state-highlight'), title, body);

    return mod;
  };

  return mod;
};