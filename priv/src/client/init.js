var shimi = {};

// A place to temporarily store global objects
shimi.globals = {};

// functions added to String
String.prototype.isBlank = function () {
  return ((/^\s*$/).test(this) && !(/\S/).test(this) && (this !== null));
};

String.prototype.trim = function () {
  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// functions added to Array
Array.prototype.trimAll = function () {
  return this.map(function (i) {
    return i.trim();
  }).filter(function (i) {
    return !i.match(/^$/);
  });
};

// General UI Stuff
shimi.panelToggle = (function () {
  var mod = {};

  mod.toggler = function (target) {
    var panel;

    if ($(target).attr('data-panel')) {
      panel = $('#' + $(target).attr('data-panel'));
    } else {
      panel = $(target).closest('.panel');
    }

    if (panel.css("display") === "none") {
      panel.css("display", "table-cell");
    } else {
      panel.css("display", "none");
    }

    return mod;
  };

  return mod;
})();