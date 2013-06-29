var shimi = {};

// A place to temporarily store global objects
shimi.globals = {};

// functions added to String
String.prototype.isBlank = function () {
  'use strict';

  return ((/^\s*$/).test(this) && !(/\S/).test(this) && (this !== null));
};

String.prototype.trim = function () {
  'use strict';

  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// functions added to Array
Array.prototype.trimAll = function () {
  'use strict';

  return this.map(function (i) {
    return i.trim();
  }).filter(function (i) {
    return !i.match(/^$/);
  });
};

// General UI Stuff
shimi.panelToggle = (function () {
  'use strict';

  var mod = {};

  mod.toggler = function (target) {
    var panel;

    if ($(target).attr('data-panel')) {
      panel = $('#' + $(target).attr('data-panel'));
    } else {
      panel = $(target).closest('.panel');
    }

    if (panel.css('display') === 'none') {
      panel.css('display', 'table-cell');
    } else {
      panel.css('display', 'none');
    }

    return mod;
  };

  return mod;
})();