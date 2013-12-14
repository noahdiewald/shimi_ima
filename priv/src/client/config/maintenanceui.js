// # Maintenance User Interface
//
// *Implicit depends:* DOM
//
// This handles UI elements that are used for maintaining a project.

// ## Variable Definitions

var templates = require('templates.js');
var ajax = require('../ajax.js');
var flash = require('../flash.js');

// ## Exported Functions

// When the upgrade button is pressed in the configuration UI, this
// will carry out the necessary action. It will make an empty `POST`
// to the upgrade path and alert the user that this was done.
var upgradeButton = function () {
  'use strict';

  ajax.post('config/upgrade', false, function () {
    flash.highlight('Task Started', 'Upgrade Project');
  });

  return 'upgrade-initiated';
};

// Initialize and display the interface.
var init = function () {
  'use strict';

  var renderedHTML = templates['config-maintenance']();
  document.getElementById('config-maintenance').insertAdjacentHTML('beforeend', renderedHTML);

  return 'maintenanceui-initialized';
};

exports.init = init;
exports.upgradeButton = upgradeButton;
