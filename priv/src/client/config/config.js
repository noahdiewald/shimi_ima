// # Config Sub-App Init
//
// *Implicit depends:* DOM
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeui = require('./doctypeui.js');
var maintenanceui = require('./maintenanceui.js');
var charsequi = require('./charsequi.js');

// ## Exported Functions

// Run initialization code for the configuration sub-application.
var init = function ()
{
  'use strict';

  doctypeui.init();
  charsequi.init();
  maintenanceui.init();

  return 'config-initialized';
};

exports.init = init;
