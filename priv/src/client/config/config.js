// # Config Sub-App Init
//
// *Implicit depends:* DOM, JQuery
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeTab = require('./doctype-tab.js');
var charseqTab = require('./charseq-tab.js').charseqTab;

// ## Exported Functions

// When the upgrade button is pressed in the configuration UI, this
// will carry out the necessary action. It will make an empty `POST`
// to the upgrade path and alert the user that this was done.
//
// TODO: This is basically here until the upgrad functionality is fleshed
// out and gets its own module.
var upgradeButton = function ()
{
  'use strict';

  $.post('config/upgrade');
  window.alert('Upgrade In Progress');
};

// Run initialization code for the configuration sub-application.
var init = function ()
{
  'use strict';

  doctypeTab.init();
  $('#main-tabs').tabs();
  charseqTab.init();
  $('.simple-tabs').tabs();

  return true;
};

exports.upgradeButton = upgradeButton;
exports.init = init;
