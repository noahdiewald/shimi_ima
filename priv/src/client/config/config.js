// # Config Sub-App Init
//
// *Implicit depends:* DOM, JQuery
//
// Initialization of the sub-application used to configure the system and
// define doctypes. It also includes code for the upgrade button element,
// for very little reason.

// ## Variable Definitions

var doctypeui = require('./doctypeui.js');
var maintenanceui = require('./maintenanceui.js');
var charsequi = require('./charsequi.js');
var editui = require('./editui.js');
var dispatcher = require('../dispatcher.js').dispatcher;
var Reactor = require('reactorjs');
var Signal = Reactor.Signal;
var Observer = Reactor.Observer;

// ## Internal Functions

// Given a click event, determine what action to take based on the
// click target.
var clickDispatch = function (e)
{
  'use strict';

  var action = dispatcher(
  {
    '.edit-document-link': function (t)
    {
      return t;
    },
    '#maintenance-upgrade-button': function (t)
    {
      maintenanceui.upgradeButton(t);
    }
  });

  action(e);
};

// ## Exported Functions

// Run initialization code for the configuration sub-application.
var init = function ()
{
  'use strict';

  // Click events
  $('body').click(function (e)
  {
    clickDispatch(e);
  });

  editui.init();
  doctypeui.init();
  charsequi.init();
  maintenanceui.init();

  return 'config-initialized';
};

exports.init = init;
