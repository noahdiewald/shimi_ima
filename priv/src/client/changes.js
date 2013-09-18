// # Change Event Handling
//
// *Implicit depends:* DOM, JQuery
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the 'change' events. This is a start and a bit of
// an experiment. It uses the JQuery `on()` function, which was not
// available when I first began programming this application.

// ## Variable Definitions

var searchui = require('./documents/searchui.js');

// ## Exported Functions

// Run to add event listeners to `document`.
var changes = function ()
{
  'use strict';

  // ### Search UI Change Events

  $(document).on('change', '#document-search-exclude', function (e)
  {
    searchui.toggleExclusion();
    return true;
  });

  $(document).on('change', '#document-search-invert', function (e)
  {
    searchui.toggleInversion();
    return true;
  });
};

exports.changes = changes;
