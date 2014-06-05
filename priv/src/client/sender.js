// # Send message to `reporter.js` worker.
//
// This has a single function which starts a web work and sends it
// a message.

// ## Variable Definitions

var r = require('receiver');

// ## Exported functions

// This is called by functions when the actions they have performed
// result in a paticular state.
var sender = function (message, arg) {
  'use strict';

  var worker = new Worker('/reporter.js');

  worker.onmessage = function (e) {
    return r.receiver(e.data.message, e.data.arg);
  };

  return worker.postMessage({
    message: message,
    arg: arg
  });
};

exports.sender = sender;
