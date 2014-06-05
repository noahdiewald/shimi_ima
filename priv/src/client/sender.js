// # Send message to `reporter.js` worker.
//
// This has a single function which starts a web work and sends it
// a message.

// ## Exported functions

// This is called by functions when the actions they have performed
// result in a paticular state.
var sender = function (message, arg) {
  'use strict';

  return globals.reporter.postMessage({
    message: message,
    arg: arg
  });
};

exports.sender = sender;
