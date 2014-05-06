// # Handle Ajax Requests
//
// *Implicit depends:* DOM
//
// Ajax helpers and request behavior standardization -- such as
// displaying a spinner.

// ## Variable Definitions

var flash = require('flash');

// ## Internal Functions

// The spinner element.
var spinner = function () {
  'use strict';

  return document.getElementById('loading');
};

// Called when request is sent.
var ajaxStart = function () {
  'use strict';

  spinner().style.display = 'block';

  return 'ajax-started';
};

// Stop the spinner when request is complete.
var ajaxStop = function () {
  'use strict';

  spinner().style.display = 'none';

  return 'ajax-stopped';
};

var makeMessage = function (response) {
  'use strict';

  var retval = response.fieldname ? response.fieldname + ' ' : '';

  return retval + response.message;
};

// Run on request completion with callback and default behavior in
// case of common errors.
var complete = function (req, callback) {
  'use strict';

  if (req.status >= 200 && req.status < 300 && callback) {
    callback(req);
  } else if (req.status === 500) {
    flash.error('Unknown Server Error', 'Please report that you received this message');
  } else if (req.status >= 400) {
    var msg;

    if (req.response && typeof req.response === 'string') {
      msg = makeMessage(JSON.stringify(req.response));
    } else if (req.response && req.response instanceof Object) {
      msg = makeMessage(req.response);
    } else if (req.status >= 404) {
      msg = 'The document was not found on the server.';
    } else {
      msg = 'That is all.';
    }

    flash.error(req.statusText, msg);
  }

  return 'ajax-complete';
};

// Returns an `onreadystatechange` handler.
var stateChange = function (req, callback) {
  'use strict';

  return function () {
    switch (req.readyState) {
    case 1:
      return ajaxStart();
    case 4:
      ajaxStop();
      return complete(req, callback);
    default:
      return 'waiting';
    }
  };
};

// Convert object to JSON if needed.
var processObject = function (obj) {
  'use strict';

  if (obj instanceof Object) {
    return JSON.stringify(obj);
  } else if (typeof obj === 'string') {
    return obj;
  } else {
    return '';
  }
};

// ## Exported Functions

// Perform an Ajax action with a URL, object to be translated to JSON,
// an HTTP method and a function to be run on completion.
var send = function (url, obj, method, callback) {
  'use strict';

  var dataObj = processObject(obj);
  var req = new XMLHttpRequest();

  req.onreadystatechange = stateChange(req, callback);
  req.open(method, url);
  req.responseType = 'json';
  req.setRequestHeader('Content-Type', 'application/json');
  req.setRequestHeader('Accept', 'application/json');

  req.send(dataObj);

  return true;
};

// Simplified `send` for GET requests.
var get = function (url, callback) {
  'use strict';

  return send(url, false, 'GET', callback);
};

// Simplified `send` for DELETE requests.
var del = function (url, callback) {
  'use strict';

  return send(url, false, 'DELETE', callback);
};

// Simplified `send` for POST requests.
var post = function (url, obj, callback) {
  'use strict';

  return send(url, obj, 'POST', callback);
};

// Simplified `send` for PUT requests.
var put = function (url, obj, callback) {
  'use strict';

  return send(url, obj, 'PUT', callback);
};

// Perform an Ajax GET action, expecting HTML, which is the old way.
var legacyHTMLGet = function (url, callback) {
  'use strict';

  var req = new XMLHttpRequest();

  req.open('GET', url);

  req.onreadystatechange = stateChange(req, callback);

  req.send();

  return true;
};

exports.send = send;
exports.post = post;
exports.put = put;
exports.del = del;
exports.get = get;
exports.legacyHTMLGet = legacyHTMLGet;
