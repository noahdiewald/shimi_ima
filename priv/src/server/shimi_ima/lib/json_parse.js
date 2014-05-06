// # Json Parse
//
// *implicit dependencies:* JSON
//
// Convert a subset of JSON to an abstract syntax.

// ## Variable Definitions

var r = require('lib/recurse.js');

// ## Internal Functions

// Get the 'type', which may not correspond to the JavaScript type.
var getType = function (val) {
  'use strict';

  var retval;

  if (typeof val === 'string') {
    return 'string';
  } else if (typeof val === 'boolean') {
    return 'boolean';
  } else if (typeof val === 'number') {
    return 'number';
  } else if (val === null) {
    return 'null';
  } else if (val instanceof Array) {
    return 'array';
  } else if (val instanceof Object) {
    return 'object';
  }
};

// Process key value pairs in an object and return an object that
// describes the original object.
var getKeyVals = function (o) {
  'use strict';

  return Object.keys(o).map(function (k) {
    var val = o[k];

    return {
      key: (o instanceof Array) ? false : k,
      index: (o instanceof Array) ? k * 1 : false,
      type: getType(val),
      value: val
    };
  });
};

// Transform the object into an AST that should be easier to work with
// in templating systems, etc.
var transform = function (obj) {
  'use strict';

  var start = {
    root: []
  };

  var transform_ = function (o, rest, accObj, id) {
    var result;
    var keyVals = getKeyVals(o.object);

    result = keyVals.reduce(function (acc, x) {
      if (x.type === 'array' || x.type === 'object') {
        return acc.concat({
          object: x.value,
          key: 'value',
          parent: x
        });
      } else {
        return acc;
      }
    }, []);

    rest = rest.concat(result);
    o.parent[o.key] = keyVals;

    if (rest && rest.length !== 0) {
      return transform_.r(rest[0], rest.slice(1), accObj, id);
    } else {
      return id.r(accObj);
    }
  };

  if (obj === null) {
    // Note that this may seem like an odd choice but it is useful to
    // have as a return value based on the assumptions of the software
    // this module relies on.
    return {};
  } else {
    return transform_.t({
      object: obj,
      parent: start,
      key: 'root'
    }, [], start, r.identity);
  }
};

// Simply a call to JSON.parse with some special error handling.
var tryParseJSON = function (jsn) {
  'use strict';

  var obj;

  try {
    obj = JSON.parse(jsn);
  } catch (e) {
    switch (e.name) {
    case 'SyntaxError':
      e.message = 'invalid JSON: ' + JSON.stringify(jsn);
      throw e;
    default:
      throw e;
    }
  }

  // I've tested this and strangely enough JSON.parse(null) === null
  if (jsn === null) {
    throw new SyntaxError('invalid JSON: null');
  }

  return obj;
};

// Some types of valid JSON are not useful in this context..
var validate = function (obj) {
  'use strict';

  var msg = 'cannot build AST from: ';

  if (typeof obj === 'string') {
    throw msg + 'string';
  } else if (typeof obj === 'number') {
    throw msg + 'number';
  } else if (obj !== null && obj.constructor === Array) {
    throw msg + 'array';
  }

  return obj;
};

// ## External Functions

var parse = function (jsn) {
  'use strict';

  var obj = tryParseJSON(jsn);

  obj = validate(obj);

  return transform(obj);
};

exports.parse = parse;
