// # Json Parse
//
// *implicit dependencies:* JSON
//
// Convert a subset of JSON to an abstract syntax.

// ## Variable Definitions

var r = require('./recurse.js');

// ## Internal Functions

// Get the 'type', which may not correspond to the JavaScript type.
var getType = function (val) {
  'use strict';

  if (val === null || (typeof val === 'string' && val.length <= 32)) {
    return 'string';
  } else if (typeof val === 'string' && val.length > 32) {
    return 'text';
  } else if (typeof val === 'boolean') {
    return 'boolean';
  } else if (typeof val === 'number') {
    return 'number';
  } else if (val instanceof Array) {
    return 'array';
  } else if (val instanceof Object && !(val instanceof Array) && val !== null) {
    return 'object';
  }
};

// If v is null, return 'null', otherwise return v.
var maybeNullToString = function (v) {
  'use strict';

  if (v === null) {
    return 'null';
  } else {
    return v;
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
      value: maybeNullToString(val)
    };
  });
};

// Transform the object into an AST that should be easier to work with
// in templating systems, etc.
var transform = function (obj) {
  'use strict';

  var start = {
    fields: []
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
    return {};
  } else {
    return transform_.t({
      object: obj,
      parent: start,
      key: 'fields'
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
