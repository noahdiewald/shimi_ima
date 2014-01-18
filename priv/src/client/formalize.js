// # Formalize
//
// *implicit dependencies:* JSON
//
// Convert JSON to and from an HTML form. Also create forms from JSON
// Schema.

// ## Variable Definitions

var r = require('./recurse.js');
var htmlparser = require('htmlparser2');
var uuid = require('node-uuid');

// ## Internal Functions

var validateFromArg = function (html) {
  'use strict';

  if (typeof html !== 'string') {
    throw 'invalid HTML: non-string';
  } else if (html.length === 0) {
    throw 'invalid HTML: ""';
  }

  return html;
};

var validateToArg = function (obj) {
  'use strict';

  var msg = 'cannot build form from: ';

  if (typeof obj === 'string' && obj !== null) {
    throw msg + 'string';
  } else if (typeof obj === 'number') {
    throw msg + 'number';
  } else if (obj !== null && obj.constructor === Array) {
    throw msg + 'array';
  }

  return obj;
};

var curr = function (state) {
  'use strict';

  return state.state[state.state.length - 1];
};

var showState = function (state) {
  'use strict';

  return ' [' + state.state.join(',') + ']';
};

var openForm = function (state) {
  'use strict';

  if (curr(state) === 'start') {
    state.state.push('open');
    state.acc = 'null';
  } else {
    throw 'invalid form: only one form allowed' + showState(state);
  }

  return state;
};

var addTextValue = function (state, text) {
  'use strict';

  state.acc = state.acc + '"' + text.replace(/\n/g, '\\n') + '"';

  return state;
};

var addKey = function (state, name) {
  'use strict';

  state.acc = addTextValue(state, name).acc + ':';

  return state;
};

var addComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.acc = state.acc + ',';
  } else {
    state.state.push('need-comma');
  }

  return state;
};

var openObject = function (state, attribs) {
  'use strict';

  if (state.acc === 'null') {
    state.acc = '';
  }

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-object');

  state.acc = state.acc + '{';

  return state;
};

var openArray = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-array');

  state.acc = state.acc + '[';

  return state;
};

var addCorrectlyTypedValue = function (state, value) {
  'use strict';

  switch (value) {
  case 'null':
  case 'true':
  case 'false':
    state.acc = state.acc + value;
    break;
  default:
    addTextValue(state, value);
  }

  return state;
};

var addValue = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.name) {
    addKey(state, attribs.name);
  }

  if (attribs.type === 'text') {
    addCorrectlyTypedValue(state, attribs.value);
  } else if (attribs.type === 'number') {
    state.acc = state.acc + attribs.value;
  }

  return state;
};

var openTextareaValue = function (state, attribs) {
  'use strict';

  addComma(state);
  addKey(state, attribs.name);
  state.state.push('open-text');

  return state;
};

var addTextareaValue = function (state, str) {
  'use strict';

  if (curr(state) === 'open-text') {
    state.state.pop();
    addTextValue(state, str);
  }

  return state;
};

var noComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.state.pop();
  }

  return state;
};

var klose = function (state, targ, callback) {
  'use strict';

  var current;

  noComma(state);
  current = curr(state);

  if (current === targ) {
    callback(state);
  } else {
    throw 'invalid form: tag mismatch' + showState(state);
  }

  return state;
};

var closeForm = function (state) {
  'use strict';

  klose(state, 'open', function (state) {
    state.state = ['done'];

    return state;
  });

  return state;
};

var closeObject = function (state) {
  'use strict';

  klose(state, 'open-object', function (state) {
    state.state.pop();
    state.acc = state.acc + '}';

    return state;
  });

  return state;
};

var closeArray = function (state) {
  'use strict';

  klose(state, 'open-array', function (state) {
    state.state.pop();
    state.acc = state.acc + ']';

    return state;
  });

  return state;
};

// Main HTML parsing function. It uses the helper functions openForm,
// openObject, addValue and openTextareaValue.
var tryParseHTML = function (html) {
  'use strict';

  var state = {
    state: ['start'],
    acc: ''
  };
  var parser;

  parser = new htmlparser.Parser({
    onopentag: function (name, attribs) {
      switch (name) {
      case 'form':
        openForm(state);
        break;
      case 'ul':
        openObject(state, attribs);
        break;
      case 'ol':
        openArray(state, attribs);
        break;
      case 'input':
        addValue(state, attribs);
        break;
      case 'textarea':
        openTextareaValue(state, attribs);
        break;
      }
    },
    ontext: function (str) {
      if (!str.match(/^\s+$/)) {
        addTextareaValue(state, str);
      }
    },
    onclosetag: function (tagname) {
      switch (tagname) {
      case 'form':
        closeForm(state);
        break;
      case 'ul':
        closeObject(state);
        break;
      case 'ol':
        closeArray(state);
        break;
      }
    }
  });

  parser.write(html);
  parser.end();

  switch (state.state.pop()) {
  case 'start':
    throw 'invalid form: no form found' + showState(state);
  case 'open':
    throw 'invalid form: no closing tag' + showState(state);
  default:
    return state.acc;
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

// If v is null, return 'null', otherwise return v.
var maybeNullToString = function (v) {
  'use strict';

  if (v === null) {
    return 'null';
  } else {
    return v;
  }
};

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

// Transform the object into an object that will be easier to convert
// into HTML.
//
// NOTE: This was done during a first pass when I thought that I might
// be using a specific templating system instead of an additional
// recursive function to do the HTML rendering. This is probably an
// unnecessary step at this point.
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

// Insert the strings in the acc object.
var insert = function (left, right, acc) {
  'use strict';

  acc.left = acc.left + left;
  acc.right = right + acc.right;

  return acc;
};

// Return a label for a key.
var label = function (key, acc, options) {
  'use strict';

  if (key) {
    if (options.spanLabel) {
      acc = insert('<span title="' + key + '" class="span-label">' + key + '</span>', '', acc);
    } else {
      acc = insert('<label for="' + key + '">' + key + '</label>', '', acc);
    }
  }

  return acc;
};

var spanTitle = function (key) {
  'use strict';

  var retval = '';

  if (key) {
    retval = '<span title="' + key + '" class="span-title">' + key + '</span>';
  }

  return retval;
};

var newEither = function (type, key, acc, options) {
  'use strict';

  var id;

  if (options.noObjectIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '" ';
  }

  return insert(spanTitle(key) + '<' + type + id + (key ? ' title="' + key + '"' : '') + '>', '</' + type + '></li>', acc);
};

// For an object.
var newObject = function (key, acc, options) {
  'use strict';

  return newEither('ul', key, acc, options);
};

// For an array.
var newArray = function (key, acc, options) {
  'use strict';

  return newEither('ol', key, acc, options);
};

// When an item has a value
var hasValue = function (acc, options) {
  'use strict';

  var id;

  if (options.noElementIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return insert('<li' + id + '>', '', acc);
};

// Longer text input.
var textarea = function (key, value, acc, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return insert('<textarea' + id + (key ? 'name="' + key + '"' : '') + '>' + value + '</textarea></li>', '', acc);
};

// Could be text or number.
var inputarea = function (key, value, type, acc, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return insert('<input' + id + ' type="' + (type === 'number' ? 'number' : 'text') + '" ' + (key ? 'name="' + key + '" ' : '') + 'value="' + value + '"/></li>', '', acc);
};

// Process the field.
var processDescriptField = function (fs, acc, options) {
  'use strict';

  if (fs && fs.value !== undefined) {
    hasValue(acc, options);

    if (fs.type && fs.type !== 'array' && fs.type !== 'object') {
      label(fs.key, acc, options);
    } else if (fs.type && fs.type === 'object') {
      newObject(fs.key, acc, options);
    } else if (fs.type && fs.type === 'array') {
      newArray(fs.key, acc, options);
    }

    if (fs.type === 'text') {
      textarea(fs.key, fs.value, acc, options);
    } else if (fs.type !== 'object' && fs.type !== 'array') {
      inputarea(fs.key, fs.value, fs.type, acc, options);
    }
  }

  return acc;
};

// Pop the accstack and insert the current acc left and right on the
// left of the poped object.
var accInsert = function (accstack, acc) {
  'use strict';

  var acc2 = accstack.pop();
  acc2.left = acc2.left + acc.left + acc.right;

  return acc2;
};

// The descriptive object created by the transform function is
// converted to HTML.
var descriptToHtml = function (obj, options) {
  'use strict';

  var acc = {};
  var result;

  if (options.noForm) {
    acc = {
      left: '',
      right: ''
    };
  } else {
    acc = {
      left: '<form>',
      right: '</form>'
    };
  }

  // This will recurse the descriptive object. The head variable is
  // the current portion of the object to process. The rest variable
  // is the remaining portion. The acc is an accumulator that stores
  // the right and left ends of a string that is built up during
  // processing. Stack is used to save state while descending complex
  // objects. Accstack is used to store acc context when
  // descending. Id is a reference to the identity function to define
  // the base case.
  var _descriptToHtml = function (head, rest, acc, stack, accstack, id) {
    var isNotObject = head && (head.type !== 'array' && head.type !== 'object');
    var done = rest.length === 0;
    var depleted = stack.length === 0;
    var next;
    var acc2;

    // There are no more fields, the stack is depleted and the value
    // doesn't need to be descended, so return the accumulator.
    if (!head && done && depleted) {
      return id.r(acc);
    }
    // If there is more on the stack, process it. We'll want to ignore
    // objects so that they can be handled in the 'else' clause. We'll
    // also want to continue if fs is undefined, which will indicate
    // that we've hit the end of object or array values.
    else if (!depleted && done && (isNotObject || !head)) {
      // Pop the next group of stored fs's off the stack where they
      // were previously stored by the 'else' clause below.
      next = stack.pop();

      // This will change the acc depending on fs information.
      processDescriptField(head, acc, options);

      // This will nest the current acc string values inside the
      // parent.
      acc2 = accInsert(accstack, acc);

      // Use next instead of fs and fsrest, fsrest was already
      // depleted in this step.
      return _descriptToHtml.r(next[0], next.slice(1), acc2, stack, accstack, id);
    }
    // Unless it is a complex value, process the value and move on to
    // the next field.
    else if (isNotObject) {
      processDescriptField(head, acc, options);

      return _descriptToHtml.r(rest[0], rest.slice(1), acc, stack, accstack, id);
    }
    // Otherwise descend the complex value.
    else {
      acc2 = {
        left: '',
        right: ''
      };

      // Push the remaining values onto the stack so they will be
      // processed later.
      stack.push(rest);
      // Push the accumulated left and right strings on to the
      // stack. This will allow them to be pulled out to nest acc2
      // later on.
      accstack.push(acc);

      processDescriptField(head, acc2, options);

      // Now use the values to specify fs and fsrest.
      return _descriptToHtml.r(head.value[0], head.value.slice(1), acc2, stack, accstack, id);
    }
  };

  // When the original object isn't null and there are fields.
  if (obj.obj && obj.fields) {
    insert('<ul>', '</ul>', acc);

    // If there is more than just an empty list of fields.
    if (obj.fields && obj.fields.length > 0) {
      result = _descriptToHtml.t(obj.fields[0], obj.fields.slice(1), acc, [], [], r.identity);
    }
  }

  return acc.left + acc.right;
};

// This is essentially the default simple form building function.
var simpleToForm = function (obj, options) {
  'use strict';

  var fields = transform(obj, options);

  fields.obj = obj !== null;

  return descriptToHtml(fields, options);
};

// ## External Functions

var toForm = function (jsn, options) {
  'use strict';

  var obj = tryParseJSON(jsn, options);
  options = options ? options : {};

  validateToArg(obj, options);

  return simpleToForm(obj, options);
};

var fromForm = function (html) {
  'use strict';

  var json;

  validateFromArg(html);
  json = tryParseHTML(html);

  return json;
};

exports.toForm = toForm;
exports.fromForm = fromForm;
