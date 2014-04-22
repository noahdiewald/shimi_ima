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
var jp = require('./json_parse.js');

// ## Internal Functions

// Reject non-strings or strings of length zero.
var validateFromArg = function (html) {
  'use strict';

  if (typeof html !== 'string') {
    throw 'invalid HTML: non-string';
  } else if (html.length === 0) {
    throw 'invalid HTML: ""';
  }

  return html;
};

// Some types of valid JSON are not suitable to use for building a
// form. A non-array object is required.
var validateToArg = function (obj) {
  'use strict';

  var msg = 'cannot build form from: ';

  if (typeof obj === 'string') {
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

// Return a title for the key as a span element.
var spanTitle = function (key, options) {
  'use strict';

  var retval = '';
  var tc = options.titleClass ? ' ' + options.titleClass : '';

  if (key) {
    retval = '<span title="' + key + '" class="span-title' + tc + '">' + key + '</span>';
  } else if (options.arrayElementHandles) {
    retval = '<span class="array-element-handle">' + options.arrayElementHandles + '</span>';
  }

  return retval;
};

// Accumulate HTML strings for a complex type, such as an object or
// array.
var newEither = function (type, key, acc, options) {
  'use strict';

  var id;

  if (options.noObjectIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '" ';
  }

  return insert(spanTitle(key, options) + '<' + type + id + (key ? ' title="' + key + '"' : '') + '>', '</' + type + '></li>', acc);
};

// Accumulate HTML strings for an object.
var newObject = function (key, acc, options) {
  'use strict';

  return newEither('ul', key, acc, options);
};

// Accumulate HTML string for an array.
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

// Process a field.
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

// The descriptive object created by the `transform` function is
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
    // also want to continue if `head` is undefined, which will
    // indicate that we've hit the end of object or array values.
    else if (!depleted && done && (isNotObject || !head)) {
      // Pop the next group of stored field information off the stack
      // where it was previously stored by the 'else' clause below.
      next = stack.pop();

      // This will change the acc depending on `head` information.
      processDescriptField(head, acc, options);

      // This will nest the current acc string values inside the
      // parent.
      acc2 = accInsert(accstack, acc);

      // Use next instead of `head` and `rest`, `rest` was already
      // depleted in this step.
      return _descriptToHtml.r(next[0], next.slice(1), acc2, stack, accstack, id);
    }
    // Unless it is a complex value (such as array or object), process
    // the value and move on to the next field.
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

      // Now use the values to specify `head` and `rest`.
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

// Build an HTML form from JSON.
var simpleToForm = function (obj, options) {
  'use strict';

  var fields = jp.transform(obj);

  fields.obj = obj !== null;

  return descriptToHtml(fields, options);
};

// ## External Functions

var toForm = function (jsn, options) {
  'use strict';

  options = options ? options : {};
  var obj = jp.tryParseJSON(jsn);

  jp.validate(obj);

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
