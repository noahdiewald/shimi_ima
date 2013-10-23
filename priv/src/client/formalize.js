// # Formalize
//
// *implicit dependencies:* JSON
//
// Convert JSON to and from an HTML form. Also create forms from JSON
// Schema.

// ## Variable Definitions

var r = require('./recurse.js');
var templates = require('templates.js');
var htmlparser = require('htmlparser2');
var console = require('console');

// ## Internal Functions

var validateFromArg = function (html)
{
  'use strict';

  if (typeof html !== 'string')
  {
    throw 'invalid HTML: non-string';
  }
  else if (html.length === 0)
  {
    throw 'invalid HTML: ""';
  }

  return html;
};

var validateToArg = function (obj)
{
  'use strict';

  var msg = 'cannot build form from: ';

  if (typeof obj === 'string' && obj !== null)
  {
    throw msg + 'string';
  }
  else if (typeof obj === 'number')
  {
    throw msg + 'number';
  }
  else if (obj !== null && obj.constructor === Array)
  {
    throw msg + 'array';
  }

  return obj;
};

var curr = function (state)
{
  'use strict';

  return state.state[state.state.length - 1];
};

var showState = function (state)
{
  'use strict';

  return ' [' + state.state.join(',') + ']';
};

var openForm = function (state)
{
  'use strict';

  if (curr(state) === 'start')
  {
    state.state.push('open');
    state.acc = 'null';
  }
  else
  {
    throw 'invalid form: only one form allowed' + showState(state);
  }

  return state;
};

var addTextValue = function (state, text)
{
  'use strict';

  state.acc = state.acc + '"' + text + '"';

  return state;
};

var addKey = function (state, name)
{
  'use strict';

  state.acc = addTextValue(state, name).acc + ':';

  return state;
};

var addComma = function (state)
{
  'use strict';

  if (curr(state) === 'need-comma')
  {
    state.acc = state.acc + ',';
  }
  else
  {
    state.state.push('need-comma');
  }

  return state;
};

var openObject = function (state, attribs)
{
  'use strict';

  if (state.acc === 'null')
  {
    state.acc = '';
  }

  if (attribs.title)
  {
    addComma(state);
    addKey(state, attribs.title);
  }

  state.state.push('open-object');

  state.acc = state.acc + '{';

  return state;
};

var openArray = function (state, attribs)
{
  'use strict';

  if (attribs.title)
  {
    addComma(state);
    addKey(state, attribs.title);
  }

  state.state.push('open-array');

  state.acc = state.acc + '[';

  return state;
};

var addCorrectlyTypedValue = function(state, value)
{
  'use strict';

  switch (value)
  {
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

var addValue = function (state, attribs)
{
  'use strict';

  addComma(state);

  if (attribs.name)
  {
    addKey(state, attribs.name);
  }

  if (attribs.type === 'text')
  {
    addCorrectlyTypedValue(state, attribs.value);
  }
  else if (attribs.type === 'number')
  {
    state.acc = state.acc + attribs.value;
  }

  return state;
};

var openTextareaValue = function (state, attribs)
{
  'use strict';

  addComma(state);
  addKey(state, attribs.name);
  state.state.push('open-text');

  return state;
};

var addTextareaValue = function (state, str)
{
  'use strict';

  if (curr(state) === 'open-text')
  {
    state.state.pop();
    addTextValue(state, str);
  }

  return state;
};

var noComma = function (state)
{
  'use strict';

  if (curr(state) === 'need-comma')
  {
    state.state.pop();
  }

  return state;
};

var klose = function (state, targ, callback)
{
  'use strict';

  var current;

  noComma(state);
  current = curr(state);

  if (current === targ)
  {
    callback(state);
  }
  else
  {
    throw 'invalid form: tag mismatch' + showState(state);
  }

  return state;
};

var closeForm = function (state)
{
  'use strict';

  klose(state, 'open', function (state)
  {
    state.state = ['done'];

    return state;
  });

  return state;
};

var closeObject = function (state)
{
  'use strict';

  klose(state, 'open-object', function (state)
  {
    state.state.pop();
    state.acc = state.acc + '}';

    return state;
  });

  return state;
};

var closeArray = function (state)
{
  'use strict';

  klose(state, 'open-array', function (state)
  {
    state.state.pop();
    state.acc = state.acc + ']';

    return state;
  });

  return state;
};

// Main HTML parsing function. It uses the helper functions openForm,
// openObject, addValue and openTextareaValue.
var tryParseHTML = function (html)
{
  'use strict';

  var state = {state: ['start'], acc: ''};
  var parser;

  parser = new htmlparser.Parser(
  {
    onopentag: function(name, attribs)
    {
      switch (name)
      {
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
    ontext: function(str)
    {
      if (!str.match(/^\s+$/))
      {
        addTextareaValue(state, str);
      }
    },
    onclosetag: function(tagname)
    {
      switch (tagname)
      {
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

  switch (state.state.pop())
  {
  case 'start':
    throw 'invalid form: no form found' + showState(state);
  case 'open':
    throw 'invalid form: no closing tag' + showState(state);
  default:
    return state.acc;
  }
};

// Simply a call to JSON.parse with some special error handling.
var tryParseJSON = function (jsn)
{
  'use strict';

  var obj;

  try
  {
    obj = JSON.parse(jsn);
  }
  catch (e)
  {
    switch (e.name)
    {
    case 'SyntaxError':
      e.message = 'invalid JSON: ' + JSON.stringify(jsn);
      throw e;
    default:
      throw e;
    }
  }

  // I've tested this and strangely enough JSON.parse(null) === null
  if (jsn === null)
  {
    throw new SyntaxError('invalid JSON: null');
  }

  return obj;
};

// If v is null, return 'null', otherwise return v.
var maybeNullToString = function (v)
{
  'use strict';

  if (v === null)
  {
    return 'null';
  }
  else
  {
    return v;
  }
};

// Get the 'type', which may not correspond to the JavaScript type.
var getType = function (val)
{
  'use strict';

  if (val === null || (typeof val === 'string' && val.length <= 32))
  {
    return 'string';
  }
  else if (typeof val === 'string' && val.length > 32)
  {
    return 'text';
  }
  else if (typeof val === 'boolean')
  {
    return 'boolean';
  }
  else if (typeof val === 'number')
  {
    return 'number';
  }
  else if (val instanceof Array)
  {
    return 'array';
  }
  else if (val instanceof Object && !(val instanceof Array) && val !== null)
  {
    return 'object';
  }
};

// Process key value pairs in an object and return an object that
// describes the original object.
var getKeyVals = function (o)
{
  'use strict';

  return Object.keys(o).map(function (k)
  {
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
var transform = function (obj)
{
  'use strict';

  var start = {fields: []};

  var transform_ = function (o, rest, accObj, id)
  {
    var result;
    var keyVals = getKeyVals(o.object);

    result = keyVals.reduce(function (acc, x)
    {
      if (x.type === 'array' || x.type === 'object')
      {
        return acc.concat({object: x.value, key: 'value', parent: x});
      }
      else
      {
        return acc;
      }
    }, []);

    rest = rest.concat(result);
    o.parent[o.key] = keyVals;

    if (rest && rest.length !== 0)
    {
      return transform_.r(rest[0], rest.slice(1), accObj, id);
    }
    else
    {
      return id.r(accObj);
    }
  };

  if (obj === null)
  {
    return {};
  }
  else
  {
    return transform_.t({object: obj, parent: start, key: 'fields'}, [], start, r.identity);
  }
};

// Insert the strings in the acc object.
var insert = function (left, right, acc)
{
  'use strict';

  acc.left = acc.left + left;
  acc.right = right + acc.right;

  return acc;
};

// Return a label for a key.
var label = function (key, acc)
{
  'use strict';

  if (key)
  {
    acc = insert('<label for="' + key + '">' + key + '</label>', '', acc);
  }

  return acc;
};

// For a labeled object.
var newObject = function (key, acc)
{
  'use strict';

  return insert('<ul' + (key ? ' title="' + key + '"' : '') + '>', '</ul></li>', acc);
};

// For a labeled array.
var newArray = function (key, acc)
{
  'use strict';

  return insert('<ol' + (key ? ' title="' + key + '"' : '') + '>', '</ol></li>', acc);
};

// When an item has a value
var hasValue = function (acc)
{
  'use strict';

  return insert('<li>', '', acc);
};

// Longer text input.
var textarea = function (key, value, acc)
{
  'use strict';

  return insert('<textarea ' + (key ? 'name="' + key + '"' : '') + '>' + value + '</textarea></li>', '', acc);
};

// Could be text or number.
var inputarea = function (key, value, type, acc)
{
  'use strict';

  return insert('<input type="' + (type === 'number' ? 'number' : 'text') + '" ' + (key ? 'name="' + key + '" ' : '') + 'value="' + value + '"/></li>', '', acc);
};

// Process the field.
var processDescriptField = function (fs, acc)
{
  'use strict';

  if (fs && fs.value !== undefined)
  {
    hasValue(acc);

    if (fs.type && fs.type !== 'array' && fs.type !== 'object')
    {
      label(fs.key, acc);
    }
    else if (fs.type && fs.type === 'object')
    {
      newObject(fs.key, acc);
    }
    else if (fs.type && fs.type === 'array')
    {
      newArray(fs.key, acc);
    }

    if (fs.type === 'text')
    {
      textarea(fs.key, fs.value, acc);
    }
    else if (fs.type !== 'object' && fs.type !== 'array')
    {
      inputarea(fs.key, fs.value, fs.type, acc);
    }
  }

  return acc;
};

// Pop the accstack and insert the current acc left and right on the
// left of the poped object.
var accInsert = function (accstack, acc)
{
  'use strict';

  var acc2 = accstack.pop();
  acc2.left = acc2.left + acc.left + acc.right;

  return acc2;
};

// The descriptive object created by the transform function is
// converted to HTML.
var descriptToHtml = function (obj)
{
  'use strict';

  var acc = {left: '<form>', right: '</form>'};
  var result;

  var _descriptToHtml = function (fs, fsrest, acc, stack, accstack, id)
  {
    var isNotObject = fs && (fs.type !== 'array' && fs.type !== 'object');
    var done = fsrest.length === 0;// && fs && (isNotObject || fs.value.length === 0);
    var depleted = stack.length === 0;
    var next;
    var acc2;
    console.error({notObj:isNotObject,done:done,depleted:depleted});
    console.error(stack);
    // There are no more fields and the value doesn't need to be
    // descended, so just return.
    if (!fs && done && depleted)
    {
      processDescriptField(fs, acc);

      if (accstack.length !== 0)
      {
        acc = accInsert(accstack, acc);
      }

      return id.r(acc);
    }
    // If there is more on the stack, process it
    else if (!depleted && done && (isNotObject || !fs))
    {
      next = stack.pop();

      // This will change the acc depending on fs information.
      processDescriptField(fs, acc);

      acc2 = accInsert(accstack, acc);

      return _descriptToHtml.r(next[0], next.slice(1), acc2, stack, accstack, id);
    }
    // Unless it is a complex value, move on to the next field.
    else if (isNotObject)
    {
      processDescriptField(fs, acc);

      return _descriptToHtml.r(fsrest[0], fsrest.slice(1), acc, stack, accstack, id);
    }
    // Otherwise descend the complex value.
    else
    {
      acc2 = {left: '', right: ''};

      stack.push(fsrest);
      accstack.push(acc);

      processDescriptField(fs, acc2);

      return _descriptToHtml.r(fs.value[0], fs.value.slice(1), acc2, stack, accstack, id);
    }
  };

  // When the original object isn't null and there are fields.
  if (obj.obj && obj.fields)
  {
    insert('<ul>', '</ul>', acc);

    // If there is more than just an empty list of fields.
    if (obj.fields && obj.fields.length > 0)
    {
      result = _descriptToHtml.t(obj.fields[0], obj.fields.slice(1), acc, [], [], r.identity);
    }
  }

  return acc.left + acc.right;
};

// This is essentially the default simple form building function.
var simpleToForm = function (obj)
{
  'use strict';

  var fields = transform(obj);

  fields.obj = obj !== null;

  //return templates['simple-to-form'](fields);
  return descriptToHtml(fields);
};

// ## External Functions

var toForm = function (jsn)
{
  'use strict';

  var obj = tryParseJSON(jsn);

  validateToArg(obj);

  return simpleToForm(obj);
};

var fromForm = function (html)
{
  'use strict';

  var json;

  validateFromArg(html);
  json = tryParseHTML(html);

  return json;
};

exports.toForm = toForm;
exports.fromForm = fromForm;
