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

  if (obj === null)
  {
    throw msg + 'null';
  }
  else if (typeof obj === 'string')
  {
    throw msg + 'string';
  }
  else if (typeof obj === 'number')
  {
    throw msg + 'number';
  }
  else if (obj.constructor === Array)
  {
    throw msg + 'array';
  }
  else if (Object.keys(obj).length === 0)
  {
    throw msg + 'empty object';
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

var openObject = function (state)
{
  'use strict';

  state.state.push('open-object');
  if (state.acc === 'null')
  {
    state.acc = '';
  }
  state.acc = state.acc + '{';

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

var addTextValue = function (state, text)
{
  'use strict';

  state.acc = state.acc + '"' + text + '"';

  return state;
};

var addKey = function (state, attribs)
{
  'use strict';

  state.acc = addTextValue(state, attribs.name).acc + ':';

  return state;
};

var addValue = function (state, attribs)
{
  'use strict';

  if (attribs.type === 'text')
  {
    addTextValue(state, attribs.value);
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
  addKey(state, attribs);
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
  else
  {
    throw 'invalid form: text outside of textarea' + showState(state);
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
    addComma(state);

    return state;
  });

  return state;
};

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
        openObject(state);
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
      addTextareaValue(state, str);
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

  return obj;
};

var simpleToForm = function (obj)
{
  'use strict';

  var fields = Object.keys(obj).reduce(function (acc, key)
  {
    var val = obj[key];
    var ret = {key: key, val: val};

    if (typeof val === 'string' && val.length <= 32)
    {
      ret.string = true;
    }
    else if (typeof val === 'number')
    {
      ret.number = true;
    }
    else if (typeof val === 'string' && val.length > 32)
    {
      ret.text = true;
    }

    return acc.concat(ret);
  }, []);

  return templates['simple-to-form']({fields: fields});
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
