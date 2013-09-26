// # Formalize
//
// *implicit dependencies:* JSON
//
// Convert JSON to and from an HTML form. Also create forms from JSON
// Schema.

// ## Variable Definitions

var r = require('./recurse.js');

// ## Internal Functions

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
    var ret = acc;

    if (typeof val === 'string' && val.length <= 32)
    {
      ret = acc + '<li><label for="' + key + '">' + key + '</label> <input type="text" name="' + key + '" value="' + val + '"/></li>';
    }

    return ret;
  }, '');

  return '<form><ul>' + fields + '</ul></form>';
};

// ## External Functions

var toForm = function (jsn)
{
  'use strict';

  var obj = tryParseJSON(jsn);

  obj = validateToArg(obj);

  return simpleToForm(obj);
};

exports.toForm = toForm;
