// # Formalize
//
// *implicit dependencies:* JSON
//
// Convert JSON to and from an HTML form. Also create forms from JSON
// Schema.

// ## Variable Definitions

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

// ## External Functions

var toForm = function (jsn)
{
  'use strict';

  var obj = tryParseJSON(jsn);

  obj = validateToArg(obj);

  return '';
};

exports.toForm = toForm;
