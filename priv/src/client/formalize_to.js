// # Form to
//
// Convert JSON to an HTML form.

// ## Variable Definitions

var json_to = require('../server/shimi_ima/lib/json_to.js');
var uuid = require('node-uuid');

// ## Internal Functions

var closeElement = function () {
  'use strict';

  return '</li>';
};

// Used for all form elements
var openElement = function (options) {
  'use strict';

  var id;

  if (options.noElementIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<li' + id + '>';
};

// Return a label for a key.
var simpleKey = function (key, options) {
  'use strict';

  var retval = '';

  if (key) {
    if (options.spanLabel) {
      retval = '<span title="' + key + '" class="span-label">' + key + '</span>';
    } else {
      retval = '<label for="' + key + '">' + key + '</label>';
    }
  }

  return retval;
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

// For a complex type, such as an object or array.
var complex = function (item, options) {
  'use strict';

  var id;
  var listType = 'ul';

  if (item.type === 'array') {
    listType = 'ol';
  }

  if (options.noObjectIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '" ';
  }

  return [openElement(options) + spanTitle(item.key, options) + '<' + listType + id + (item.key ? ' title="' + item.key + '"' : '') + '>', '</' + listType + '>' + closeElement(options)];
};

// Longer text input.
var textarea = function (key, value, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<textarea' + id + (key ? 'name="' + key + '"' : '') + '>' + value + '</textarea>';
};

// Could be text or number.
var inputarea = function (key, value, type, options) {
  'use strict';

  var id;

  if (options.noInputIds) {
    id = '';
  } else {
    id = ' id="' + uuid.v4() + '"';
  }

  return '<input' + id + ' type="' + (type === 'number' ? 'number' : 'text') + '" ' + (key ? 'name="' + key + '" ' : '') + 'value="' + value + '"/>';
};

var simple = function (item, options) {
  'use strict';

  var retval = openElement(options) + simpleKey(item.key, options);

  if (item.type === 'string' && item.value.length > 32) {
    retval = retval + textarea(item.key, item.value, options);
  } else {
    retval = retval + inputarea(item.key, item.value, item.type, options);
  }

  return [retval + closeElement(options), ''];
};

var context = function (options) {
  'use strict';

  var retval = ['', ''];

  if (!options.noForm) {
    retval = ['<form>', '</form>'];
  }

  return retval;
};

var root = function () {
  'use strict';

  return ['<ul>', '</ul>'];
};

// ## External Functions

var transform = function (json, options) {
  'use strict';

  var funs = {
    context: context,
    root: root,
    simple: simple,
    complex: complex
  };

  return json_to.transform(json, funs, options);
};

exports.transform = transform;
