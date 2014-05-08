var json_to = require('lib/json_to');

var context = function (options) {
  'use strict';

  return ['', ''];
};

var root = function (options) {
  'use strict';

  return ['<row>', '</row>'];
};

var getKey = function (item) {
  'use strict';

  var key = item.key;

  // This is more restrictive than the XML standard.
  if (!key) {
    key = 'item';
  } else if (!key.match(/^[a-zA-Z_]/)) {
    key = '_' + key;
  }

  return key;
};

var encodeXML = function (str) {
  'use strict';

  var value = str;

  if (typeof str === 'string') {
    value = str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&apos;');
  }

  return value;
};

var getIndex = function (item) {
  'use strict';

  return (item.key ? '' : ' index="' + item.index + '"');
};

var x = function (item) {
  'use strict';

  return '<' + getKey(item) + getIndex(item) + ' type="' + item.type + '">';
};

var y = function (item) {
  'use strict';

  return '</' + getKey(item) + '>';
};

var simple = function (item, options) {
  'use strict';

  return [x(item) + encodeXML(item.value) + y(item), ''];
};

var complex = function (item, options) {
  'use strict';

  return [x(item), y(item)];
};

var to_xml = function (json, overrides) {
  'use strict';

  overrides = overrides ? overrides : {};

  var funs = {
    context: overrides.context ? overrides.context : context,
    root: overrides.root ? overrides.root : root,
    simple: overrides.simple ? overrides.simple : simple,
    complex: overrides.complex ? overrides.complex : complex
  };

  return json_to.transform(json, funs);
};

exports.to_xml = to_xml;
