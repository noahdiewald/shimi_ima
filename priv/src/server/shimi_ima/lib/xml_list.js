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

  return (item.key ? item.key : 'item');
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

  return [x(item) + item.value + y(item), ''];
};

var complex = function (item, options) {
  'use strict';

  return [x(item), y(item)];
};

var to_xml = function (json) {
  'use strict';

  var funs = {
    context: context,
    root: root,
    simple: simple,
    complex: complex
  };

  return json_to.transform(json, funs);
};

exports.to_xml = to_xml;
