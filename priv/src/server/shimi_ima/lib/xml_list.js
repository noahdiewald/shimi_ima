var xml_shared = require('lib/xml_shared');

var root = function (options) {
  'use strict';

  return ['<row>', '</row>'];
};

var to_xml = function (json) {
  'use strict';

  var funs = {
    root: root,
  };

  return xml_shared.to_xml(json, funs);
};

exports.to_xml = to_xml;
