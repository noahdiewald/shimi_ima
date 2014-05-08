function show(doc, req) {
  'use strict';

  var xml = require('lib/xml');
  var root = function () {
    return ['<doc>', '</doc>'];
  };
  var xmlString = xml.to_xml(JSON.stringify(doc), {
    root: root
  });

  return {
    headers: {
      'Content-Type': 'text/xml'
    },
    body: '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + xmlString
  };
}
