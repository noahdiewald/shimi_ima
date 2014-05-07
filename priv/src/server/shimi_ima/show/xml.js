function show(doc, req) {
  'use strict';

  var xml_show = require('lib/xml_show');

  return {
    headers: {
      'Content-Type': 'text/xml'
    },
    body: '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + xml_show.to_xml(JSON.stringify(doc))
  };
}
