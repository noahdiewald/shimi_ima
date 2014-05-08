function list(head, req) {
  'use strict';

  var xml = require('lib/xml');
  var row;
  var root = function () {
    return ['<rows>', '</rows>'];
  };

  start({
    'headers': {
      'Content-Type': 'text/xml'
    }
  });

  send('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
  send('<rows>');

  while (row = getRow()) {
    send(xml.to_xml(JSON.stringify(row), {
      root: root
    }));
  }

  send('</rows>');
}
