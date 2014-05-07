function list(head, req) {
  'use strict';

  var xml_list = require('lib/xml_list');
  var row;

  start({
    'headers': {
      'Content-Type': 'text/xml'
    }
  });

  send('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
  send('<rows>');

  while (row = getRow()) {
    send(xml_list.to_xml(JSON.stringify(row)));
  }

  send('</rows>');
}
