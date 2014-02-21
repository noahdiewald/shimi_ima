function update(doc, req) {
  'use strict';

  var newDoc = JSON.parse(req.body);
  var stamped;

  if (newDoc.fieldsets && !newDoc.category) {
    newDoc.changes = require('lib/update_helpers').get_changes(newDoc, doc);
  }
  stamped = require('lib/update_helpers').stamp(newDoc, doc, req);

  return [stamped.doc, JSON.stringify(stamped.message)];
}
