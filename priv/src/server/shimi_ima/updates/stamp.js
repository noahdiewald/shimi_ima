function update(doc, req) {
  var newDoc = JSON.parse(req.body);
  var stamped = require('lib/update_helpers').stamp(newDoc, doc, req);
  
  if (doc && doc.fieldsets) {
    var changes = require('lib/update_helpers').get_changes(newDoc, doc);
    stamped.doc.changes = changes;
  }

  return [stamped.doc, JSON.stringify(stamped.message)];
}
