function update(doc, req) {
  var newDoc = JSON.parse(req.body);
  var stamped = require('lib/update_helpers').stamp(newDoc, doc, req);

  return [stamped.doc, JSON.stringify(stamped.message)];
}
