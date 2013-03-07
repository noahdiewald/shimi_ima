function update(doc, req) {
  var newDoc = JSON.parse(req.body);
  var now = (new Date()).toUTCString();
  var message;

  if (!doc) {
    if (newDoc._id) {
      newDoc.created_at_ = now;
      newDoc.created_by_ = req.userCtx.name;
      message = 'Created at ' + now.toString + ' by ' + req.userCtx.name;
    } else {
      newDoc = null;
      message = 'This application expects the document _id in the JSON body';
    }
  } else {

    newDoc.updated_at_ = now;
    newDoc.updated_by_ = req.userCtx.name;
    newDoc.created_at_ = doc.created_at_;

    if (doc.create_user_) {
      newDoc.created_by_ = doc.create_user_;
    } else {
      newDoc.created_by_ = doc.created_by_;
    }

    message = 'Updated at ' + now.toString + ' by ' + req.userCtx.name;

    newDoc.prev_ = doc._rev;
  }

  return [newDoc, newDoc._id];
}