exports.stamp = function(newDoc, doc, req) {
  var now = (new Date()).toUTCString();
  var message;

  if (!doc) {
    if (newDoc._id) {
      newDoc.created_at_ = now;
      newDoc.created_by_ = req.userCtx.name;
      message = {
        id: newDoc._id,
        timestamp: newDoc.created_at_,
        user: newDoc.created_by_
      };
    } else {
      newDoc = null;
      message = 'This application expects the document _id in the JSON body';
    }
  } else {
    newDoc.updated_at_ = now;
    newDoc.updated_by_ = req.userCtx.name;
    newDoc.created_at_ = doc.created_at_;
    newDoc.created_by_ = doc.created_by_;

    message = {
      id: newDoc._id,
      timestamp: newDoc.updated_at_,
      user: newDoc.updated_by_,
      changes: newDoc.changes
    };

    newDoc.prev_ = doc._rev;
  }

  return {
    doc: newDoc,
    message: message
  };
};

exports.get_changes = function(newDoc, doc) {

};
