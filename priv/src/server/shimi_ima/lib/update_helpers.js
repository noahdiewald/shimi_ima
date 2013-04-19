var stamp = function(newDoc, doc, req) {
  var now = (new Date()).toUTCString();
  var message;

  if (!doc) {
    if (newDoc._id) {
      newDoc.created_at_ = now;
      newDoc.created_by_ = req.userCtx.name;
      message = {
        id: newDoc._id,
        timestamp: newDoc.created_at_,
        user: newDoc.created_by_,
        changes: newDoc.changes
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

var get_changes = function(newDoc, doc) {
  // This function is not implemented as efficiently as it could be but
  // I am more concerned with clarity at this point.
  var foldFields;
  var metaInstance = "00000000000000000000000000000000";
  var changes = {};

  if (!doc) {
    changes[metaInstance] = {
      created: true
    };
  } else {
    if (Object.testEnv) {
      foldFields = Object.foldFields;
    } else {
      foldFields = require('lib/fields').fromFieldsetsFold;
    }
  
    var makeChangeObject = function(field, fieldset) {
      var obj = {
        fieldset: fieldset.id,
        fieldsetLabel: fieldset.label,
        fieldsetInstance: fieldset.instance ? fieldset.instance : null,
        field: field.id,
        fieldLabel: field.label
      };

      return obj;
    };
    var oldInstances = foldFields(doc.fieldsets, function(acc, fields, fieldset) {
      fields.forEach(function(field) {
        var obj = makeChangeObject(field, fieldset);
        obj.originalValue = JSON.stringify(field.value);
        acc[field.instance] = obj;
      });
      return acc;
    }, {});
    changes = foldFields(newDoc.fieldsets, function(acc, fields, fieldset) {
      fields.forEach(function(field) {
        var val = JSON.stringify(field.value);
        if (acc[field.instance] === undefined) {
          acc[field.instance] = makeChangeObject(field, fieldset);
          acc[field.instance]["newValue"] = val;
        } else if (acc[field.instance]["originalValue"] === val) {
          delete acc[field.instance];
        } else {
          acc[field.instance]["newValue"] = val;
        }
      });
      return acc;
    }, oldInstances);

    if (doc.deleted_ === true && newDoc.deleted_ !== true) {
      changes[metaInstance] = {
        restored: true
      };
    } else if (doc.deleted_ !== false && newDoc.deleted_ === true) {
      changes[metaInstance] = {
        deleted: true
      };
    }
  }

  if (Object.keys(changes).length === 0) {
    return null;
  } else {
    return changes;
  }
};

exports.get_changes = get_changes;
exports.stamp = stamp;
