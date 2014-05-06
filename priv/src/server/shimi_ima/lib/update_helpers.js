var get_head_values = function (d) {
  'use strict';

  return d['head'].map(function (x) {
    var h = d.index[x];
    if (typeof h[0] === 'string') {
      return [JSON.stringify(h[1])];
    } else {
      return h.map(function (y) {
        return [JSON.stringify(y[1])];
      });
    }
  });
};

var stamp = function (newDoc, doc, req) {
  'use strict';

  var now = (new Date()).toUTCString();
  var message = {
    document_id: newDoc._id,
    doctype: newDoc.doctype,
    changes: newDoc.changes
  };

  if (newDoc.head && !newDoc.category) {
    message.head_ids = newDoc.head;
    message.head_values = get_head_values(newDoc);
  }

  if (!doc) {
    if (newDoc._id) {
      newDoc.created_at_ = now;
      newDoc.created_by_ = req.userCtx.name;
      message.timestamp = newDoc.created_at_;
      message.user = newDoc.created_by_;
    } else {
      newDoc = null;
      message = 'This application expects the document _id in the JSON body';
    }
  } else {
    newDoc.updated_at_ = now;
    newDoc.updated_by_ = req.userCtx.name;
    newDoc.created_at_ = doc.created_at_;
    newDoc.created_by_ = doc.created_by_;
    message.timestamp = newDoc.updated_at_;
    message.user = newDoc.updated_by_;
    newDoc.prev_ = doc._rev;
  }

  return {
    doc: newDoc,
    message: message
  };
};

var get_changes = function (newDoc, doc) {
  'use strict';

  // This function is not implemented as efficiently as it could be but
  // I am more concerned with clarity at this point.
  var foldFields;
  var changes = {};

  if (doc) {
    foldFields = require('lib/fields').fromFieldsetsFold;

    var makeChangeObject = function (field, fieldset) {
      var obj = {
        fieldset: fieldset.id,
        fieldsetLabel: fieldset.label,
        fieldsetInstance: fieldset.instance ? fieldset.instance : null,
        field: field.id,
        fieldLabel: field.label
      };

      return obj;
    };
    var oldInstances = foldFields(doc.fieldsets, function (acc, fields, fieldset) {
      fields.forEach(function (field) {
        var obj = makeChangeObject(field, fieldset);
        obj.originalValue = JSON.stringify(field.value);
        acc[field.instance] = obj;
      });
      return acc;
    }, {});
    changes = foldFields(newDoc.fieldsets, function (acc, fields, fieldset) {
      fields.forEach(function (field) {
        var val = JSON.stringify(field.value);
        if (acc[field.instance] === undefined) {
          acc[field.instance] = makeChangeObject(field, fieldset);
          acc[field.instance]['newValue'] = val;
        } else if (acc[field.instance]['originalValue'] === val) {
          delete acc[field.instance];
        } else {
          acc[field.instance]['newValue'] = val;
        }
      });
      return acc;
    }, oldInstances);
  }

  if (Object.keys(changes).length === 0) {
    return null;
  } else {
    return changes;
  }
};

exports.get_changes = get_changes;
exports.stamp = stamp;
