function map(doc) {
  'use strict';

  if (doc.category === 'doctype') {
    if (doc.fieldsets) {
      doc.fieldsets.forEach(function (fieldset) {
        emit([doc._id, fieldset._id, 'fieldset', fieldset.order], [fieldset.name, fieldset.label]);

        if (fieldset.fields) {
          fieldset.fields.forEach(function (field) {
            emit([doc._id, fieldset._id, 'fieldset-field', -field.order], [field.name, field.label, field._id]);

            if (field.head && field.charseq) {
              emit(['_head-charseq', doc._id, field.order], {
                '_id': field.charseq
              });
            }
          });
        }
      });
    }
  }
}
