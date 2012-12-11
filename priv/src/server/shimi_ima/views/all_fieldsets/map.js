function map(doc) {
  if (doc.category === 'fieldset') {
    emit([doc.doctype, doc._id, 'fieldset', doc.order], [doc.name, doc.label]);
  } else if (doc.category === 'field') {
    emit([doc.doctype, doc.fieldset, 'fieldset-field', -doc.order], [doc.name, doc.label]);
    if (doc.head) {
      if (doc.charseq) {
        emit(['_head-charseq', doc.doctype, doc.order], {
          '_id': doc.charseq
        });
      }
    }
  }
}