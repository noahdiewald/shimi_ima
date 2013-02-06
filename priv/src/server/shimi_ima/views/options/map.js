function map(doc) {
  if (doc.category === 'index') {
    emit([doc.doctype, doc.name]);
  }
}