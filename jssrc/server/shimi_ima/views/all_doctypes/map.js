function (doc) {
  if (doc.category === 'doctype') {
    emit(doc._id, doc.description);
  }
}