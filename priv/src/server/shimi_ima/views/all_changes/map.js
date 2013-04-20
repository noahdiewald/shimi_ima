function map(doc) {
  if (doc.category === 'change') {
    emit(doc._id, doc);
  }
}
