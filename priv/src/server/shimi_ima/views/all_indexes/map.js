function map(doc) {
  if (doc.category === 'index') {
    emit(doc._id, null);
  }
}