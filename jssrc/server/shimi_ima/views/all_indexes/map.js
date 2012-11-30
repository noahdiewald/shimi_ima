function (doc) {
  if (doc.category === 'index') {
    emit(doc._id, null);
  }
}