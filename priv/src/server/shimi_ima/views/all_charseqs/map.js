function map(doc) {
  if (doc.category === 'charseq') {
    emit(doc.name, doc.description);
  }
}