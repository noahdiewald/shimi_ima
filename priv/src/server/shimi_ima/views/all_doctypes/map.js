function map(doc) {
  'use strict';

  if (doc.category === 'doctype') {
    if (doc.name) {
      emit(doc.name, doc.description);
    } else {
      emit(doc._id, doc.description);
    }
  }
}
