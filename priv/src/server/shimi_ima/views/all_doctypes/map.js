function map(doc) {
  'use strict';

  if (doc.category === 'doctype') {
    emit(doc.name, doc.description);
  }
}
