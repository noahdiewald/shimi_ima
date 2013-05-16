function map(doc) {
  'use strict';

  if (doc.category === 'charseq') {
    emit(doc.name, doc.description);
  }
}