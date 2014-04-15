function map(doc) {
  'use strict';

  emit(doc.name, 'project-' + doc._id);
}
