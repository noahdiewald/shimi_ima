function map(doc)
{
  'use strict';

  if (doc.category === 'doctype')
  {
    emit(doc._id, doc.description);
  }
}
