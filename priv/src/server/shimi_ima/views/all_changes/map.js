function map(doc)
{
  'use strict';

  if (doc.category === 'change')
  {
    emit(doc._id, doc);
  }
}
