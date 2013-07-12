function map(doc)
{
  'use strict';

  if (doc.category === 'index')
  {
    emit(doc._id, null);
  }
}
