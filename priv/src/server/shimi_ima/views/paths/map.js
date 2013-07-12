function map(doc)
{
  'use strict';

  if (doc._attachments)
  {
    var path = [];

    if (doc.path)
    {
      path = doc.path;
    }

    emit(path, 1);
  }
}
