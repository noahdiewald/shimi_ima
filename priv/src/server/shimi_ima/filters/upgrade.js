function filter(doc, req) {
  'use strict';

  if (doc._id === '_design/shimi_ima') {
    return true;
  } else {
    return false;
  }
}