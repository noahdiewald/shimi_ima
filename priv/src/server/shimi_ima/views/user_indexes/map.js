function map(doc) {
  'use strict';
  return require('views/lib/user_indexes').user_indexes(doc, emit);
}
