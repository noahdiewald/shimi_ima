// So far this is just a test and will not always produce a valid CSV.
exports.list = function (head, req, start, send, getRow) {
  'use strict';

  start({
    'headers': {
      'Content-Type': 'text/csv; charset=utf-8'
    }
  });

  send(['ID', 'KEY', 'VALUE'].join(',') + '\n');

  for (var row = getRow(); row !== undefined; row = getRow()) {
    send([JSON.stringify(row.id), JSON.stringify(row.key), JSON.stringify(row.value)].join(',') + '\n');
  }
};
