function list(head, req) {
  'use strict';

  start({
    'headers': {
      'Content-Type': 'text/csv; charset=utf-8'
    }
  });

  var row;
  var csvVal = function (item) {
    return item.replace(/"/g, '""').replace(/^(.*)$/, '"$1"');
  };
  var csvCell = function (item) {
    if (item === null) {
      return 'null';
    } else {
      return csvVal(item.toString());
    }
  };
  var csvKey = function (item) {
    return csvCell(item.map(function (x) {
      return x[1];
    }));
  };

  while (row = getRow()) {
    send([csvCell(row.id), csvKey(row.key), csvCell(row.value)].join(',') + '\n');
  }
}
