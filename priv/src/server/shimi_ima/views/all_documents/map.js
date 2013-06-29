function map(doc) {
  'use strict';

  var isReversal = function (elem) {
    return elem.reversal === true;
  };

  var isHead = function (elem) {
    return elem.head === true;
  };

  var gatherElems = function (acc, obj, filterFun, reversal) {
    acc = acc.concat(obj.fields.filter(function (elem, index) {
      return (filterFun(elem));
    }).map(function (elem, index) {
      if (reversal) {
        return [elem.value];
      } else {
        return [elem.sortkey, elem.value];
      }
    }));

    return acc;
  };

  var gather = function (acc, filterFun, reversal) {
    doc.fieldsets.forEach(function (fieldset, index) {
      if (!fieldset.multiple) {
        acc = gatherElems(acc, fieldset, filterFun, reversal);
      } else {
        fieldset.multifields.forEach(function (multifield, index) {
          acc = gatherElems(acc, multifield, filterFun, reversal);
        });
      }
    });

    return acc;
  };

  if (doc.doctype && !doc.category && doc.fieldsets && !doc.deleted_) {
    var heads = gather([], isHead);
    var reversals = gather([], isReversal, true);

    if (heads.length === 0) {
      heads = [
        ['', doc._id]
      ];
    }

    emit([doc.doctype, heads], reversals);
  }
}