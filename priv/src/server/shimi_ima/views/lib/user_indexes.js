var user_indexes = function (doc, emit, testEnv) {
  'use strict';

  /*
    The end user is able to define view indexes both explicitly, using
    the index tool, and implicitly, using the configuration interface
    to create new doctypes. These views need to be generated using
    information specific to documents that define certain parameters. In
    this case, the documents are of category "index" or "doctype". The
    map function below emits the properly generated design documents
    for these categories.
  */

  var retval = false;

  var makeObj = function (fun) {
    return {
      _id: '_design/' + doc._id,
      version: doc._rev,
      views: {
        index: {
          map: fun
        }
      }
    };
  };

  if (doc.category === 'index') {
    var userIndexMap = function (doc) {
      if (doc.doctype === 'doc.$doctype$' && !(doc.category) && doc.index && doc.deleted_.toString() === 'doc.$show_deleted$') {

        var display_fields = doc.$fields$;
        var runReplace = false;

        var lookup = function (fieldid) {
          var retval;

          switch (fieldid) {
          case 'created_by_':
          case 'updated_by_':
          case 'created_at_':
          case 'updated_at_':
            retval = ['', doc[fieldid]];
            break;
          default:
            retval = doc.index[fieldid];
          }

          return retval;
        };

        var isArray = function (anArray) {
          return Object.prototype.toString.apply(anArray) === '[object Array]';
        };

        var notBlank = function (val) {
          return (val !== undefined && val !== null && val.toString() !== '');
        };

        var runTest = function (fieldid, value, pred, existential) {
          var fieldVal = lookup(fieldid);
          var retval = false;

          if (notBlank(fieldVal)) {
            if (!isArray(fieldVal[0])) {
              retval = pred(fieldVal[1], value);
            } else {
              if (typeof existential === 'number') {
                retval = pred(fieldVal[existential][1], value);
              } else {
                retval = fieldVal.some(function (fVal) {
                  return pred(fVal[1], value);
                });
              }
            }
          }

          return retval;
        };

        var runShapeTest = function (fieldid, value, pred) {
          var fieldVal = lookup(fieldid);
          return pred(fieldVal, value);
        };

        var equals = function (fieldid, value, existential) {
          var pred = function (value1, value2) {
            return value1 === value2;
          };
          return runTest(fieldid, value, pred, existential);
        };

        var greaterThan = function (fieldid, value, existential) {
          var pred = function (value1, value2) {
            return value1 > value2;
          };
          return runTest(fieldid, value, pred, existential);
        };

        var lessThan = function (fieldid, value, existential) {
          var pred = function (value1, value2) {
            return value1 < value2;
          };
          return runTest(fieldid, value, pred, existential);
        };

        var matches = function (fieldid, value, existential) {
          var pred = function (value1, value2) {
            return !!value1.match(value2);
          };
          return runTest(fieldid, value, pred, existential);
        };

        var hasExactly = function (fieldid, value) {
          var pred = function (value1, value2) {
            var retval = false;
            if (value1) {
              retval = value1.length === value2;
            }
            return retval;
          };
          return runShapeTest(fieldid, value, pred);
        };

        var hasGreater = function (fieldid, value) {
          var pred = function (value1, value2) {
            var retval = false;
            if (value1) {
              retval = value1.length > value2;
            }
            return retval;
          };
          return runShapeTest(fieldid, value, pred);
        };

        var hasLess = function (fieldid, value) {
          var pred = function (value1, value2) {
            var retval = false;
            if (value1) {
              retval = value1.length < value2;
            }
            return retval;
          };
          return runShapeTest(fieldid, value, pred);
        };

        var hasMember = function (fieldid, value, existential) {
          var pred = function (value1, value2) {
            return value1.indexOf(value2) >= 0;
          };
          return runTest(fieldid, value, pred, existential);
        };

        var isDefined = function (fieldid) {
          var pred = function (value1, value2) {
            return value1 !== value2;
          };
          return runShapeTest(fieldid, undefined, pred);
        };

        var isTrue = function (fieldid, existential) {
          return equals(fieldid, true, existential);
        };

        var isBlank = function (fieldid, existential) {
          var pred = function (value1, value2) {
            return !notBlank(value1);
          };
          return runTest(fieldid, null, pred, existential);
        };

        var existentialTest = function (fieldid, testFun) {
          var retval = false;
          var fields = lookup(fieldid);

          retval = fields.some(function (v, i) {
            return testFun(i);
          });

          return retval;
        };

        var head = function () {
          var hd = doc.head;
          return hd.map(function (h) {
            var v = lookup(h);
            if (isArray(v[0])) {
              if (notBlank(v[0][1])) {
                return v[0][1] + '...';
              } else {
                return '...';
              }
            } else {
              return v[1];
            }
          }).join(', ');
        };

        var condition = function () {
          return (doc.$expression$);
        };

        if (doc.$replace_function_exists$) {
          runReplace = doc.$replace_function$;
        }

        var format = function (val) {
          var retval = '_BLANK_';

          if (isArray(val)) {
            retval = val.map(function (i) {
              return i.toString();
            }).join(',');
          } else {
            if (notBlank(val)) {
              retval = val.toString();
            }
          }

          if (doc.$replace_function_exists$) {
            return runReplace(retval);
          }

          return retval;
        };

        var emitIf = function (fieldVal) {
          if (condition()) {
            var formatted = format(fieldVal[1]);

            if (display_fields.lenghth > 1) {
              emit([
                ['', formatted]
              ], head());
            } else {
              if (doc.$replace_function_exists$) {
                emit([
                  ['', formatted]
                ], head());
              } else {
                emit([
                  [fieldVal[0], formatted]
                ], head());
              }
            }
          }
        };

        var emitForField = function (fieldid) {
          var fieldVal = lookup(fieldid);

          if (!notBlank(fieldVal)) {
            emitIf(['', '_UNDEFINED_']);
          } else if (isArray(fieldVal[0])) {
            fieldVal.forEach(function (item) {
              emitIf(item);
            });
          } else {
            emitIf(fieldVal);
          }
        };

        emitForField(display_fields);

        return 'passed initial if clause';
      }

      return false;
    };

    var maybe_item = function (item) {
      if (item) {
        return item.toString();
      } else {
        return 'function () {return false;}';
      }
    };

    var doctypeRe = new RegExp('doc\\.\\$doctype\\$', 'g');
    var deletedRe = new RegExp('doc\\.\\$show_deleted\\$', 'g');
    var expRe = new RegExp('doc\\.\\$expression\\$', 'g');
    var fieldsRe = new RegExp('doc\\.\\$fields\\$', 'g');
    var existsRe = new RegExp('doc\\.\\$replace_function_exists\\$', 'g');
    var replaceRe = new RegExp('doc\\.\\$replace_function\\$', 'g');

    var fun = userIndexMap.toString().replace(doctypeRe, doc.doctype);
    fun = fun.replace(deletedRe, doc.show_deleted.toString());
    fun = fun.replace(expRe, doc.expression);
    fun = fun.replace(fieldsRe, JSON.stringify(doc.fields));
    fun = fun.replace(existsRe, (doc.replace_function !== undefined).toString());
    fun = fun.replace(replaceRe, maybe_item(doc.replace_function));

    retval = makeObj(fun);
    emit(doc._id, JSON.stringify(retval));
  } else if (doc.category === 'doctype') {
    var doctypeMap = function (doc) {
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

      if (doc.doctype === '$doctype$' && doc.fieldsets && !doc.deleted_) {
        var heads = gather([], isHead);
        var reversals = gather([], isReversal, true);

        if (heads.length === 0) {
          heads = [
            ['', doc._id]
          ];
        }
        emit(heads, reversals);
        return {
          id: doc._id,
          key: heads,
          value: reversals
        };
      }

      return false;
    };

    retval = makeObj(doctypeMap.toString().replace(/\$doctype\$/, doc._id));
    emit(doc._id, JSON.stringify(retval));
  }

  return retval;
};

exports.user_indexes = user_indexes;
