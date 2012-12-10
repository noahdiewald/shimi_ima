var simple_user_index = {
   "_id": "0923ebc77f5e57e0edbe40eed1f282e1",
   "_rev": "9-d20eea26efebb4379e5f20efdd4ae9ad",
   "category": "index",
   "doctype": "Entry",
   "show_deleted": false,
   "fields": [
       "d5331cbb4d62fe3d2899f142d90746b7"
   ],
   "fields_label": [
       "Headword:Lexical Category"
   ],
   "name": "all_vaio",
   "conditions": [
       {
           "is_or": false,
           "parens": false,
           "negate": false,
           "fieldset": "d5331cbb4d62fe3d2899f142d9036de5",
           "field": "d5331cbb4d62fe3d2899f142d90746b7",
           "operator": "equal",
           "argument": "vai+o"
       }
   ],
   "expression": "(equals('d5331cbb4d62fe3d2899f142d90746b7','vai+o'))",
   "updated_at_": "Mon, 10 Dec 2012 01:44:45 GMT",
   "updated_by_": "admin",
   "created_at_": "Tue, 20 Nov 2012 15:16:22 GMT",
   "created_by_": "monica",
   "prev_": "8-e3053a73fcf555a8a8b4e5d035678aaa"
};


Object.prototype.testEnv = true;

var emit = function() {return false};

function map(doc) {
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
      _id: "_design/" + doc._id,
      version: doc._rev,
      views: {
        index: {
          map: fun
        }
      }
    };
  };

  if (doc.category === "index") {
    var userIndexMap = function (doc) {
      if (doc.doctype === "doc.$doctype$" && !(doc.category) && doc.index && doc.deleted_.toString() === "doc.$show_deleted$") {

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
              ], ['', head()]);
            } else {
              if (doc.$replace_function_exists$) {
                emit([
                  ['', formatted]
                ], ['', head()]);
              } else {
                emit([
                  [fieldVal[0], formatted]
                ], ['', head()]);
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

        display_fields.forEach(function (item) {
          emitForField(item);
        });

        return "passed initial if clause";
      }

      return false;
    };

    var maybe_item = function (item) {
      if (item) {
        return item.toString();
      } else {
        return "function () {return false;}";
      }
    };

    var doctypeRe = new RegExp("doc\\.\\$doctype\\$", "g");
    var deletedRe = new RegExp("doc\\.\\$show_deleted\\$", "g");
    var expRe = new RegExp("doc\\.\\$expression\\$", "g");
    var fieldsRe = new RegExp("doc\\.\\$fields\\$", "g");
    var existsRe = new RegExp("doc\\.\\$replace_function_exists\\$", "g");
    var replaceRe = new RegExp("doc\\.\\$replace_function\\$", "g");

    var fun = userIndexMap.toString().replace(doctypeRe, doc.doctype);
    fun = fun.replace(deletedRe, doc.show_deleted.toString());
    fun = fun.replace(expRe, doc.expression);
    fun = fun.replace(fieldsRe, JSON.stringify(doc.fields));
    fun = fun.replace(existsRe, (doc.replace_function !== undefined).toString());
    fun = fun.replace(replaceRe, maybe_item(doc.replace_function));

    retval = makeObj(fun);
    emit(doc._id, JSON.stringify(retval));
  } else if (doc.category === "doctype") {
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

      if (doc.doctype === "$doctype$" && doc.fieldsets && !doc.deleted_) {
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
}

var assert = require("should");

var mapFunction = function() {
  var fbody = map(simple_user_index).views.index.map;
  return new Function("doc", "var f = "+ fbody + "; return f(doc);");
};

describe("User index view map funtion", function() {
  describe("when encountering a user index", function() {
    it("should return an object with the correct _id", function() {
      map(simple_user_index)._id.should.equal("_design/" + simple_user_index._id);
    });
    it("should return an object with the correct version", function() {
      map(simple_user_index).version.should.equal(simple_user_index._rev);
    });
    it("should return an object with an evaluable map function", function() {
      (function() {mapFunction();}).should.not.throwError();
    });
    describe("when evaluating the returned map function of the simple user index", function() {
      it("should return false when the object is not the right doctype", function() {
        mapFunction()({}).should.equal(false);
      });
     it("should return false when the object is deleted", function() {
        var doc = {
          doctype: "Entry",
          index: [],
          deleted_: true
        };
        mapFunction()({}).should.equal(false);
      });
     it("should return false when the object has no index", function() {
        var doc = {
          doctype: "Entry",
          deleted_: true
        };
        mapFunction()({}).should.equal(false);
      });
      it("should return \"passed initial if clause\" when the object is the right doctype, is not deleted and has an index", function() {
        var doc = {
          doctype: "Entry",
          index: [],
          deleted_: false
        };
        mapFunction()({}).should.equal(false);
      });
    });
  });
});