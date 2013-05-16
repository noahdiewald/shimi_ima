// Returns an object with references to add/edit doctype dialog
// field elements with helper functions. 
shimi.doctypeElems = (function () {
  'use strict';

  var mod = {};

  mod.attrs = ['description', 'doctype', 'rev'];

  mod.get = function (values) {
    var fObj = {};

    fObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        fObj[field].val(source[field]);
      });
      return fObj;
    };

    fObj.getDoctypeInputVals = function () {
      var valObj = {
        'category': 'doctype',
        'description': fObj.description.val(),
        '_id': fObj.doctype.val()
      };
      return valObj;
    };

    fObj.clear = function () {
      shimi.form.clear($('#doctype-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    mod.attrs.forEach(function (item) {
      fObj[item] = $('#doctype-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();