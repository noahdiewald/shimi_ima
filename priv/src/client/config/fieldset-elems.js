// Returns an object with references to add/edit fieldset dialog
// field elements with helper functions. 
shimi.fieldsetElems = (function () {
  var mod = {};

  mod.attrs = ["name", "label", "order", "description", "doctype", "rev", "multiple", "collapse", "fieldset"];

  mod.get = function (values) {
    var fObj = {};

    fObj.attrs = mod.attrs;

    fObj.copyValues = function (source) {
      Object.keys(source).forEach(function (field) {
        fObj[field].val(source[field]);
        if (fObj[field].is('input[type=checkbox]')) {
          if (source[field] === "true") {
            fObj[field].attr('checked', true);
          }
        }
      });
      return fObj;
    };

    fObj.getFieldsetInputVals = function () {
      var valObj = {
        "category": "fieldset",
        "name": fObj.name.val(),
        "label": fObj.label.val(),
        "order": fObj.order.val() * 1,
        "description": fObj.description.val(),
        "doctype": fObj.doctype.val(),
        "multiple": fObj.multiple.is(':checked'),
        "collapse": fObj.collapse.is(':checked')
      };
      return valObj;
    };

    fObj.clear = function () {
      shimi.form.clear($('#fieldset-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };

    fObj.attrs.forEach(function (item) {
      fObj[item] = $('#fieldset-' + item + '-input');
    });

    fObj.copyValues(values);

    return fObj;
  };

  return mod;
})();