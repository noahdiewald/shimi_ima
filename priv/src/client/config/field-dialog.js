// Dialog for manipulating fields
shimi.fieldDialog = function (url, values) {
  'use strict';

  var f = shimi.fieldElems.get(values);

  var dialog = $('#field-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Save': function () {
        var obj = f.clearDisabled().getFieldInputVals();
        var complete = function (context) {
          shimi.doctypeTab.initFields(url);
          $(context).dialog('close');
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};