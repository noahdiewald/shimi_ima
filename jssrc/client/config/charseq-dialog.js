// Dialog for manipulating doctypes
shimi.charseqDialog = function (values) {
  var f = shimi.charseqElems.get(values);

  var dialog = $("#charseq-dialog").dialog({
    width: 650,
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function (context) {
          shimi.charseqTab.init();
          $(context).dialog("close");
        };

        if (values && values.rev) {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }

        shimi.form.send(url, obj, method, complete, this);
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};