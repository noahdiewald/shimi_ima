shimi.initReplaceDialog = function () {
  var replaceFunction = $('#index-replace_function-input');
  var indexData = $('#index-editing-data');
  var remove = $('#index-remove_function-input');

  if (indexData.attr('data-index-replace_function')) {
    replaceFunction.val(indexData.attr('data-index-replace_function'));
  } else {
    shimi.form.clear(replaceFunction).removeClass('ui-state-error');
  }

  var dialog = $("#index-replace-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function () {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!remove.is(':checked')) {
          if (replaceFunction.val().isBlank()) {
            replaceFunction.addClass('ui-state-error');
          } else {
            replaceFunction.removeClass('ui-state-error');
          }

          if (checkResult) {
            indexData.attr('data-index-replace_function', replaceFunction.val());
            $('#replace-function-message').text("This index has a replacement function.");
          }
        } else {
          indexData.removeAttr('data-index-replace_function');
          $('#replace-function-message').empty();
        }

        $(this).dialog("close");
      },
      "Cancel": function () {
        $(this).dialog("close");
      }
    },
    close: function () {
      shimi.form.clear(replaceFunction).removeClass('ui-state-error');
    }
  });

  return dialog;
};