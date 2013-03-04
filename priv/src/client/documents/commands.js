shimi.commands = (function () {
  var mod = {};
  var commandInput = function () {
    return $('#edit-command-input');
  };
  var commandDialog = function () {
    return $('#command-dialog');
  };
  var setContext = function (elem, context) {
    return elem.attr("data-last-active", context);
  };
  var getContext = function (elem) {
    return elem.attr("data-last-active");
  };

  mod.execute = function (command) {
    var restoreFocus = true;

    switch (command) {
    case "w":
    case "clear":
      shimi.editui.clear();
      break;
    case "c":
    case "create":
      shimi.editui.create();
      restoreFocus = false;
      break;
    case "s":
    case "save":
      shimi.editui.save();
      break;
    case "d":
    case "delete":
      $("#document-view").show();
      if ($("#document-delete-button").css("display") !== "none") {
        $("#document-delete-button").click();
      }
      break;
    case "e":
    case "edit":
      $("#document-view").show();
      if ($("#document-edit-button").css("display") !== "none") {
        $("#document-edit-button").click();
        restoreFocus = false;
      }
      break;
    case "r":
    case "restore":
      $("#document-view").show();
      if ($("#document-restore-button").css("display") !== "none") {
        $("#document-restore-button").click();
      }
      break;
    }

    if (restoreFocus) {
      var cdialog = commandDialog();
      var context = getContext(cdialog);
      $('#' + context).focus();
    } else {
      shimi.dispatch.send("lost-focus");
    }

    shimi.dispatch.send("executed-command");
    return mod;
  };

  mod.dialogOpen = function (context) {
    var cinput = commandInput();
    var cdialog = commandDialog();
    cinput.val("");
    setContext(cdialog, context).show();
    cinput.focus();
    return mod;
  };

  mod.dialogClose = function () {
    var cinput = commandInput();
    var cdialog = commandDialog();
    setContext(cdialog, "").hide();
    cinput.val("");
    return mod;
  };

  return mod;
})();