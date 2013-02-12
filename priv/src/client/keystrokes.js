var selectInput = function () {
  var inputable = 'input, select';
  var t = function () {
    return $('#edit-tabs');
  };

  var cur = t().find('.ui-tabs-active a').attr('href');
  $(cur).find(inputable + ", textarea").first().focus();
};

$(document).on("keydown", '#document-worksheets-form', function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("worksheet-form-submit");
    return false;
  }
  return true;
});

$(document).on("keydown", '#document-sets-form', function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("sets-form-submit");
    return false;
  }
  return true;
});

$('#new-set-form').on("keydown", function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("new-set-form-submit");
    return false;
  }
  return true;
});

$(document).bind('keydown', 'Alt+n', function (e) {
  var t = function () {
    return $('#edit-tabs');
  };
  var totaltabs = t().find('li').length;
  var selected = t().tabs('option', 'active');

  if (selected < totaltabs - 1) {
    t().tabs('option', 'active', selected + 1);
    selectInput();
  } else {
    t().tabs('option', 'active', 0);
    selectInput();
  }

  return false;
});

$(document).bind('keydown', 'Alt+c', function (e) {
  var active = $(document.activeElement);
  shimi.editui.showCommandDialog(active);
  return true;
});

$(document).bind('keydown', 'Alt+p', function (e) {
  var t = function () {
    return $('#edit-tabs');
  };
  var totaltabs = t().find('li').length;
  var selected = t().tabs('option', 'active');

  if (selected !== 0) {
    t().tabs('option', 'active', selected - 1);
    selectInput();
  } else {
    t().tabs('option', 'active', totaltabs - 1);
    selectInput();
  }

  return false;
});


$('#edit-command-input').on("keydown", function (e) {
  if (e.which === 13) {
    var command = $('#edit-command-input').val();
    var restoreFocus = true;
    $('#command-dialog').dialog("close");

    switch (command) {
    case "w":
    case "clear":
      shimi.editui.clear();
      break;
    case "c":
    case "create":
      shimi.editui.create();
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
      $('#' + $('#command-dialog').attr('data-last-active')).focus();
    } else {
      selectInput();
    }
  }

  return true;
});

$("#edit-document-form input").on('keydown', function (e) {
  if (e.which === 13) {
    if ($("#save-document-button").css("display") === "none") {
      shimi.editui.create();
    } else {
      shimi.editui.save();
    }
  }
  return true;
});

$(document).on('keydown', "#edit-document-form textarea", 'Alt+x', function (e) {
  shimi.editui.toggleTextarea($(e.target));
  return false;
});

$(document).on("keypress", '#view-jump-id', function (e) {
  if (e.which === 13) {
    var docid = $('#view-jump-id').val();
    shimi.viewui.get(docid);
    return false;
  }
  return true;
});

$(document).on("keydown", '#document-search-term', function (e) {
  if (e.which === 13) {
    shimi.searchui.getSearch();
    return false;
  }
  return true;
});