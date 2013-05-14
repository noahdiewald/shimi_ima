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
    shimi.dispatch.send("lost-focus");
  } else {
    t().tabs('option', 'active', 0);
    shimi.dispatch.send("lost-focus");
  }

  return false;
});

$(document).bind('keydown', 'Alt+c', function (e) {
  var active = $(document.activeElement).attr("id");
  shimi.dispatch.send("initiated-command", active);
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
    shimi.dispatch.send("lost-focus");
  } else {
    t().tabs('option', 'active', totaltabs - 1);
    shimi.dispatch.send("lost-focus");
  }

  return false;
});


$(document).on("keydown", '#edit-command-input', function (e) {
  if (e.which === 13) {
    var command = $('#edit-command-input').val();
    shimi.dispatch.send("submitted-command", command);
  }
  return true;
});

$(document).on('keydown', "#edit-document-form input", function (e) {
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

$(document).on("keyup", '#index-filter-form input', function (e) {
  var getIndexTimer;
  window.clearTimeout(getIndexTimer);
  getIndexTimer = setTimeout(function () {
    if (e.which !== 8 && e.which !== 46) {
      if (document.getElementById("all-document-container")) {
        shimi.indexui.get();
      } else {
        shimi.ipreviewui.get();
      }
    }
  }, 500);
});