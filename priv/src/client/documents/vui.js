// View pane UI elements
shimi.vui = (function (args) {
  var mod = {};
  var dv = function () {
    return $("#document-view");
  };
  var dvt = function () {
    return $("#document-view-tree");
  };
  var viewInfo = function () {
    return $("#document-view-info");
  };

  mod.formatTimestamps = function () {
    $('.timestamp').each(

    function (i, item) {
      var newDate = (new Date($(item).text())).toLocaleString();
      if (newDate !== "Invalid Date") {
        $(item).text(newDate);
      }
    });

    return mod;
  };

  mod.get = function (id, rev, callback) {
    var url = "documents/" + id;
    var htmlTarget = dv();

    if (rev) {
      url = url + "/" + rev;
      htmlTarget = dvt();
    }

    $.get(url, function (documentHtml) {
      htmlTarget.html(documentHtml);
      window.location.hash = id;
      mod.formatTimestamps();
      dv().fadeTo('slow', 1);
      if (callback) {
        callback();
      }

      if (!rev) {
        var restoreButton = $('#document-restore-button');
        var editButton = $('#document-edit-button');
        var deleteButton = $('#document-delete-button');

        if (shimi.store(restoreButton).d("deleted") === "true") {
          editButton.hide();
          deleteButton.hide();
          restoreButton.show();
        }
      }
    });

    return mod;
  };

  mod.restore = function (id, rev) {
    var url = "./documents/" + id + "?rev=" + rev;
    var restoreButton = $('#document-restore-button');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var body;
    var title;

    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function (req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was restored.";

          mod.get(id, null, function () {
            dv().fadeTo('slow', 1);
            shimi.iui.get(skey, sid);
          });
          shimi.flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          shimi.flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document was erased and cannot be restored.";
          title = req.statusText;

          shimi.flash(title, body).error();
        }
      }
    });

    return mod;
  };

  mod.del = function (id, rev) {
    var url = "./documents/" + id + "?rev=" + rev;
    var restoreButton = $('#document-restore-button');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var body;
    var title;

    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function (req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was deleted.";
          var response = JSON.parse(req.responseText);

          shimi.store(restoreButton).put("document-rev", response.rev);

          $('#document-delete-button').hide();
          $('#document-edit-button').hide();
          restoreButton.show();
          dv().fadeTo('slow', 0.5);

          shimi.iui.get(skey, sid);
          shimi.flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          shimi.flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document appears to have been deleted already.";
          title = req.statusText;

          shimi.flash(title, body).error();
        }
      }
    });

    return mod;
  };

  mod.confirmIt = function (callback) {
    if (window.confirm("Are you sure?")) {
      var s = shimi.store(viewInfo());
      var id = s.d("document");
      var rev = s.d("rev");

      callback(id, rev);
    }

    return mod;
  };

  mod.edit = function () {
    shimi.editui.resetFields();
    if ($('#document-view-tree').hasClass('oldrev')) {
      $('#save-document-button').addClass('oldrev');
    } else {
      $('#save-document-button').removeClass('oldrev');
    }
    shimi.efs.fillFieldsets();

    return mod;
  };

  mod.confirmDelete = function () {
    var s = shimi.store(viewInfo());
    var id = s.d("document");
    var rev = s.d("rev");
    return mod.confirmIt(function () {
      mod.del(id, rev);
    });
  };

  mod.confirmRestore = function () {
    var s = shimi.store(viewInfo());
    var id = s.d("document");
    var rev = s.d("rev");
    return mod.confirmIt(function () {
      mod.restore(id, rev);
    });
  };

  mod.collapseToggle = function (target) {
    $(target).parent('li').toggleClass('collapsed');

    return mod;
  };

  mod.fetchRevision = function (target) {
    var s = shimi.store($(target));
    var id = s.d("document");
    var oldrev = s.d("oldrev");

    $('.revision-link').removeClass('selected-revision');
    $(target).addClass('selected-revision');

    mod.get(id, oldrev);

    return mod;
  };

  return mod;
})();