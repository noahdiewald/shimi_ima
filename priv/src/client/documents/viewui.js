// View pane UI elements
shimi.viewui = (function (args) {
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

  // Make an object where fieldsets with deletions are identified.
  var getDeletions = function (changes) {
    return Object.keys(changes).reduce(function (acc, x) {
      // If it was changed and there is no new value, it was deleted.
      if (changes[x].newValue === undefined) {
        if (acc[changes[x].fieldset] === undefined) {
          acc[changes[x].fieldset] = {};
        }
        acc[changes[x].fieldset][x] = changes[x];
      }

      return acc;
    }, {});
  };

  var processIncoming = function (docJson) {
    shimi.globals.changes = {};
    var withDeletions = {};

    if (docJson.changes) {
      withDeletions = getDeletions(docJson.changes);
    }

    docJson.fieldsets.forEach(function (fset) {
      var fsetId = fset.id;

      if (withDeletions[fsetId] !== undefined) {
        fset.removal = true;
        fset.altered = true;
      }

      var fieldFunc = function (field) {
        var changes = {};
        var change;

        if (docJson.changes) {
          changes = docJson.changes;
        }
        change = changes[field.instance];

        field.json_value = JSON.stringify(field.value);
        shimi.globals.changes[field.instance] = {
          fieldset: fsetId,
          field: field.id,
          originalValue: field.json_value
        };

        if (change !== undefined) {
          field.changed = true;
          fset.altered = true;

          if (change.originalValue === undefined) {
            fset.addition = true;
          } else {
            field.originalValue = JSON.parse(change.originalValue);
          }
        }

        if (field.subcategory === "textarea") {
          field.is_textarea = true;
        } else if (field.value && field.subcategory.match("multi")) {
          field.value = field.value.join(", ");
        }

        return true;
      };

      if (fset.multiple) {
        fset.multifields.forEach(function (mfs) {
          mfs.fields.forEach(function (field) {
            fieldFunc(field);
            return true;
          });
        });
      } else {
        fset.fields.forEach(function (field) {
          fieldFunc(field);
          return true;
        });
      }

      return true;
    });

    return true;
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
    var tmpl;

    if (rev) {
      url = url + "/" + rev;
      htmlTarget = dvt();
      tmpl = function (docJson) {
        return templates['document-view-tree'].render(docJson, {
          'document-view-field': templates['document-view-field']
        });
      };
    } else {
      tmpl = function (docJson) {
        return templates['document-view'].render(docJson, {
          'document-view-tree': templates['document-view-tree'],
          'document-view-field': templates['document-view-field']
        });
      };

    }

    $.getJSON(url, function (docJson) {
      var documentHtml;

      processIncoming(docJson);
      documentHtml = tmpl(docJson);
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
            shimi.indexui.get(skey, sid);
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

          shimi.indexui.get(skey, sid);
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
    shimi.fieldsets.fillFieldsets();

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