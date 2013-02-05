// Edit pane UI elements
shimi.editui = (function () {
  var mod = {};

  // Imports
  var store = shimi.store;
  var flash = shimi.flash;

  // UI Elements
  var saveButton = function () {
    return $('#save-document-button');
  };
  var createButton = function () {
    return $('#create-document-button');
  };
  var editButton = function () {
    return $('#document-edit-button');
  };

  var keyboard = function () {
    var inputable = 'input, select';
    var t = $('#edit-tabs');

    var selectInput = function () {
      var cur = t.find('.ui-tabs-selected a').attr('href');
      $(cur).find(inputable + ", textarea").first().focus();
    };

    $(document).bind('keydown', 'Alt+p', function (e) {
      var totaltabs = t.tabs('length');
      var selected = t.tabs('option', 'selected');

      if (selected !== 0) {
        t.tabs('select', selected - 1);
        selectInput();
      } else {
        t.tabs('select', totaltabs - 1);
        selectInput();
      }

      return false;
    });

    $(document).bind('keydown', 'Alt+n', function (e) {
      var totaltabs = t.tabs('length');
      var selected = t.tabs('option', 'selected');

      if (selected < totaltabs - 1) {
        t.tabs('select', selected + 1);
        selectInput();
      } else {
        t.tabs('select', 0);
        selectInput();
      }

      return false;
    });

    $(document).live('keydown', 'Alt+c', function (e) {
      var active = $(document.activeElement);
      mod.showCommandDialog(active);
      return true;
    });

    $('#edit-command-form').live("submit", function (e) {
      return false;
    });

    $('#edit-command-input').live("keydown", function (e) {
      if (e.which === 13) {
        var command = $('#edit-command-input').val();
        var restoreFocus = true;
        $('#command-dialog').dialog("close");

        switch (command) {
        case "w":
        case "clear":
          mod.clear();
          break;
        case "c":
        case "create":
          mod.create();
          break;
        case "s":
        case "save":
          mod.save();
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

    $("#edit-document-form input").live('keydown', function (e) {
      if (e.which === 13) {
        if ($("#save-document-button").css("display") === "none") {
          mod.create();
        } else {
          mod.save();
        }
      }
      return true;
    });

    $("#edit-document-form textarea").on('keydown', 'Alt+x', function (e) {
      mod.toggleTextarea($(e.target));
      return false;
    });

    return mod;
  };

  var validationError = function (req) {
    var body = JSON.parse(req.responseText);
    var title = req.statusText;

    var invalid = $('[data-field-instance=' + body.instance + ']');
    var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');

    invalidTab.addClass('ui-state-error');
    invalid.addClass('ui-state-error');

    flash(title, body.fieldname + " " + body.message).error();

    return mod;
  };

  var instances = function () {
    var text = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
    var makeInstance = function () {
      return text.map(function () {
        return text[Math.floor(Math.random() * text.length)];
      }).join('');
    };

    $("[data-field-instance]").each(

    function (index, item) {
      var newInstance = makeInstance();
      $(item).first().attr('data-field-instance', newInstance);
      $(item).first().attr('data-group-id', newInstance);
      $(item).first().attr('id', newInstance);
      $(item).first().next('.expander').attr('data-group-id', newInstance);
      $(item).first().next().next('.expander').attr('data-group-id', newInstance);
    });

    return mod;
  };

  mod.init = function () {
    var url = "documents/edit";

    $.get(url, function (documentEditHtml) {

      $('#document-edit').html(documentEditHtml);
      $('#edit-tabs').tabs();
      keyboard();
      shimi.fieldsets.initFieldsets();
    });

    return mod;
  };

  mod.afterFreshRefresh = function () {
    afterRefresh();

    return mod;
  };

  mod.afterEditRefresh = function () {
    var sharedAttrs = ['data-document-id', 'data-document-rev'];

    sharedAttrs.forEach(function (elem) {
      saveButton().attr(elem, editButton().attr(elem));
    });

    saveButton().show();
    afterRefresh();

    return mod;
  };

  var afterRefresh = function () {
    shimi.form.initDateFields();
    instances();

    return mod;
  };

  mod.resetFields = function () {
    $('.field').each(function (index) {
      var field = $(this);
      var thedefault = field.attr('data-field-default');

      if (thedefault && thedefault !== '') {
        if (field.is('select.multiselect')) {
          field.val(thedefault.split(","));
        } else if (field.is('input.boolean')) {
          field.attr('checked', thedefault === true);
        } else {
          field.val(thedefault);
        }
      } else {
        field.val('');
        field.removeAttr('checked');
      }
    });

    return mod;
  };

  mod.save = function () {
    if (saveButton().hasClass('oldrev')) {
      if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
        return false;
      }
    }

    var body;
    var title;
    var s = store(saveButton());
    var root = $('#edit-document-form');
    var document = s.d("document");
    var rev = s.d("rev");
    var url = "./documents/" + document + "?rev=" + rev;
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };

    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton().hide();
    $.extend(obj, shimi.fieldsets.fieldsetsToObject(root));

    $.ajax({
      type: "PUT",
      url: url,
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function (req, status) {
        if (req.status === 204 || req.status === 200) {
          title = "Success";
          body = "Your document was saved.";
          shimi.viewui.get(document);
          shimi.indexiu.get(skey, sid);
          flash(title, body).highlight();
          saveButton().removeClass('oldrev').show();
        } else if (req.status === 403) {
          validationError(req);
          saveButton().show();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;

          flash(title, body.message).error();
          saveButton().hide();
        }
      }
    });
  };

  mod.create = function () {
    var s = store(createButton());
    var root = $('#edit-document-form');
    var skey = $('#first-index-element').attr('data-first-key');
    var sid = $('#first-index-element').attr('data-first-id');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };

    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    createButton().hide();
    $.extend(obj, shimi.fieldsets.fieldsetsToObject(root));

    var postUrl = $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function (req, status) {
        if (req.status === 201) {
          var title = "Success";
          var body = "Your document was created.";
          var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);

          saveButton().hide().attr('disabled', 'true');
          $('.fields').remove();
          shimi.fieldsets.initFieldsets();
          shimi.viewui.get(documentId);
          shimi.indexiu.get(skey, sid);
          flash(title, body).highlight();
          createButton().show();
        } else if (req.status === 403) {
          validationError(req);
          createButton().show();
        }
      }
    });
  };

  mod.clear = function () {
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton().hide().attr('disabled', 'disabled');
    $('.fields').remove();
    shimi.fieldsets.initFieldsets();
  };

  mod.showHelpDialog = function (target) {
    if (target.is('.label-text')) {
      target = target.parent('label').find('.ui-icon-help');
    }

    $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));

    return mod;
  };

  mod.showCommandDialog = function (context) {
    $('#edit-command-input').val("");
    $('#edit-dialog').attr('data-last-active', context.id);
    var commandDialog = $('#command-dialog').dialog({
      modal: true,
      close: function () {
        $('#edit-command-input').val("");
        context.focus();
      }
    });
    commandDialog.dialog('open');
    return mod;
  };

  mod.toggleTextarea = function (target) {
    var textarea = $('#' + target.attr('data-group-id'));

    if (target.attr("id") === textarea.attr("data-group-id")) {
      textarea.toggleClass('expanded');
      textarea.next().next("span").toggleClass('expanded');
    } else {
      textarea.toggleClass('expanded');
      target.toggleClass('expanded');
    }

    return mod;
  };

  return mod;
})();