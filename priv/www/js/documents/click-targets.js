// These are the target functions called in the click dispatch area.
// Some are defined elsewhere for organizational reasons.

function removeFieldset(target) {
  target.parent().remove();
}

function saveDoc(target) {
  var saveButton = target;
  var root = $('#edit-document-form');
  var document = dInfo("document", saveButton);
  var rev = dInfo("rev", saveButton);
  var url = "./documents/" + document + "?rev=" + rev;
  var obj = {
    doctype: dInfo("doctype", saveButton),
    description: dInfo("description", saveButton)
  };
  
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton.button('disable');
  $.extend(obj, fieldsetsToObject(root));
  
  $.ajax({
    type: "PUT",
    url: url,
    dataType: "json",
    contentType: "application/json",
    processData: false,
    data: JSON.stringify(obj),
    complete: function(req, status) {
      if (req.status == 204) {
        var title = "Success";
        var body = "Your document was saved.";
        getDocument(document, true);
        getIndex();
        flashHighlight(title, body);
        saveButton.button('enable');
      } else if (req.status == 403) {
        setInvalidError(req);
        saveButton.button('enable');
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
        
        flashError(title, body.message);
        saveButton.button('enable');
      }
    }
  });
}

function createDoc(target) {
  var createButton = target;
  var root = $('#edit-document-form');
  var obj = {
    doctype: dInfo("doctype", createButton),
    description: dInfo("description", createButton)
  };
  
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  createButton.button('disable');
  $.extend(obj, fieldsetsToObject(root));
  
  postUrl = $.ajax({
    type: "POST",
    dataType: "json",
    contentType: "application/json",
    processData: false,
    data: JSON.stringify(obj),
    complete: function(req, status) {
      if (req.status == 201) {
        var title = "Success";
        var body = "Your document was created.";
        var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);
        
        $('#save-document-button').hide();
        $('#save-document-button').attr('disabled','true');
        $('.fields').remove();
        initFieldsets();
        getDocument(documentId);
        getIndex();
        flashHighlight(title, body);
        createButton.button('enable');
      } else if (req.status == 403) {
        setInvalidError(req);
        createButton.button('enable');
      }
    }
  });
}

function clearDoc(target) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').hide();
  $('#save-document-button').attr('disabled','disabled');
  $('.fields').remove();
  initFieldsets();
}

function editDoc(target) {
  resetFields();
  fillFieldsets();
}

function deleteDoc(target) {
  if (confirm("Are you sure?")) {
    var document = dInfo("document", target);
    var rev = dInfo("rev", target);
    
    deleteDocument(document, rev);
  }
}

function collapseToggle(target) {
  target.parent('li').toggleClass('collapsed');
}

function panelToggle(target) {
  var panel = $('#' + target.attr('data-panel'));
  panel.toggle();
}

function showHelpDialog(target) {
  if (target.is('.label-text')) {
    target = target.parent('label').find('.ui-icon-help');
  }
  
  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));
}

function toggleTextarea(target) {
  var textarea = $('#' + target.attr('data-group-id'));
  
  textarea.toggleClass('expanded');
  target.toggleClass('expanded');
}