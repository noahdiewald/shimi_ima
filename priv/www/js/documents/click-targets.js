function addFieldset(target) {
  var fieldsetContainer = $("#container-" + target.attr('data-fieldset-id'));
  var url = buildUrl(fieldsetContainer.attr('data-project-id'),
                     fieldsetContainer.attr('data-doctype-id'),
                     fieldsetContainer.attr('data-fieldset-id'));
     
  initFieldset(fieldsetContainer, url);
}

function removeFieldset(target) {
  target.parent().remove();
}

function save(target) {
  var saveButton = target;
  var root = $('#edit-document-form');
  var documentId = saveButton.attr('data-document-id');
  var documentRev = saveButton.attr('data-document-rev');
  var url = "./documents/" + documentId + "?rev=" + documentRev;
  var obj = {
    doctype: saveButton.attr('data-doctype-id'),
    description: saveButton.attr('data-doctype-description')
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
        getDocument(documentId, true);
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

function create(target) {
  var createButton = target;
  var root = $('#edit-document-form');
  var obj = {
    doctype: createButton.attr('data-doctype-id'),
    description: createButton.attr('data-doctype-description')
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

function clear(target) {
  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  $('#save-document-button').hide();
  $('#save-document-button').attr('disabled','disabled');
  $('.fields').remove();
  initFieldsets();
}

function edit(target) {
  resetFields();
  fillFieldsets();
}

function delete(target) {
  if (confirm("Are you sure?")) {
    var docid = target.attr('data-document-id');
    var docrev = target.attr('data-document-rev');
    
    deleteDocument(docid, docrev);
  }
}

function collapseToggle(target) {
  target.parent('li').toggleClass('collapsed');
}

function panelToggle(target) {
  var panel = $('#' + target.attr('data-panel'));
  panel.toggle();
}