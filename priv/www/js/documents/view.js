// Functions for building view via ajax calls. Functions preceded by "get"
// are generally pulling in data. Functions preceded by "init" are generally
// generating parts of a form.

// Get a document revision and display it in the middle column
var getRevision = function(id, rev) {
  var url = "documents/" + id + "/" + rev;
  var dvt = $('#document-view-tree');

  dvt.hide();

  $.get(url, function(documentHtml) {
          dvt.html(documentHtml);
          dvt.show();
        });
};

// Get a document and display it in the middle column
function getDocument(id, runAfterEditRefresh) {
  var url = "documents/" + id;
  
  $('#document-view').fadeTo('slow', 1);
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);

    if (runAfterEditRefresh) afterEditRefresh();
    
    var restoreButton = $('#document-restore-button');
    var editButton = $('#document-edit-button');
    var deleteButton = $('#document-delete-button');
   
    editButton.button({icons: {primary: 'ui-icon-pencil'}});
    deleteButton.button({icons: {primary: 'ui-icon-trash'}});
    restoreButton.button({icons: {primary: 'ui-icon-refresh'}});
    
    if (dInfo("deleted", restoreButton) === "true") {
      editButton.hide();
      deleteButton.hide();
    } else {
      restoreButton.hide();
    }
  });
}

function restoreDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  var restoreButton = $('#document-restore-button');
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 200) {
        var title = "Success";
        var body = "Your document was restored.";
        var response = JSON.parse(req.responseText);
        
        putValue("document-rev", response.rev, restoreButton);
        
        $('#document-restore-button').hide();
        $('#document-delete-button').show();
        $('#document-edit-button').show();
        $('#document-view h2').text("Restored Document");
        $('#document-view').fadeTo('slow', 1);
        
        getIndex();
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Document was erased and cannot be restored.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });
}

function deleteDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  var restoreButton = $('#document-restore-button');
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 200) {
        var title = "Success";
        var body = "Your document was deleted.";
        var response = JSON.parse(req.responseText);
        
        putValue("document-rev", response.rev, restoreButton);
        
        $('#document-delete-button').hide();
        $('#document-edit-button').hide();
        restoreButton.show();
        $('#document-view h2').text("Deleted Document");
        $('#document-view').fadeTo('slow', 0.5);
        
        getIndex();
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Document appears to have been deleted already.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });
}
