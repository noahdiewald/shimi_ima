// Functions for building view via ajax calls. Functions preceded by "get"
// are generally pulling in data. Functions preceded by "init" are generally
// generating parts of a form.

// Get a document and display it in the middle column
function getDocument(id, runAfterEditRefresh) {
  var url = "documents/" + id;
  
  $('#document-view').fadeTo('slow', 1);
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);
    
    if (runAfterEditRefresh) afterEditRefresh();
    
    $('#document-edit-button').button({icons: {primary: 'ui-icon-pencil'}});
    
    $('#document-delete-button').button({icons: {primary: 'ui-icon-trash'}});
  });
}

function deleteDocument(docid, docrev) {
  var url = "./documents/" + docid + "?rev=" + docrev;
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 204) {
        var title = "Success";
        var body = "Your document was deleted. You may undo this by clicking Edit and then Create as New.";
        
        $('#document-delete-button').hide();
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
