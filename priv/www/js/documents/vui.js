// View pane UI elements

var vui = function(args) {
  var mod = {};
  
  mod.target = args.target;
  mod.rev = args.rev;
  mod.id = args.id;
  
  mod.formatTimestamps = function() {
    $('.timestamp').each(
      function(i, item) {
        var newDate = (new Date($(item).text())).toLocaleString();
        if (newDate !== "Invalid Date") {
          $(item).text(newDate);
        }
      });
      
     return mod;
  };
  
  mod.rev = function(rev) {
    mod.rev = rev;
    
    return mod;
  };
  
  mod.id = function(id) {
    mod.id = id;
    
    return mod;
  };
  
  mod.target = function(target) {
    mod.target = target;
    
    return mod;
  };
  
  mod.get = function(callback) {
    var url = "documents/" + mod.id;
    var dvt = $('#document-view');
    
    if (mod.rev) {
      url = "documents/" + mod.id + "/" + mod.rev;
    }
    
    dvt.hide();
    
    $.get(url, function(documentHtml) {
      dvt.html(documentHtml);
      mod.formatTimestamps();
      if (callback) {
        callback();
      }
      dvt.show();
      
      if (!mod.rev) {
        var restoreButton = $('#document-restore-button');
        var editButton = $('#document-edit-button');
        var deleteButton = $('#document-delete-button');
       
        editButton.button({icons: {primary: 'ui-icon-pencil'}});
        deleteButton.button({icons: {primary: 'ui-icon-trash'}});
        restoreButton.button({icons: {primary: 'ui-icon-refresh'}});
        
        if (store(restoreButton).d("deleted") === "true") {
          editButton.hide();
          deleteButton.hide();
        } else {
          restoreButton.hide();
        }
      }
    });
    
    return mod;
  };
  
  mod.restore = funtion() {
    var url = "./documents/" + mod.id + "?rev=" + mod.rev;
    var restoreButton = $('#document-restore-button');
    
    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function(req, status) {
        if (req.status === 200) {
          var title = "Success";
          var body = "Your document was restored.";
  
          mod.rev(null).get(function() {getIndex();});
          flashHighlight(title, body);
        } else if (req.status === 409) {
          var body = JSON.parse(req.responseText);
          var title = req.statusText;
            
          flashError(title, body.message);
        } else if (req.status === 404) {
          var body = "Document was erased and cannot be restored.";
          var title = req.statusText;
            
          flashError(title, body);
        }
      }
    });
  
    return mod;
  };
  
  mod.delete = function() {
    var url = "./documents/" + mod.id + "?rev=" + mod.rev;
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
          
          store(restoreButton).put("document-rev", response.rev);
          
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
    
    return mod;
  };
  
  mod.confirmIt = function(f) {
    if (confirm("Are you sure?")) {
      var s = store(target);
      var id = s.d("document");
      var rev = s.d("rev");
      
      f(id, rev);
    }
      
     return mod;
  };
  
  mod.edit = function() {
    resetFields();
    if ($('#document-view-tree').hasClass('oldrev')) {
      $('#save-document-button').addClass('oldrev');
    } else {
      $('#save-document-button').removeClass('oldrev');
    }
    fillFieldsets();
    
    return mod;
  };
  
  mod.confirmDelete = function() {
    return mod.confirmIt(function(d, r) {mod.id(d).rev(r).delete();});
  };
  
  mod.confirmRestore = function() {
    return mod.confirmIt(function(d, r) {mod.id(d).rev(r).restore();});
  };
  
  mod.collapseToggle = function() {
    target.parent('li').toggleClass('collapsed');
    
    return mod;
  };
  
  mod.fetchRevision = function() {
    var s = store(target);
    var id = s.d("document");
    var rev = s.d("rev");
    var oldrev = target.attr("data-document-oldrev");
  
    if (rev != oldrev) {
      $('#document-view-tree').addClass('oldrev');
    } else {
      $('#document-view-tree').removeClass('oldrev');
    }
  
    getRevision(id, oldrev);
    
    return mod;
  };
  
  return mod;
};
