// View pane UI elements

shimi.vui = function(args) {
  var mod = {};
  var vui = shimi.vui;
  var iui = shimi.iui;
  var eui = shimi.eui;
  var efs = shimi.efs();
  var store = shimi.store;
  var flash = shimi.flash;
  
  mod.evTarget = args.target;
  mod.docRev = args.rev;
  mod.docId = args.id;
  
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
    mod.docRev = rev;
    
    return mod;
  };
  
  mod.id = function(id) {
    mod.docId = id;
    
    return mod;
  };
  
  mod.target = function(target) {
    mod.evTarget = target;
    
    return mod;
  };
  
  mod.get = function(callback) {
    var url = "documents/" + mod.docId;
    var dvt = $('#document-view');
    
    if (mod.docRev) {
      url = "documents/" + mod.docId + "/" + mod.docRev;
    }
    
    $.get(url, function(documentHtml) {
      dvt.html(documentHtml);
      mod.formatTimestamps();
      if (callback) {
        callback();
      }
      
      if (!mod.docRev) {
        var restoreButton = $('#document-restore-button');
        var editButton = $('#document-edit-button');
        var deleteButton = $('#document-delete-button');
       
        editButton.button({icons: {primary: 'ui-icon-pencil'}});
        deleteButton.button({icons: {primary: 'ui-icon-trash'}});
        restoreButton.button({icons: {primary: 'ui-icon-refresh'}});
        
        if (store(restoreButton).d("deleted") === "true") {
          $('#document-view').fadeTo('slow', 0.5);
          editButton.hide();
          deleteButton.hide();
        } else {
          $('#document-view').fadeTo('slow', 1);
          restoreButton.hide();
        }
      }
    });
    
    return mod;
  };
  
  mod.restore = function() {
    var url = "./documents/" + mod.docId + "?rev=" + mod.docRev;
    var restoreButton = $('#document-restore-button');
    var body;
    var title;
    
    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function(req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was restored.";
          
          mod.rev(null).get(function() {
            $('#document-view').fadeTo('slow', 1);
            iui().get();
          });
          flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;
            
          flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document was erased and cannot be restored.";
          title = req.statusText;
            
          flash(title, body).error();
        }
      }
    });
  
    return mod;
  };
  
  mod.del = function() {
    var url = "./documents/" + mod.docId + "?rev=" + mod.docRev;
    var restoreButton = $('#document-restore-button');
    var body;
    var title;
    
    $.ajax({
      type: "DELETE",
      url: url,
      dataType: "json",
      contentType: "application/json",
      complete: function(req, status) {
        if (req.status === 200) {
          title = "Success";
          body = "Your document was deleted.";
          var response = JSON.parse(req.responseText);
          
          store(restoreButton).put("document-rev", response.rev);
          
          $('#document-delete-button').hide();
          $('#document-edit-button').hide();
          restoreButton.show();
          $('#document-view h2').text("Deleted Document");
          $('#document-view').fadeTo('slow', 0.5);
          
          iui().get();
          flash(title, body).highlight();
        } else if (req.status === 409) {
          body = JSON.parse(req.responseText);
          title = req.statusText;
            
          flash(title, body.message).error();
        } else if (req.status === 404) {
          body = "Document appears to have been deleted already.";
          title = req.statusText;
            
          flash(title, body).error();
        }
      }
    });
    
    return mod;
  };
  
  mod.confirmIt = function(f) {
    if (window.confirm("Are you sure?")) {
      var s = store(mod.evTarget);
      var id = s.d("document");
      var rev = s.d("rev");
      
      f(id, rev);
    }
      
     return mod;
  };
  
  mod.edit = function() {
    eui().resetFields();
    if ($('#document-view-tree').hasClass('oldrev')) {
      $('#save-document-button').addClass('oldrev');
    } else {
      $('#save-document-button').removeClass('oldrev');
    }
    efs.fillFieldsets();
    
    return mod;
  };
  
  mod.confirmDelete = function() {
    return mod.confirmIt(function(d, r) {mod.id(d).rev(r).del();});
  };
  
  mod.confirmRestore = function() {
    return mod.confirmIt(function(d, r) {mod.id(d).rev(r).restore();});
  };
  
  mod.collapseToggle = function() {
    mod.evTarget.parent('li').toggleClass('collapsed');
    
    return mod;
  };
  
  mod.fetchRevision = function() {
    var s = store(mod.evTarget);
    var id = s.d("document");
    var rev = s.d("rev");
    var oldrev = s.d("oldrev");
  
    if (rev !== oldrev) {
      $('#document-view-tree').addClass('oldrev');
    } else {
      $('#document-view-tree').removeClass('oldrev');
    }
  
    vui({rev: oldrev, id: id}).get();
    
    return mod;
  };
  
  return mod;
};
