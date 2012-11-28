shimi.fm = (function() {
  var mod = {};
  
  var getDirListing = function(path) {
    if (path === undefined) {
      path = "";
    }
    
    $.get("file_manager/list_dirs/" + path, function (data) {
      $('#file-paths').html(data);
    });
  };
  
  mod.goDir = function(target) {
    var newpath = $(target).attr('data-path');
    mod.refreshListings(newpath);
    
    return mod;
  };
  
  mod.rootDir = function() {
    mod.refreshListings();
    
    return mod;
  };
  
  mod.upDir = function() {
    var newpath = path.split("/");
    newpath.pop();
    newpath = newpath.join("/");
        
    mod.refreshListings(newpath);
    
    return mod;
  };
  
  var getFileListing = function (path) {
    if (path === undefined) {
      path = "";
    }
    
    $.get("file_manager/list_files/" + path, function (data) {
      $('#file-listing').html(data);
    });
  };
  
  var editFile = function(target) {
    var fileId = target.attr('data-file-id');
    var url = "file_manager/" + fileId;
        
    $.getJSON(url, function (obj) {
      pathEditDialog(obj, path).dialog('open');
    });
    
    return mod;
  };
  
  // Used to take path
  mod.deleteFile = function(target) {
    var fileId = target.attr('data-file-id');
    var fileRev = target.attr('data-file-rev');
    var url = "file_manager/" + fileId + "?rev=" + fileRev;
    var complete = function () {
      mod.refreshListings(path);
      shimi.flash("Success", "File Deleted").highlight();
    };
        
    shimi.form.send(url, null, 'DELETE', complete, target);
    
    return mod;
  };
  
  var pathEditDialog = function (obj, path) {
    var pathInput = $('#file-path-input');
    
    if (obj.path) {
      pathInput.val(obj.path.join("/"));
    } else {
      pathInput.val('');
    }
    
    var dialog = $('#edit-path-dialog').dialog({
      autoOpen: false,
      modal: true,
      buttons: {
        "Move": function () {
          var url = "file_manager/" + obj._id + "?rev=" + obj._rev;
          var complete = function () {
            mod.refreshListings(path);
            shimi.flash("Success", "File Moved").highlight();
          };
          
          obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split("/");
          shimi.form.send(url, obj, 'PUT', complete, dialog);
          $(this).dialog("close");
        },
        "Cancel": function() {
          $(this).dialog("close");
        }
      }
    });
    
    return dialog;
  };
  
  mod.refreshListings = function(path) {
    getDirListing(path);
    getFileListing(path);
  };
  
  return mod;
})();
