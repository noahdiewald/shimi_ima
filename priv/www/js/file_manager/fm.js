shimi.fm = function() {
  var mod = {};
  
  var getDirListing = function (path) {
    if (path === undefined) {
      path = "";
    }
    
    $.get("file_manager/list_dirs/" + path, function (data) {
      $('#file-paths').html(data);
      $('.dir').click(function (e) {
        var newpath = $(e.target).attr('data-path');
        
        mod.refreshListings(newpath);
      });
      $('#up-dir').button().click(function () {
        var newpath = path.split("/");
        newpath.pop();
        newpath = newpath.join("/");
        
        mod.refreshListings(newpath);
      });
      $('#root-dir').button().click(function () {
        mod.refreshListings();
      });
    });
  };
  
  var getFileListing = function (path) {
    if (path === undefined) {
      path = "";
    }
    
    $.get("file_manager/list_files/" + path, function (data) {
      $('#file-listing').html(data);
      refreshButtons(path);
    });
  };
  
  var refreshEditButton = function (path) {
      $('.edit-file-button').button({
        icons: {primary: 'ui-icon-pencil'}
      }).click(function (e) {
        var target = $(e.target).parent('a');
        var fileId = target.attr('data-file-id');
        var url = "file_manager/" + fileId;
        
        $.getJSON(url, function (obj) {
          pathEditDialog(obj, path).dialog('open');
        });
      });
  };
  
  var refreshDeleteButton = function (path) {
      $('.delete-file-button').button({
        icons: {primary: 'ui-icon-trash'}
      }).click(function(e) {
        var target = $(e.target).parent('a');
        var fileId = target.attr('data-file-id');
        var fileRev = target.attr('data-file-rev');
        var url = "file_manager/" + fileId + "?rev=" + fileRev;
        var complete = function () {
          mod.refreshListings(path);
          shimi.flash("Success", "File Deleted").highlight();
        };
        
        shimi.form.send(url, null, 'DELETE', complete, target);
      });
  };
  
  var refreshButtons = function (path) {
    refreshEditButton();
    refreshDeleteButton();
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
  
  mod.refreshListings = function (path) {
    getDirListing(path);
    getFileListing(path);
  };
  
  return mod;
};
