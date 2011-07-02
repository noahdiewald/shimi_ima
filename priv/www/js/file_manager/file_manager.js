var getDirListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_dirs/" + path, function (data) {
    $('#file-paths').html(data);
    $('.dir').click(function (e) {
      var newpath = $(e.target).attr('data-path');
      
      refreshListings(newpath);
    });
    $('#up-dir').button().click(function () {
      var newpath = path.split("/");
      newpath.pop();
      newpath = newpath.join("/");
      
      refreshListings(newpath);
    });
    $('#root-dir').button().click(function () {
      refreshListings();
    });
  });
};

var getFileListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_files/" + path, function (data) {
    $('#file-listing').html(data);
    refreshButtons(path);
  });
};

var refreshListings = function (path) {
  getDirListing(path);
  getFileListing(path);
};

var refreshButtons = function (path) {
  refreshEditButton();
  refreshDeleteButton();
};

var refreshEditButton = function (path) {
    $('.edit-file-button').button({
      icons: {primary: 'ui-icon-pencil'}
    }).click(function (e) {
      target = $(e.target).parent('a');
      fileId = target.attr('data-file-id');
      url = "file_manager/" + fileId;
      
      $.getJSON(url, function (obj) {
        pathEditDialog(obj, path).dialog('open');
    });
};

var refreshDeleteButton = function (path) {
    $('.delete-file-button').button({
      icons: {primary: 'ui-icon-trash'}
    }).click(function(e) {
      target = $(e.target).parent('a');
      fileId = target.attr('data-file-id');
      fileRev = target.attr('data-file-rev');
      url = "file_manager/" + fileId + "?rev=" + fileRev;
      complete = function () {
        refreshListings(path);
        flashHighlight("Success", "File Deleted");
      };
      
      sendConfigDoc(url, null, 'DELETE', complete, target);
    });
};

var pathEditDialog = function (obj, path) {
  var pathInput = $('#file-path-input');
  
  pathInput.val(obj.path.join("/");
  
  var dialog = $('#edit-path-dialog').dialog(
    autoOpen: false,
    modal: true,
    buttons: {
      "Move": function () {
        var url = "file_manager/" + obj._id + "?rev=" + obj._rev;
        complete = function () {
          refreshListings(path);
          flashHighlight("Success", "File Moved");
        };
        
        obj.path = pathInput.val().replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split("/");
        sendConfigDoc(url, obj, 'PUT', complete, dialog);
        $(this).dialog("close");
      }
      "Cancel": function() {
        $(this).dialog("close");
      }
    }
  });
  
  return dialog;
};

$(function () {
  refreshListings();
  
  $('#file-upload-target').load(function() {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function () {
      if (encoded) if (encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {message: false};
      }
    };
    
    if (obj()) if (obj().message && obj().status === "error") {
      flashError("Error", obj().message);
      refreshListings();
    } else if (obj().message) {
      flashHighlight("Success", obj().message);
      refreshListings();
    }
  });
});