var getDirListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_dirs/" + path, function (data) {
    $('#file-paths').html(data);
    $('.dir').click(function (e) {
      var newpath = $(e.target).attr('data-path');
      getDirListing(newpath);
      getFileListing(newpath);
    });
    $('#up-dir').button().click(function () {
      var newpath = path.split("/");
      newpath.pop();
      newpath = newpath.join("/");
      
      getDirListing(newpath);
      getFileListing(newpath);
    });
    $('#root-dir').button().click(function () {
      getDirListing();
      getFileListing();
    });
  });
};

var getFileListing = function (path) {
  if (path === undefined) path = "";
  
  $.get("file_manager/list_files/" + path, function (data) {
    $('#file-listing').html(data);
    $('.edit-file-button').button();
    $('.delete-file-button').button();
  });
};

$(function () {
  $('#file-upload-target').load(function() {
    var encoded = $('#file-upload-target').contents().find('body').html();
    var obj = function () {
      if (encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {message: false};
      }
    };
    
    if (obj().message) {
      flashError("Error", obj.message);
    } else {
      getDirListing();
      getFileListing();
    }
  });
});