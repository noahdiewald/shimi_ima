var getFileListing = function () {
  $.get("file_manager/index", function (data) {
    $('#file-listing').html(data);
    $('.edit-file-button').button();
    $('.delete-file-button').button();
  });
};

$(function () {
  getFileListing();
  
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
      getFileListing();
    }
  });
});