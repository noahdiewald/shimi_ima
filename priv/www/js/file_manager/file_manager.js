$(function () {
  $('#file-upload-target').load(function() {
    var encoded = $('#file-upload-target').contents().find('body').html();
    var obj = JSON.parse(encoded);
    
    if (obj.message) {
      flashError("Error", obj.message);
    } else {
      flashHighlight("Success", "Your file has been uploaded");
    }
  });
});