$(function () {
  $('#file-upload-form').submit(function(e) {
    e.target = 'file-upload-target';
  });
});