$(function () {
  shimi.fm().refreshListings();
  
  $('#file-upload-target').load(function() {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function () {
      if (encoded && encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {message: false};
      }
    };
    
    if (obj() && obj().message && obj().status === "error") {
      shimi.flash("Error", obj().message).error();
      shimi.fm().refreshListings();
    } else if (obj().message) {
      shimi.flash("Success", obj().message).highlight();
      shimi.fm().refreshListings();
    }
  });
});