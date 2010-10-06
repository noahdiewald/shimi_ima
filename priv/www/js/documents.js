function getDocument(id) {
  var url = "documents/" + id;
  
  $.get(url, function(documentHtml) {
    $('#document-view').html(documentHtml);
  });
}

$(function () {
  $('.view-document-link').click(function() {
    getDocument(this.hash.slice(1));
  });
});
