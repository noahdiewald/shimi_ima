shimi.iiui = function() {
  var mod = {};

  mod.init = function() {
    var url = "indexes";
    var target = $('#index-index-listing');
    
    $.get(url, function(index) {
            target.html(index);
            target.click(function(e) {
                           shimi.ieui().init($(e.target).attr('data-index-id'));
                         });
          });
          
    return mod;
  };

  return mod;
};
