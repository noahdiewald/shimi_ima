var shimi = {};

// functions added to String

String.prototype.isBlank = function() {
  return ((/^\s*$/).test(this) && !(/\S/).test(this) && (this !== null));
};

String.prototype.trim = function() {
  return this.replace(/^\s+/,'').replace(/\s+$/,'');
};

// functions added to Array
Array.prototype.trimAll = function() {
  return this.map(function (i) {
                    return i.trim();
                  }).filter(function (i) {
                              return !i.match(/^$/);
                            });
};

// General UI Stuff

shimi.panelToggle = function() {
  var toggler = function(e) {
    var panel;
    
    if ($(e.target).attr('data-panel')) {
      panel = $('#' + $(e.target).attr('data-panel'));
    } else {
      panel = $(e.target).closest('.panel');
    }
    panel.toggle();
  };

  $('#panel-toggle li')
    .live("click", function(e) {toggler(e);});
  $('.panel > h2')
    .live("dblclick", function(e) {toggler(e);});
};
