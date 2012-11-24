// functions added to String

// Simple 'parser' for quoted values

String.prototype.parseQuoted = function(qChar) {
  var quoteChar = (qChar || "'");
  var outArray = [];
  var inArray = this.split('');
  var inQuote = false;
  var quoteCount = 0;
  var currCell = [];
  
  var onQuote = function() {
    if (inQuote && (quoteCount % 2 === 0)) {
      ++quoteCount;
    } else if (inQuote && (quoteCount % 2 === 1)) {
      ++quoteCount;
      currCell.push("'");
    } else if (!inQuote) {
      inQuote = true;
    }
  };
  
  var outQuote = function() {
    outArray.push(currCell.join(''));
    currCell = [];
    quoteCount = 0;
    inQuote = false;
  };
  
  inArray.forEach(function(item) {
    if (/'/.test(item)) {
      onQuote();
    } else if (quoteCount % 2 === 1) {
      outQuote();
    } else if (quoteCount % 2 === 0) {
      quoteCount = 0;
      if (inQuote) {
        currCell.push(item);
      } else if (/\S/.test(item)) {
        return false;
      }
    }
  });
  
  outArray.push(currCell.join(''));
  
  return outArray;
};

String.prototype.isBlank = function() {
  return ((/^\s*$/).test(this) && (!(/\S/).test(this)) && (!(this === null)));
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

var uiToggle = function() {
  var toggler = function(e) {
    var toggleElem;

    if ($(e.target).attr('data-target')) {
      toggleElem = $('#' + $(e.target).attr('data-target'));
      toggleElem.toggle();
    }
  };

  $('.toggler').live("click", function(e) {toggler(e);});
};

var panelToggle = function() {
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
 
$(function () {
    $('.notification').hide();
  
    $('#loading').hide()
      .ajaxStart(function() {
                   $(this).show();
                 })
      .ajaxStop(function() {
                  $(this).hide();
                });

    panelToggle();
    uiToggle();    
  // Buttons
  
  $(".remove-button").button({
    icons: {primary: "ui-icon-minus"},
    text: false
  }).click(function() {
    $(this).parent().remove();
  });
  
  $(".help-button").button({
    icons: {primary: "ui-icon-help"},
    text: false
  });
  
  $(".link-button").button({
    icons: {primary: "ui-icon-link"}
  });
  
  $(".edit-button").button({
    icons: {primary: "ui-icon-pencil"}
  });
  
  $(".create-continue-button").button({
    icons: {
      primary: "ui-icon-disk",
      secondary: "ui-icon-arrowthick-1-e"
    }
  });
  
  shimi.form().initDateFields();
});