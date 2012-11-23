// functions added to String

// Simple 'parser' for quoted values

String.prototype.parseQuoted = function(quoteChar) {
  var quoteChar = (quoteChar || "'");
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
  return ((/^\s*$/).test(this) && ! (/\S/).test(this) && ! (this === null));
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

// Dialog form helpers

// TODO: maybe saying $('input, select, textarea...) could serve
//       as alternative to variable passing.
function clearValues(inputFields) {
  inputFields.each(function(index) {
    var inputField = $(this);
    
    if (! inputField.attr('data-retain')) {
      if (inputField.is(':checked')) {
        inputField.attr('checked', false);
      }
      inputField.val('');
    }
  });
  
  return inputFields;
}

function sendConfigDoc(ajaxUrl, obj, method, completeFun, callContext) {
  var dataObj;

  if (obj) {
    dataObj = JSON.stringify(obj);
  }

  $.ajax({
    type: method,
    url: ajaxUrl,
    dataType: "json",
    context: callContext,
    contentType: "application/json",
    processData: false,
    data: dataObj,
    complete: function(req, status) {
      if (req.status >= 200 && req.status < 300) {
        completeFun(this, req);
      } else if (req.status == 500) {
        flashError("Unknown Server Error", "Please report that you received " +
                   "this message");
      } else if (req.status >= 400) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
        
        flash(title, body.fieldname + " " + body.message).error();
      }
    }
  });
}

// Validation
  
function updateTips(t, tips) {
  tips.text(t).addClass('ui-state-highlight');
  setTimeout(function() {
    tips.removeClass('ui-state-highlight', 1500);
  }, 500);
}

function checkLength(o, n, min, max, tips) {
  if ( o.val().length > max || o.val().length < min ) {
    o.addClass('ui-state-error');
    updateTips("Length of " + n + " must be between " + min + " and " + max + ".", tips);
    return false;
  } else {
    return true;
  }
}

function checkRegexp(o, regexp, n, tips) {
  if ( !( regexp.test( o.val() ) ) ) {
    o.addClass('ui-state-error');
    updateTips(n, tips);
    return false;
  } else {
    return true;
  }
}

// Date Picker

function initDateFields() {
  $(".date").datepicker({dateFormat: "yy-mm-dd"});
}

var fillOptionsFromUrl = function(url, selectElement, callback) {
  $.get(url, function(options) {
    selectElement.html(options);
    if (callback) callback();
  });
  
  return false;
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
  
  initDateFields();
  
  // Sortable
  
  $("#sortable").sortable({
    update: function(event, ui) {
      var lis = $(event.target).children('li');
      lis.forEach(function(li) {$(li).text($(li).text() + " " + x);});
    }
  });
});