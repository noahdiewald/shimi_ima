// Initialize the form in the edit pane

function initEdit() {
  var url = "documents/edit";
  
  $.get(url, function(documentEditHtml) {

    $('#document-edit').html(documentEditHtml);
    $('#edit-tabs').tabs();
    setKeyboardEvents();
    arrangeTabBar();
    initFieldsets();
    initEditButtons();
  });
  
  return true;
}

// This function initializes all the keyboard events for the
// edit pane
function setKeyboardEvents() {
  var inputable = 'input, select';
  var t = $('#edit-tabs');
  
  var selectInput = function() {
    var cur = t.find('.ui-tabs-selected a').attr('href');
    $(cur).find(inputable + ", textarea").first().focus();
  };
  
  $(document).bind('keydown', 'Alt+p', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected != 0) {
      t.tabs('select', selected - 1);
      selectInput();
    } else {
      t.tabs('select', totaltabs - 1);
      selectInput();
    }
    
    return false;
  });
  
  $(document).bind('keydown', 'Alt+n', function(e) {
    var totaltabs = t.tabs('length');
    var selected = t.tabs('option', 'selected');
    
    if (selected < totaltabs - 1) {
      t.tabs('select', selected + 1);
      selectInput();
    } else {
      t.tabs('select', 0);
      selectInput();
    }
    
    return false;
  });
}

// This reshapes the tab bar in the edit pane to better
// fit in the available space
function arrangeTabBar() {
  var tabs = $('#tab-list li');
  var containerWidth = $('#tabs-container').width();
  var tabsWidth;
  
  tabsWidth = tabs.toArray().reduce(function(acc, elem) {
    var width = elem.offsetWidth;
    
    if (isNaN(width)) {
      return acc;
    } else {
      return acc + elem.offsetWidth;
    }
  }, 50);
  
  if (containerWidth < tabsWidth) {
    tabs.switchClass('ui-corner-top', 'ui-corner-all');
    tabs.children().css('padding', '2px');
    tabs.children().css('font-weight', 'normal');
    tabs.parent().css('padding-bottom', '4px');
  }
}

// Functions for initializing buttons

// Initialize buttons for form in right column
function initEditButtons() {
  initAddButton();
  initSaveButton();
  initCreateButton();
  initClearButton();
  return true;
}

// Initialize the button that will add additional fieldsets to
// a multiple fieldset 
function initAddButton() {
  $(".add-button").button({icons: {primary: "ui-icon-plus"}});
  
  return true;
}

// Initialize the button that removes a fieldset from a multiple fieldset
function initRemoveButton() {  
  return $(".remove-button").button({icons: {primary: "ui-icon-minus"}});
}

// Initialize the save button that is used for already existing documents.
// Hide the button until the form has been filled by an already existing
// document's values. See the after functions above.
function initSaveButton() {
  $('#save-document-button').button({ icons: {primary: "ui-icon-disk"}});
  $('#save-document-button').hide();
  return $('#save-document-button').attr('disabled', 'disabled');
}

// When a form validation error has occurred, this will cause it to
// be displayed
function setInvalidError(req) {
  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = $('[data-field-instance=' + body.instance + ']');
  var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');
  
  invalidTab.addClass('ui-state-error');
  invalid.addClass('ui-state-error');
  
  flashError(title, body.fieldname + " " + body.message);
}

// Initialize the create as new button used to create a new document based
// on filled in values.
function initCreateButton() {
  return $("#create-document-button").button({icons: {primary: "ui-icon-document"}});
}

// Initialize the clear button that will rebuild the form fresh.
function initClearButton() {
  return $('#clear-document-button').button({icons: {primary: "ui-icon-refresh"}});
}

// Functions to run after the form is refreshed or filled.

// This is run after the form is refreshed with new values inserted.
function afterEditRefresh() {
  var saveBttn = $('#save-document-button');
  var editBttn = $('#document-edit-button');
  var sharedAttrs = ['data-document-id', 'data-document-rev'];
  
  sharedAttrs.forEach(function(elem) {
    saveBttn.attr(elem, editBttn.attr(elem));
  });
  
  saveBttn.show();
  
  afterRefresh();
  
  return true;
}

// This is run after the form is cleared or created empty
function afterFreshRefresh() {
  initRemoveButton();
  afterRefresh();
  return true;
}

// This should be run after either afterFreshRefresh() or afterEditRefresh()
function afterRefresh() {
  initDateFields();
  initInstances();
  return true;
}

var initInstances = function() {
  var text = ['0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f',
              '0','1','2','3','4','5','6','7','8','9', 'a','b','c','d','e','f'];  
  var makeInstance = function() {
    return text.map(function() {
                      return text[Math.floor(Math.random() * text.length)];
                    }).join('');
  };

  $("[data-field-instance]").each(
    function(index, item) {
      var newInstance = makeInstance();
      $(item).first().attr('data-field-instance', newInstance);
      $(item).first().attr('data-group-id', newInstance);
      $(item).first().attr('id', newInstance);
      $(item).first().next().next('.expander')
        .attr('data-group-id', newInstance);
    });

  return true;
};

function resetFields() {
  $('.field').each(function(index) {
    var field = $(this);
    var thedefault = field.attr('data-field-default');
    
    if (thedefault && thedefault != '') {
      if (field.is('select.multiselect')) {
        field.val(thedefault.split(","));
      } else if (field.is('input.boolean')) {
        field.attr('checked', thedefault == true);
      } else {
        field.val(thedefault);
      }
    } else {
      field.val('');
      field.removeAttr('checked');
    }
  });
}
