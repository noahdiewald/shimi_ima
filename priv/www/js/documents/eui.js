// Edit pane UI elements

shimi.eui = function() {
  var mod = {};
  
  // Imports
  var iui = shimi.iui;
  var vui = shimi.vui;
  var efs = shimi.efs;
  var store = shimi.store;
  var flash = shimi.flash;
  
  // UI Elements
  var saveButton = $('#save-document-button');
  var addButton = $(".add-button");
  var clearButton = $('#clear-document-button');
  var createButton = $('#create-document-button');
  var editButton = $('#document-edit-button');
  var removeButton = $(".remove-button");
  
  var keyboard = function() {
    var inputable = 'input, select';
    var t = $('#edit-tabs');
    
    var selectInput = function() {
      var cur = t.find('.ui-tabs-selected a').attr('href');
      $(cur).find(inputable + ", textarea").first().focus();
    };
    
    $(document).bind('keydown', 'Alt+p', function(e) {
      var totaltabs = t.tabs('length');
      var selected = t.tabs('option', 'selected');
      
      if (selected !== 0) {
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
  
    return mod;
  };
  
  var buttons = function() {
    addButton.button({icons: {primary: "ui-icon-plus"}});
    saveButton.button({ icons: {primary: "ui-icon-disk"}});
    saveButton.hide();
    saveButton.attr('disabled', 'disabled');
    createButton.button({icons: {primary: "ui-icon-document"}});
    clearButton.button({icons: {primary: "ui-icon-refresh"}});
  
    return mod;
  };
  
  var rbutton = function() {
    removeButton.button({icons: {primary: "ui-icon-minus"}});
    
    return mod;
  };
  
  var validationError = function(req) {
    var body = JSON.parse(req.responseText);
    var title = req.statusText;
  
    var invalid = $('[data-field-instance=' + body.instance + ']');
    var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');
    
    invalidTab.addClass('ui-state-error');
    invalid.addClass('ui-state-error');
    
    flash(title, body.fieldname + " " + body.message).error();
  
    return mod;
  };
  
  var afterEditRefresh = function() {
    var sharedAttrs = ['data-document-id', 'data-document-rev'];
    
    sharedAttrs.forEach(function(elem) {
      saveButton.attr(elem, editButton.attr(elem));
    });
    
    saveButton.show();
    afterRefresh();
    
    return mod;
  };
  
  var afterRefresh = function() {
    shimi.form().initDateFields();
    instances();
    
    return mod;
  };
  
  var instances = function() {
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
        $(item).first().next('.expander')
          .attr('data-group-id', newInstance);
        $(item).first().next().next('.expander')
          .attr('data-group-id', newInstance);
      });
  
      return mod;
  };
  
  mod.init = function() {
    var url = "documents/edit";
    
    $.get(url, function(documentEditHtml) {
  
      $('#document-edit').html(documentEditHtml);
      $('#edit-tabs').tabs();
      keyboard();
      efs().initFieldsets();
      buttons();
    });
  
    return mod;
  };
  
  mod.afterFreshRefresh = function() {
    rbutton();
    afterRefresh();
  
    return mod;
  };
  
  mod.resetFields = function() {
    $('.field').each(function(index) {
      var field = $(this);
      var thedefault = field.attr('data-field-default');
      
      if (thedefault && thedefault !== '') {
        if (field.is('select.multiselect')) {
          field.val(thedefault.split(","));
        } else if (field.is('input.boolean')) {
          field.attr('checked', thedefault === true);
        } else {
          field.val(thedefault);
        }
      } else {
        field.val('');
        field.removeAttr('checked');
      }
    });
    
    return mod;
  };
  
  mod.save = function() {
    if (saveButton.hasClass('oldrev')) {
      if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
        return false;
      }
    }
    
    var body;
    var title;
    var s = store(saveButton);
    var root = $('#edit-document-form');
    var document = s.d("document");
    var rev = s.d("rev");
    var url = "./documents/" + document + "?rev=" + rev;
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton.button('disable');
    $.extend(obj, efs().fieldsetsToObject(root));
    
    $.ajax({
             type: "PUT",
             url: url,
             dataType: "json",
             contentType: "application/json",
             processData: false,
             data: JSON.stringify(obj),
             complete: function(req, status) {
               if (req.status === 204 || req.status === 200) {
                 title = "Success";
                 body = "Your document was saved.";
                 vui({id: document}).get();
                 iui().get();
                 flash(title, body).highlight();
                 saveButton.removeClass('oldrev');
                 saveButton.button('enable');
               } else if (req.status === 403) {
                 validationError(req);
                 saveButton.button('enable');
               } else if (req.status === 409) {
                 body = JSON.parse(req.responseText);
                 title = req.statusText;
                 
                 flash(title, body.message).error();
                 saveButton.button('enable');
               }
             }
           });
  };

  mod.create = function() {  
    var s = store(createButton);
    var root = $('#edit-document-form');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    createButton.button('disable');
    $.extend(obj, efs().fieldsetsToObject(root));
    
    var postUrl = $.ajax({
      type: "POST",
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: JSON.stringify(obj),
      complete: function(req, status) {
        if (req.status === 201) {
          var title = "Success";
          var body = "Your document was created.";
          var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);
          
          saveButton.hide();
          saveButton.attr('disabled','true');
          $('.fields').remove();
          efs().initFieldsets();
          vui({id: documentId}).get();
          iui().get();
          flash(title, body).highlight();
          createButton.button('enable');
        } else if (req.status === 403) {
          validationError(req);
          createButton.button('enable');
        }
      }
    });
  };
  
  mod.clear = function() {
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    saveButton.hide();
    saveButton.attr('disabled','disabled');
    $('.fields').remove();
    efs().initFieldsets();
  };
  
  mod.removeFieldset = function(target) {
    target.parent().remove();
  };
  
  mod.showHelpDialog = function(target) {
    if (target.is('.label-text')) {
      target = target.parent('label').find('.ui-icon-help');
    }
    
    $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));
    
    return mod;
  };
  
  mod.toggleTextarea = function(target) {
    var textarea = $('#' + target.attr('data-group-id'));
    
    textarea.toggleClass('expanded');
    target.toggleClass('expanded');
    
    return mod;
  };
  
  return mod;
};