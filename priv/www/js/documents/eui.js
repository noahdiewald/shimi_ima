// Edit pane UI elements

©.eui = function(args) {
  var mod = {};
  mod.priv = {};
  
  mod.target = args.target;
  mod.rev = args.rev;
  mod.id = args.id;
  mod.saveButton = $('#save-document-button');
  mod.addButton = $(".add-button");
  mod.clearButton = $('#clear-document-button');
  mod.createButton = $('#create-document-button');
  mod.editButton = $('#document-edit-button');
  mod.removeButton = $(".remove-button");
  
  mod.init = function() {
    var url = "documents/edit";
    
    $.get(url, function(documentEditHtml) {
  
      $('#document-edit').html(documentEditHtml);
      $('#edit-tabs').tabs();
      mod.priv.keyboard();
      initFieldsets();
      mod.priv.buttons();
    });
  
    return mod;
  };
  
  mod.priv.keyboard = function() {
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
  
  mod.priv.buttons = function() {
    mod.addButton.button({icons: {primary: "ui-icon-plus"}});
    mod.saveButton.button({ icons: {primary: "ui-icon-disk"}});
    mod.saveButton.hide();
    mod.saveButton.attr('disabled', 'disabled');
    mod.createButton.button({icons: {primary: "ui-icon-document"}});
    mod.clearButton.button({icons: {primary: "ui-icon-refresh"}});
  
    return mod;
  };
  
  mod.priv.removeButton = function() {
    mod.removeButton.button({icons: {primary: "ui-icon-minus"}});
    
    return mod;
  };
  
  mod.priv.validationError = function(req) {
    var body = JSON.parse(req.responseText);
    var title = req.statusText;
  
    var invalid = $('[data-field-instance=' + body.instance + ']');
    var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');
    
    invalidTab.addClass('ui-state-error');
    invalid.addClass('ui-state-error');
    
    flash(title, body.fieldname + " " + body.message).error();
  
    return mod;
  };
  
  mod.priv.afterEditRefresh = function() {
    var sharedAttrs = ['data-document-id', 'data-document-rev'];
    
    sharedAttrs.forEach(function(elem) {
      mod.saveButton.attr(elem, mod.editButton.attr(elem));
    });
    
    mod.saveButton.show();
    mod.priv.afterRefresh();
    
    return mod;
  };
  
  mod.priv.afterFreshRefresh = function() {
    mod.priv.removeButton();
    mod.priv.afterRefresh();
  
    return mod;
  };
  
  mod.priv.afterRefresh = function() {
    mod.priv.dateFields();
    mod.priv.instances();
    
    return mod;
  };
  
  mod.priv.instances = function() {
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
    if (mod.saveButton.hasClass('oldrev')) {
      if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
        return false;
      }
    }
    
    var body;
    var title;
    var s = store(mod.saveButton);
    var root = $('#edit-document-form');
    var document = s.d("document");
    var rev = s.d("rev");
    var url = "./documents/" + document + "?rev=" + rev;
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    mod.saveButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
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
                 mod.saveButton.removeClass('oldrev');
                 mod.saveButton.button('enable');
               } else if (req.status === 403) {
                 mod.priv.validationError(req);
                 mod.saveButton.button('enable');
               } else if (req.status === 409) {
                 body = JSON.parse(req.responseText);
                 title = req.statusText;
                 
                 flash(title, body.message).error();
                 mod.saveButton.button('enable');
               }
             }
           });
  };

  mod.create = function() {  
    var s = store(mod.createButton);
    var root = $('#edit-document-form');
    var obj = {
      doctype: s.d("doctype"),
      description: s.d("description")
    };
    
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    mod.createButton.button('disable');
    $.extend(obj, fieldsetsToObject(root));
    
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
          
          mod.saveButton.hide();
          mod.saveButton.attr('disabled','true');
          $('.fields').remove();
          initFieldsets();
          vui({id: documentId}).get();
          iui().get();
          flash(title, body).highlight();
          mod.createButton.button('enable');
        } else if (req.status === 403) {
          mod.priv.validationError(req);
          mod.createButton.button('enable');
        }
      }
    });
  };
  
  mod.clear = function() {
    $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
    mod.saveButton.hide();
    mod.saveButton.attr('disabled','disabled');
    $('.fields').remove();
    initFieldsets();
  };
  
  mod.removeFieldset = function() {
    mod.target.parent().remove();
  };
  
  mod.showHelpDialog = function() {
    if (mod.target.is('.label-text')) {
      mod.target = mod.target.parent('label').find('.ui-icon-help');
    }
    
    $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(mod.target.attr('title'));
  };
  
  mod.toggleTextarea = function() {
    var textarea = $('#' + mod.target.attr('data-group-id'));
    
    textarea.toggleClass('expanded');
    mod.target.toggleClass('expanded');
  };
  
  return mod;
};