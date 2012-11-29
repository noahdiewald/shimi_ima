shimi.ieui = (function() {
  var mod = {};
  
  var tableBody = function () {
    return $('#index-conditions-listing tbody');
  };

  var editingData = function() {
    return $('#index-editing-data');
  };
  
  var fixArgumentType = function(argument, subcategory, operator) {
    switch (subcategory) {
    case "integer":
    case "rational":
      argument = argument * 1;
      break;
    }
  
    switch (operator) {
    case "hasExactly":
    case "hasGreater":
    case "hasLess":
      argument = Math.floor(argument * 1);
      break;
    }
    
    return argument;
  };
  
  var getIndexConditions = function(doctypeId, rows) {
    var conditions = rows.map(
      function(index, row) {
        row = $(row);
        var is_or = row.find('td.or-condition').attr('data-value') === "true";
        var paren = row.find('td.paren-condition').attr('data-value');
        var condition;
      
        if (is_or) {
          condition = {"is_or": true, "parens": false};
        } else if (paren) {
          condition = {"is_or": false, "parens": paren};
        } else {
          var fieldId = row.find('td.field-condition').attr('data-value');
          var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
          var argument = row.find('td.argument-condition').attr('data-value');
          var fieldDoc = shimi.ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
          var negate = 
            row.find('td.negate-condition').attr('data-value') === "true";
          var operator = row.find('td.operator-condition').attr('data-value');
  
          argument = fixArgumentType(argument, fieldDoc.subcategory, operator);
        
          condition = {
            "is_or": false,
            "parens": false,
            "negate": negate,
            "fieldset": fieldsetId,
            "field": fieldId,
            "operator": operator,
            "argument": argument
          };
        }
      
        return condition;
      }).toArray();
    
    return conditions;
  };
  
  var saveIndex = function(buttonData, completeFunction) {
    var indexId = buttonData.attr('data-index-id');
    var indexRev = buttonData.attr('data-index-rev');
    var url = "indexes/" + indexId + "?rev=" + indexRev;
    var doctype = buttonData.attr('data-index-doctype');
    
    var obj = {
      "_id": indexId,
      "category": "index",
      "doctype": doctype,
      "show_deleted": buttonData.attr('data-index-show_deleted') === "true",
      "fields": JSON.parse(buttonData.attr('data-index-fields')),
      "fields_label": JSON.parse(buttonData.attr('data-index-fields_label')),
      "name": buttonData.attr('data-index-name'),
      "conditions": getIndexConditions(doctype, $('#index-conditions-listing tbody tr'))
    };
    
    if (buttonData.attr('data-index-replace_function')) {
      obj.replace_function = buttonData.attr('data-index-replace_function');
    }
  
    shimi.form.send(url, obj, 'PUT', completeFunction, this);
  
    return false;  
  };
  
  var deleteIndex = 
    function(indexId, indexRev, completeMessage, completeFunction) {
      var url = "indexes/" + indexId + "?rev=" + indexRev;
      var title;
      var body;
      
      $.ajax(
        {
          type: "DELETE",
          url: url,
          dataType: "json",
          contentType: "application/json",
          complete: function(req, status) {
            if (req.status === 204) {
              title = "Success";
              body = completeMessage;
          
              completeFunction();
          
              shimi.flash(title, body).highlight();
            } else if (req.status === 409) {
              body = JSON.parse(req.responseText);
              title = req.statusText;
            
              shimi.flash(title, body.message).error();
            } else if (req.status === 404) {
              body = "Index appears to have been deleted already.";
              title = req.statusText;
              
              shimi.flash(title, body).error();
            }
          }
        });
  
      return false;  
    };
  
  mod.init = function(target) {
    var indexId = $(target).attr('data-index-id');
    var url = "indexes/" + indexId;
    var htmlTarget = $('#index-conditions');
    
    $.get(url, function(indexData) {
            htmlTarget.html(indexData);
            tableBody().sortable();
            shimi.piui.get();
          });
    
    return false;
  };
  
  mod.save = function() {
    var bData = editingData();
    
    if (bData.length !== 0) {
      var completeFunction = function() {
        mod.init(bData);
        shimi.flash("Success", "Your index has been saved.").highlight();
      };
      
      saveIndex(bData, completeFunction);
    } else {
      shimi.flash("Info", "No index has been chosen to save.").highlight();
    }
  };
  
  mod.replace = function() {
     var bData = editingData();
     
     if (bData.length !== 0) {
       shimi.initReplaceDialog().dialog("open");
     } else {
       shimi.flash("Info", "You must choose an index first.").highlight();
     }
     
     return mod;
   };
  
  mod.addCond = function() {
    var bData = editingData();
                           
    if (bData.length !== 0) {
      shimi.initIndexBuilderDialog(
        bData.attr('data-index-doctype')).dialog("open");
    } else {
      shimi.flash("Info", "You must choose an index first.").highlight();
    }
    
    return mod;                    
  };
  
  mod.remCond = function(target) {
    $(target).closest('tr').remove();
    return mod;
  };
  
  mod.newCond = function() {
    shimi.initIndexNewDialog().dialog("open");
    return mod;
  };
  
  mod.del = function() {
    var bData = editingData();

    if (bData.length !== 0) {
      var indexId = bData.attr('data-index-id');
      var indexRev = bData.attr('data-index-rev');
      var completeMessage = "Your index has been deleted.";
      var completeFunction = function() {
        $('#index-conditions').empty();
        shimi.iiui.init();
      };
      
      if (window.confirm("Are you sure?")) {
        deleteIndex(indexId, indexRev, completeMessage, completeFunction);
      }
    } else {
      shimi.flash("Info", "No index has been chosen to delete.").highlight();
    }
    
    return mod;
  };
  
  return mod;
})();