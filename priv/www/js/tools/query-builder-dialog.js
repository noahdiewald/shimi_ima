function initQueryBuilderDialog(queryDoctype) {
  var builderOr = $("#builder-or-input");
  var builderNegate = $("#builder-negate-input");
  var builderOperator = $("#builder-operator-input");
  var builderArgument = $("#builder-argument-input");
  var builderFieldset = $("#builder-fieldset-input");
  var builderField = $("#builder-field-input");
  var notBlank = [builderOperator, builderFieldset, builderField];
  var fieldset_url = 'doctypes/' + queryDoctype + '/fieldsets';
  var condition_url = 'queries/condition';
  
  var appendCondition = function(builderRow) {
    tableBody = $('#query-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();
    initConditionRemoveButtons(tableBody);
    
    return false;
  };
    
  fillOptionsFromUrl(fieldset_url, builderFieldset);
  
  builderOr.change(function() {
    if (builderOr.is(':checked')) {
      $('#builder-conditions').hide();
    } else {
      $('#builder-conditions').show();
    }
  });
  
  var dialog = $("#query-builder-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        $('.input').removeClass('ui-state-error');
        
        // place holder for client side validation
        var checkResult = true;
        
        if (!builderOr.is(':checked')) {
          notBlank.forEach(function(item) {
            if (item.val().isBlank()) {
              item.addClass('ui-state-error');
              checkResult = false;
            } else {
              item.removeClass('ui-state-error');
            }
          });
        }
        
        if (checkResult) {
          if (builderOr.is(':checked')) {
            $.get(condition_url, {"is_or": true}, function(data) {appendCondition(data)});
          } else {
            $.get(condition_url, {
              "is_or": false,
              "negate": builderNegate.is(':checked'),
              "fieldset": builderFieldset.val(),
              "field": builderField.val(),
              "operator": builderOperator.val(),
              "argument": builderArgument.val()
            }, function(data) {appendCondition(data)});
          }
          
          $(this).dialog("close");
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      $('#builder-conditions').show();
      builderFieldset.unbind('change');
      builderField.unbind('change');
      builderOperator.unbind('change');
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  setQueryFieldsetEvents(queryDoctype, builderFieldset, builderField);
  setQueryFieldEvents(queryDoctype, builderFieldset, builderField);
  setQueryOperatorEvents(builderArgument, builderOperator, builderField);
  
  return dialog;
}
