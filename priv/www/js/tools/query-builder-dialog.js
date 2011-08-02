function initQueryBuilderDialog(queryDoctype) {
  var builderOr = $("#builder-or-input");
  var builderNegate = $("#builder-negate-input");
  var builderOperator = $("#builder-operator-input").inputDisable();
  var builderArgument = $("#builder-argument-input").inputDisable();
  var builderFieldset = $("#builder-fieldset-input").inputDisable();
  var builderField = $("#builder-field-input").inputDisable();
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
    
  fillOptionsFromUrl(fieldset_url, builderFieldset, function () {builderFieldset.inputEnable()});
  
  builderOr.change(function() {
    if (builderOr.is(':checked')) {
      $('#builder-conditions').hide();
    } else {
      $('#builder-conditions').show();
    }
  });
  
  var fieldsetEvents = function () {
    setQueryFieldsetEvents(queryDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderField.inputDisable();
      builderArgument.inputDisable();
      
      return function () {
        builderField.inputEnable();
      };
    });
  };
  
  var fieldEvents = function () {
    setQueryFieldEvents(queryDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderArgument.inputDisable();
      
      return function () {
        builderOperator.inputEnable();
      };
    });
  };
  
  var operatorEvents = function () {
    setQueryOperatorEvents(builderArgument, builderOperator, builderField, function () {
      builderArgument.inputDisable();
      
      return function () {
        builderArgument.inputEnable();
      };
    });
  };
  
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
  
  fieldsetEvents();
  fieldEvents();
  operatorEvents();
  
  return dialog;
}
