// # Builder dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding conditions to user created indexes.

// TODO I would rather avoid having this as a JQuery plugin.

require('../jquery-ui-input-state.js');

// Variable Definitions

var ihelpers = require('./ihelpers.js');
var form = require('../form.js');
var evs = require('./ievents.js');

// Exported functions

// The dialog for adding a condition to an index.
var initIndexBuilderDialog = function (indexDoctype) {
  'use strict';

  var builderOr = $('#builder-or-input');
  var builderParen = $('#builder-paren-input');
  var builderNegate = $('#builder-negate-input');
  var builderOperator = $('#builder-operator-input').inputDisable();
  var builderArgument = $('#builder-argument-input').inputDisable();
  var builderFieldset = $('#builder-fieldset-input').inputDisable();
  var builderField = $('#builder-field-input').inputDisable();
  var notBlank = [builderOperator, builderFieldset, builderField];
  var fieldset_url = 'doctypes/' + indexDoctype + '/fieldsets';
  var condition_url = 'indexes/condition';

  $('.ui-helper-reset div').show();

  var appendCondition = function (builderRow) {
    var tableBody = $('#index-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();

    return false;
  };

  ihelpers.fOpts(fieldset_url, builderFieldset, function () {
    builderFieldset.inputEnable();
  });

  builderOr.change(function () {
    if (builderOr.is(':checked')) {
      $('#builder-conditions').hide();
      $('#builder-parens').hide();
    } else {
      $('#builder-conditions').show();
      $('#builder-parens').show();
    }
  });

  builderParen.change(function () {
    if (builderParen.val()) {
      $('#builder-or').hide();
      $('#builder-conditions').hide();
    } else {
      $('#builder-or').show();
      $('#builder-conditions').show();
    }
  });

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderField.inputDisable();
      builderArgument.inputDisable();

      return function () {
        builderField.inputEnable();
      };
    });
  };

  var fieldEvents = function () {
    evs.setIndexFieldEvents(indexDoctype, builderFieldset, builderField, function () {
      builderOperator.inputDisable();
      builderArgument.inputDisable();

      return function () {
        builderOperator.inputEnable();
      };
    });
  };

  var operatorEvents = function () {
    evs.setIndexOperatorEvents(builderArgument, builderOperator, builderField, function () {
      builderArgument.inputDisable();

      return function () {
        builderArgument.inputEnable();
      };
    });
  };

  var dialog = $('#index-builder-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Create': function () {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!builderOr.is(':checked') && !builderParen.val()) {
          notBlank.forEach(function (item) {
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
            $.get(condition_url, {
              'is_or': true
            }, function (data) {
              appendCondition(data);
            });
          } else if (builderParen.val()) {
            $.get(condition_url, {
              'is_or': false,
              'parens': builderParen.val(),
              'negate': false
            }, function (data) {
              appendCondition(data);
            });
          } else {
            $.get(condition_url, {
              'is_or': false,
              'parens': false,
              'negate': builderNegate.is(':checked'),
              'fieldset': builderFieldset.val(),
              'field': builderField.val(),
              'operator': builderOperator.val(),
              'argument': builderArgument.val()
            }, function (data) {
              appendCondition(data);
            });
          }

          $(this).dialog('close');
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      $('#builder-conditions').show();
      builderFieldset.unbind('change');
      builderField.unbind('change');
      builderOperator.unbind('change');
      form.clear($('.input')).removeClass('ui-state-error');
    }
  });

  fieldsetEvents();
  fieldEvents();
  operatorEvents();

  return dialog;
};

exports.initIndexBuilderDialog = initIndexBuilderDialog;
