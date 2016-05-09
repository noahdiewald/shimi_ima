// # Builder dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding conditions to user created indexes.

// ## Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var form = require('form');
var ajax = require('ajax');
//var evs = require('index_tool/ievents');
var templates = require('templates');

var evs = ihelpers.evs();

// ## Exported functions

// The dialog for adding a condition to an index.
var initIndexBuilderDialog = function (indexDoctype) {
  'use strict';

  var builderOrInput = document.getElementById('builder-or-input');
  var builderParenInput = document.getElementById('builder-paren-input');
  var builderNegateInput = document.getElementById('builder-negate-input');
  var builderOperatorInput = document.getElementById('builder-operator-input');
  var builderArgumentInput = document.getElementById('builder-argument-input');
  var builderFieldsetInput = document.getElementById('builder-fieldset-input');
  var builderFieldInput = document.getElementById('builder-field-input');
  var builderConditions = document.getElementById('builder-conditions');
  var builderParens = document.getElementById('builder-parens');
  var builderOr = document.getElementById('builder-or');
  var dialogElem = document.getElementById('index-builder-dialog');
  var tableBody = document.getElementById('index-conditions-listing').getElementsByTagName('tbody')[0];
  var notBlank = [builderOperatorInput, builderFieldsetInput, builderFieldInput];

  builderOperatorInput.setAttribute('disable', 'disable');
  builderArgumentInput.setAttribute('disable', 'disable');
  builderFieldsetInput.setAttribute('disable', 'disable');
  builderFieldInput.setAttribute('disable', 'disable');
  form.show(document.querySelector('.ui-helper-reset div'));

  var appendCondition = function (builderRow) {
    tableBody.insertAdjacentHTML('beforeend', builderRow);
    // TODO: allow for arranging rows some other way.
    $(tableBody).sortable();

    return false;
  };

  ihelpers.fsOpts(indexDoctype, builderFieldsetInput, function () {
    builderFieldsetInput.removeAttribute('disable');
  });

  builderOrInput.onchange = function () {
    if (builderOrInput.checked) {
      form.hide(builderConditions);
      form.hide(builderParens);
    } else {
      form.show(builderConditions);
      form.show(builderParens);
    }
  };

  builderParenInput.onchange = function () {
    if (builderParenInput.value) {
      form.hide(builderConditions);
      form.hide(builderOr);
    } else {
      form.show(builderConditions);
      form.show(builderOr);
    }
  };

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, builderFieldsetInput, builderFieldInput, function () {
      builderOperatorInput.setAttribute('disable', 'disable');
      builderFieldInput.setAttribute('disable', 'disable');
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderFieldInput.removeAttribute('disable');
      };
    });
  };

  var fieldEvents = function () {
    evs.setIndexFieldEvents(indexDoctype, builderFieldsetInput, builderFieldInput, function () {
      builderOperatorInput.setAttribute('disable', 'disable');
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderOperatorInput.removeAttribute('disable');
      };
    });
  };

  var operatorEvents = function () {
    evs.setIndexOperatorEvents(builderArgumentInput, builderOperatorInput, builderFieldInput, function () {
      builderArgumentInput.setAttribute('disable', 'disable');

      return function () {
        builderArgumentInput.removeAttribute('disable');
      };
    });
  };

  var getLabel = function (elem) {
    return elem.querySelector('[value="' + elem.value + '"]').innerHTML.trim();
  };

  var dialog = $(dialogElem).dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Create': function () {
        Array.prototype.forEach.call(document.querySelectorAll('.input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;
        var cond = {};

        if (!builderOrInput.checked && !builderParenInput.value) {
          notBlank.forEach(function (item) {
            if (item.value.isBlank()) {
              item.classList.add('ui-state-error');
              checkResult = false;
            } else {
              item.classList.remove('ui-state-error');
            }
          });
        }

        if (checkResult) {
          if (builderOrInput.checked) {
            cond['is_or'] = true;

            appendCondition(templates['index-condition'](cond));
          } else if (builderParenInput.value) {
            cond['is_or'] = false;
            cond['paren_' + builderParenInput.value] = true;

            appendCondition(templates['index-condition'](cond));
          } else {
            cond = {
              is_or: false,
              negate: builderNegateInput.checked,
              fieldset: builderFieldsetInput.value,
              fieldset_label: getLabel(builderFieldsetInput),
              field: builderFieldInput.value,
              field_label: getLabel(builderFieldInput),
              operator: builderOperatorInput.value,
              argument: builderArgumentInput.value
            };

            appendCondition(templates['index-condition'](cond));
          }

          $(this).dialog('close');
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      form.show(builderConditions);
      builderFieldsetInput.onchange = undefined;
      builderFieldInput.onchange = undefined;
      builderOperatorInput.onchange = undefined;
      form.clear(document.getElementsByClassName('input'));
      Array.prototype.forEach.call(document.getElementsByClassName('input'), function (item) {
        item.classList.remove('ui-state-error');
      });
    }
  });

  fieldsetEvents();
  fieldEvents();
  operatorEvents();

  return dialog;
};

exports.initIndexBuilderDialog = initIndexBuilderDialog;
