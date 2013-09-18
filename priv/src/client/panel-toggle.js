// # Panel Toggler
//
// Interface elements called panels can be visible or hidden.

// Given an element that points to a panel id with a `data-panel`
// attribute, toggle the panel's visibility.
var panelToggler = function (target)
{
  'use strict';

  var panel;

  if ($(target).attr('data-panel'))
  {
    panel = $('#' + $(target).attr('data-panel'));
  }
  else
  {
    panel = $(target).closest('.panel');
  }

  if (panel.css('display') === 'none')
  {
    panel.css('display', 'table-cell');
  }
  else
  {
    panel.css('display', 'none');
  }

  return target;
};

exports(panelToggler);
