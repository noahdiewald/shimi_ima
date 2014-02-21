/*
 Simple plugin for manipulating input.
*/

(function ($) {
  'use strict';

  $.fn.inputDisable = function () {
    this.val('');
    this.attr('disabled', 'disabled');
    this.addClass('ui-state-disabled');
    return this;
  };

  $.fn.inputEnable = function () {
    this.removeAttr('disabled');
    this.removeClass('ui-state-disabled');
    return this;
  };

})(jQuery);
