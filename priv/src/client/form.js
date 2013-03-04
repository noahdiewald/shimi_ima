shimi.form = (function () {
  var mod = {};

  mod.toggle = function (t) {
    var toggleElem;
    var target = $(t);

    if (target.attr('data-target')) {
      toggleElem = $('#' + target.attr('data-target'));
      toggleElem.toggle();
    }
    return mod;
  };

  mod.cancelDialog = function (t) {
    var target = $(t);
    var toggleElem;
    var elemId;

    if (target.attr('data-target')) {
      elemId = '#' + target.attr('data-target');
      toggleElem = $(elemId);
      toggleElem.hide();
      mod.clear(undefined, toggleElem.find("form"));
    }
    return mod;
  };

  mod.clear = function (inputFields, form) {
    if (inputFields === undefined) {
      inputFields = $(form).find("input, select, textarea");
    }
    inputFields.each(function (index, elem) {
      var inputField = $(elem);

      if (!inputField.attr('data-retain')) {
        if (inputField.is(':checked')) {
          inputField.attr('checked', false);
        }
        inputField.val('');
      }
    });
    return inputFields;
  };

  mod.send = function (ajaxUrl, obj, method, completeFun, callContext) {
    var dataObj;

    if (obj) {
      dataObj = JSON.stringify(obj);
    }

    $.ajax({
      type: method,
      url: ajaxUrl,
      dataType: "json",
      context: callContext,
      contentType: "application/json",
      processData: false,
      data: dataObj,
      complete: function (req, status) {
        if (req.status >= 200 && req.status < 300) {
          completeFun(this, req);
        } else if (req.status === 500) {
          shimi.flash("Unknown Server Error", "Please report that you received " + "this message").error();
        } else if (req.status >= 400) {
          var body = JSON.parse(req.responseText);
          var title = req.statusText;

          shimi.flash(title, body.fieldname + " " + body.message).error();
        }
      }
    });

    return true;
  };

  // Validation
  mod.updateTips = function (t, tips) {
    tips.text(t).addClass('ui-state-highlight');
    setTimeout(function () {
      tips.removeClass('ui-state-highlight', 1500);
    }, 500);

    return true;
  };

  mod.checkLength = function (o, n, min, max, tips) {
    if (o.val().length > max || o.val().length < min) {
      o.addClass('ui-state-error');
      mod.updateTips("Length of " + n + " must be between " + min + " and " + max + ".", tips);
      return false;
    } else {
      return true;
    }
  };

  mod.checkRegexp = function (o, regexp, n, tips) {
    if (!(regexp.test(o.val()))) {
      o.addClass('ui-state-error');
      mod.updateTips(n, tips);
      return false;
    } else {
      return true;
    }
  };

  // Date Picker
  mod.initDateFields = function () {
    $(".date").datepicker({
      dateFormat: "yy-mm-dd"
    });

    return true;
  };

  mod.fillOptionsFromUrl = function (url, selectElement, callback) {
    $.get(url, function (options) {
      selectElement.html(options);
      if (callback) {
        callback();
      }
    });

    return false;
  };

  return mod;
})();