shimi.ihelpers = (function () {
  'use strict';

  var mod = {};
  var s = shimi.sess();
  mod.evs = {};

  var disableOptions = function (options, disables) {
    options.children().show();

    disables.forEach(function (item) {
      options.children('option:contains(' + item + ')').hide();
    });

    return false;
  };

  var disableOperatorOptions = function (fieldDoc) {
    var options = $('#builder-operator-input');

    switch (fieldDoc.subcategory) {
    case 'select':
    case 'docselect':
    case 'text':
    case 'textarea':
      disableOptions(options, ['member', 'true']);
      break;
    case 'integer':
    case 'rational':
    case 'date':
      disableOptions(options, ['member', 'true', 'match']);
      break;
    case 'boolean':
    case 'openboolean':
      disableOptions(options, ['equal', 'greater', 'less', 'member', 'match']);
      break;
    case 'multiselect':
    case 'docmultiselect':
      disableOptions(options, ['equal', 'greater', 'less', 'true', 'match']);
      break;
    }

    return false;
  };

  mod.alterArg = function (argumentField, operatorField, fieldField, callback) {
    var fieldDoc = function () {
      return s.get(fieldField.val());
    };

    callback();

    try {
      // Destroy these if initialized already
      argumentField.removeAttr('disabled').datepicker('destroy');
      argumentField.removeAttr('disabled').autocomplete('destroy');
    } catch (err) {
      window.console.log(err.message);
    }

    var dateOrText = function (argumentField, fdoc) {
      if (fdoc.subcategory === 'date') {
        argumentField.removeAttr('disabled');
        argumentField.datepicker({
          dateFormat: 'yy-mm-dd'
        });
      } else {
        argumentField.removeAttr('disabled');
        argumentField.autocomplete({
          source: fdoc.allowed
        });
      }

      return mod;
    };

    var fdoc = fieldDoc();

    if (fdoc) {
      switch (operatorField.val()) {
      case 'true':
      case 'isDefined':
      case 'blank':
        argumentField.attr('disabled', 'disabled').val('');
        break;
      case 'equal':
      case 'member':
      case 'greater':
      case 'less':
      case 'hasExactly':
      case 'hasGreater':
      case 'hasLess':
        dateOrText(argumentField, fdoc);
        break;
      }

    }

    return mod;
  };

  mod.alterOpts = function (fieldDoc, fieldId, callback) {
    disableOperatorOptions(fieldDoc);
    callback();

    return mod;
  };

  mod.fOpts = function (url, selectElement, callback) {
    $.get(url, function (options) {
      selectElement.html(options);
      if (callback) {
        callback();
      }
    });

    return mod;
  };

  mod.getFieldDoc = function (fieldId, fieldsetId, doctypeId, callback) {
    var fieldDoc = s.get(fieldId);
    var url = 'doctypes/' + doctypeId + '/fieldsets/' + fieldsetId + '/fields/' + fieldId + '?format=json';

    if (fieldDoc) {
      if (callback) {
        callback(fieldDoc);
      }
      return fieldDoc;
    } else {
      $.ajax({
        url: url,
        async: false,
        dataType: 'json',
        success: function (data) {
          s.put(data);
          if (callback) {
            callback(s.get(fieldId));
          }
        }
      });

      return s.get(fieldId);
    }
  };

  mod.evs.setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
    indexDoctype.change(function () {
      var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      mod.fOpts(url, indexFieldset, callback2);
    });

    return false;
  };

  mod.evs.setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexFieldset.change(function () {
      var callback2;

      if (typeof indexDoctype !== 'string') {
        indexDoctype = indexDoctype.val();
      }

      if (indexFieldset.val()) {
        var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.val() + '/fields?as=options';

        if (callback) {
          callback2 = callback();
        }

        mod.fOpts(url, indexField, callback2);
      }
    });

    return mod;
  };

  mod.evs.setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexField.change(function () {
      var fieldId = indexField.val();
      var fieldsetId = indexFieldset.val();
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      if (!(fieldId.isBlank())) {
        mod.getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
          shimi.ihelpers.alterOpts(data, fieldId, callback2);
        });
      }
    });

    return mod;
  };

  mod.evs.setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
    operatorField.change(function () {
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      mod.alterArg(argumentField, operatorField, fieldField, callback2);
    });

    return mod;
  };

  return mod;
})();