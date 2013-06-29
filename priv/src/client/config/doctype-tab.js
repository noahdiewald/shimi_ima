shimi.doctypeTab = (function () {
  'use strict';

  var mod = {};

  var cpath = function (source, category) {
    return shimi.path(source, category, 'config');
  };

  // Populate the listing of fields
  mod.initFields = function (path) {
    path.field = false;

    $.get(path.toString(), function (fields) {
      var fieldContainer = $('#fields-' + path.fieldset);
      fieldContainer.empty();
      fieldContainer.html(fields);
    });

    return mod;
  };

  // Populate the listing of fieldsets
  mod.initFieldsets = function (url) {
    $.get(url.toString(), function (fieldsets) {
      var fieldsetContainer = $('#fieldsets-' + url.doctype);

      fieldsetContainer.empty();
      fieldsetContainer.accordion();
      fieldsetContainer.accordion('destroy');
      fieldsetContainer.html(fieldsets);

      fieldsetContainer.accordion({
        autoHeight: false,
        collapsible: true,
        active: false
      });
    });
  };

  // populate the tabs listing the doctypes
  mod.init = function () {
    var url = 'config/doctypes';

    $('#doctype-tabs').tabs();

    $.get(url, function (doctypes) {
      var fieldsetDoctype = $('#fieldset-doctype-input');

      $('#doctype-tabs-headings').empty();
      $('#doctype-tabs-headings + .ui-tabs-panel').remove();
      $('#doctype-tabs').tabs('destroy');
      $('#doctype-tabs-headings').html(doctypes);

      var loadFun = function (event, ui) {
        var source = $(ui.panel).children('div[data-fieldset-doctype]');
        var fieldsetsPath = shimi.path(source, 'fieldset', 'config');
        mod.initFieldsets(fieldsetsPath);
      };

      $('#doctype-tabs').tabs({
        load: function (e, ui) {
          loadFun(e, ui);
        }
      });
    });
  };

  // Button that opens a dialog for editing a field
  mod.editField = function (target) {
    var url = cpath(target, 'field');
    var oldobj = {};
    var attrs = shimi.fieldElems.attrs;
    var charseqUrl = 'config/charseqs?as=options';

    $.get(charseqUrl, function (charseqs) {
      $('#field-charseq-input').html(charseqs);
      attrs.forEach(function (item) {
        oldobj[item] = shimi.store(target).get('field-' + item);
      });
      shimi.fieldDialog(url, oldobj).dialog('open');
    });
  };

  // Button that opens a dialog for deleting a field
  mod.deleteField = function (target) {
    var answer = window.confirm('Are you sure? This is permanent.');

    if (answer) {
      var url = cpath(target, 'field');
      var complete = function () {
        url.field = false;
        url.rev = false;

        mod.initFields(url);
      };
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a field
  mod.addField = function (target) {
    var url = cpath(target, 'field');
    var charseqUrl = 'config/charseqs?as=options';

    $.get(charseqUrl, function (charseqs) {
      $('#field-charseq-input').html(charseqs);
      shimi.fieldDialog(url, {
        fieldset: url.fieldset,
        doctype: url.doctype
      }).dialog('open');
    });
  };

  // Button that opens a dialog for editing a fieldset
  mod.editFieldset = function (target) {
    var url = cpath(target, 'fieldset');
    var oldobj = {};
    var attrs = shimi.fieldsetElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get('fieldset-' + item);
    });

    shimi.fieldsetDialog(url, oldobj).dialog('open');
  };

  // Button that opens a dialog for deleting a fieldset
  mod.deleteFieldset = function (target) {
    var url = cpath(target, 'fieldset');

    var complete = function () {
      url.fieldset = false;
      url.rev = false;
      mod.initFieldsets(url);
    };

    if (window.confirm('Are you sure? This is permanent.')) {
      url.del(complete, this);
    }
  };

  // Button that opens a dialog for adding a fieldset
  mod.addFieldset = function (target) {
    var url = cpath(target, 'fieldset');
    shimi.fieldsetDialog(url, {
      doctype: url.doctype
    }).dialog('open');
  };

  mod.editDoctype = function (target) {
    var url = cpath(target, 'doctype');
    var oldobj = {};
    var attrs = shimi.doctypeElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get('doctype-' + item);
    });
    shimi.doctypeDialog(url, oldobj).dialog('open');
  };

  mod.touchDoctype = function (target) {
    var docid = shimi.store(target).get('doctype-doctype');
    $.post('config/doctypes/' + docid + '/touch');
    window.alert('Touch In Progress');
  };

  mod.deleteDoctype = function (target) {
    var url = cpath(target, 'doctype');
    var complete = function () {
      url.doctype = false;
      url.rev = false;
      mod.init();
    };

    if (window.confirm('Are you sure? This is permanent.')) {
      url.del(complete, this);
    }
  };

  mod.addDoctype = function (target) {
    var url = cpath(target, 'doctype');
    shimi.doctypeDialog(url, {}).dialog('open');
  };

  return mod;
})();