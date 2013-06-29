shimi.charseqTab = (function () {
  'use strict';

  var mod = {};

  mod.add = function () {
    shimi.charseqDialog().dialog('open');
    return mod;
  };

  mod.edit = function (target) {
    var oldobj = {};
    var attrs = shimi.charseqElems.attrs;

    attrs.forEach(function (item) {
      oldobj[item] = shimi.store(target).get64('charseq-' + item);
    });
    shimi.charseqDialog(oldobj).dialog('open');

    return mod;
  };

  mod.del = function (target) {
    var s = shimi.store(target);
    var id = s.get('charseq-charseq');
    var rev = s.get('charseq-rev');
    var url = 'config/charseqs/' + id + '?rev=' + rev;
    var complete = function () {
      mod.init();
    };

    if (window.confirm('Are you sure? This is permanent.')) {
      shimi.form.send(url, {}, 'DELETE', complete, this);
    }

    return mod;
  };

  mod.init = function () {
    var tabs = $('#charseq-tabs');
    var heads = $('#charseq-tabs-headings');
    var url = 'config/charseqs';

    tabs.tabs();

    $.get(url, function (charseqs) {
      heads.empty();
      //$('#charseq-tabs-headings + .ui-tabs-panel').remove();
      heads.find('.ui-tabs-panel').remove();
      tabs.tabs('destroy');
      heads.html(charseqs);

      tabs.tabs();
    });

    return mod;
  };

  return mod;
})();