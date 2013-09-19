// # Charseq tab initialization
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var charseqDialog = require('./charseq-dialog.js').charseqDialog;
var charseqElems = require('./charseq-elems.js').charseqElems;
var store = require('../store.js').store;
var form = require('../form.js');

// Exported functions

// Object containing initialization and other functions.
var charseqTab = (function ()
{
  'use strict';

  var mod = {};

  mod.add = function ()
  {
    charseqDialog().dialog('open');
    return mod;
  };

  mod.edit = function (target)
  {
    var oldobj = {};
    var attrs = charseqElems.attrs;

    attrs.forEach(function (item)
    {
      oldobj[item] = store(target).get64('charseq-' + item);
    });
    charseqDialog(oldobj).dialog('open');

    return mod;
  };

  mod.del = function (target)
  {
    var s = store(target);
    var id = s.get('charseq-charseq');
    var rev = s.get('charseq-rev');
    var url = 'config/charseqs/' + id + '?rev=' + rev;
    var complete = function ()
    {
      mod.init();
    };

    if (window.confirm('Are you sure? This is permanent.'))
    {
      form.send(url,
      {}, 'DELETE', complete, this);
    }

    return mod;
  };

  mod.init = function ()
  {
    var tabs = $('#charseq-tabs');
    var heads = $('#charseq-tabs-headings');
    var url = 'config/charseqs';

    tabs.tabs();

    $.get(url, function (charseqs)
    {
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

exports.charseqTab = charseqTab;
