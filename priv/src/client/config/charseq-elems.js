// # Working with elements of a charseq manipulation HTML form
//
// *Implicit depends:* DOM, JQuery
//
// A charaseq is a collection of information used in definining properies
// of a script, including some phonological information and information
// used for collation of items written in the script.

// Variable Definitions

var form = require('../form.js');

// Exported functions

// Return object for working with charseq elements
var charseqElems = (function ()
{
  'use strict';

  var mod = {};

  mod.attrs = ['description', 'characters', 'name', 'sort_ignore', 'locale', 'tailoring', 'vowels', 'consonants', 'ietf_tag', 'iso639_tag', 'charseq', 'rev'];

  mod.get = function (values)
  {
    var cObj = {};

    cObj.attrs = mod.attrs;

    cObj.copyValues = function (source)
    {
      Object.keys(source).forEach(function (field)
      {
        cObj[field].val(source[field]);
      });
      return cObj;
    };

    cObj.getCharseqInputVals = function ()
    {
      var valObj = {
        'category': 'charseq',
        'description': cObj.description.val(),
        'characters': cObj.parse(cObj.characters.val()),
        'name': cObj.name.val(),
        'sort_ignore': cObj.parse(cObj.sort_ignore.val()),
        'locale': cObj.locale.val(),
        'tailoring': cObj.tailoring.val(),
        'vowels': cObj.parse(cObj.vowels.val()),
        'consonants': cObj.parse(cObj.consonants.val()),
        'ietf_tag': cObj.ietf_tag.val(),
        'iso639_tag': cObj.iso639_tag.val(),
        '_id': (cObj.charseq.val() || undefined),
        'rev': (cObj.rev.val() || undefined)
      };
      return valObj;
    };

    cObj.parse = function (val)
    {
      if (val && !val.isBlank())
      {
        return JSON.parse(val);
      }
      else
      {
        return [];
      }
    };

    cObj.clear = function ()
    {
      form.clear($('#charseq-dialog .input')).removeClass('ui-state-error');
      return cObj;
    };

    cObj.attrs.forEach(function (item)
    {
      cObj[item] = $('#charseq-' + item + '-input');
    });

    if (values)
    {
      cObj.copyValues(values);
    }

    return cObj;
  };

  return mod;
})();

exports(charseqElems);
