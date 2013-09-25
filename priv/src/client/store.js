// # Data Attribute Storage and Retrieval Helpers
//
// *Implicit depends:* DOM
//
// It is likely that this mechanism will be replaced with a superior
// mechanism for storing data on the client about documents.

// ## Variables

var utils = require('./utils.js');

// ## Internal functions

// Identity function for tail recursion
var identity = function (x)
{
  'use strict';

  return x;
};

// Part of the tail call optimization code
Function.prototype.r = function ()
{
  'use strict';

  return [this, arguments];
};

// Tail call optimization taken from Spencer Tipping's Javascript in Ten
// Minutes.
//
// For more information see:
// <https://github.com/spencertipping/js-in-ten-minutes>
Function.prototype.t = function ()
{
  'use strict';

  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape)
  {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
};

// Camel case a string
var cc = function (str)
{
  'use strict';

  return str.replace(/-./, function (substr)
  {
    return substr.toUpperCase()[1];
  });
};

// ## External functions

// Takes a JQuery element and returns an object with helper methods for
// getting and putting custom data attribute values.
var store = function (elem)
{
  'use strict';

  var mod = {};

  // TODO: Remove this when fieldsets.js has JQuery dependency removed
  if (elem.dataset === undefined)
  {
    elem = elem[0];
  }

  // This funtion takes a key that corresponds to the name of the data
  // attribute without the `data-` prefix. The element is expected to have
  // an attribute data-group-id with a value that is the id of the
  // element actually holding the data.
  //
  // ### Examples
  //
  // Given the following HTML:
  //
  //     <div
  //       id='someid'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  // This HTML contains a level of indirection and demonstrates the use
  // of the `data-group-id`:
  //
  //     <div
  //       id='someid2'
  //       data-fieldset-fieldset='fsid'
  //       data-fieldset-doctype='did'></div>
  //
  //     <div
  //       id='someid'
  //       data-group-id='someid2'
  //       data-fieldset-fieldset='fsid'></div>
  //
  //     <div
  //      id='thisid'
  //      data-group-id='someid'></div>
  //
  // The `data-fieldset-doctype` may be retrieved like this:
  //
  //     var thisId = document.getElementById('thisid');
  //     store(thisId).get('fieldset-doctype') == 'did';
  //
  mod.get = function (key)
  {
    var keycc = cc(key);
    var prelim = elem.dataset[keycc];

    if (prelim)
    {
      return prelim;
    }

    var getValue1 = function (key, elem, id)
    {
      var gid = elem.dataset.groupId;
      var store = document.getElementById(gid);
      var val = store.dataset[key];
      var next = store.dataset.groupId;

      if (val === undefined && next !== undefined && gid !== next)
      {
        return getValue1.r(key, store, id);
      }

      return id.r(val);
    };

    return getValue1.t(keycc, elem, identity);
  };

  // Like 'get' but will decode base64 encoded values.
  mod.get64 = function (key)
  {
    var retval = mod.get(key);
    retval = utils.Base64.decode(retval.replace(/'/g, '')).replace(/(^'|'$)/g, '');
    return retval;
  };

  //  This function will set an attribute at the target with a name
  //  corresponding to key and a value of value.
  mod.put = function (key, value)
  {
    var keycc = cc(key);
    var dataElem = elem.dataset.groupId;
    document.getElementById(dataElem).dataset[keycc] = value;
  };

  //  Helper function for attributes that begin with `data-fieldset`.
  mod.fs = function (key)
  {
    return mod.get('fieldset-' + key);
  };

  //  Helper function for attributes that begin with `data-field`.
  mod.f = function (key)
  {
    return mod.get('field-' + key);
  };

  //  Helper function for attributes that begin with `data-document`.
  mod.d = function (key)
  {
    return mod.get('document-' + key);
  };

  return mod;
};

exports.store = store;
