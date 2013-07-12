shimi.sess = function ()
{
  'use strict';

  var mod = {};

  mod.put = function (doc)
  {
    if (!window.sessionStorage[doc._id])
    {
      window.sessionStorage[doc._id] = JSON.stringify(doc);
    }

    return doc._id;
  };

  mod.get = function (docId)
  {
    var doc = window.sessionStorage[docId];

    if (doc)
    {
      return JSON.parse(doc);
    }
    else
    {
      return null;
    }
  };

  return mod;
};
