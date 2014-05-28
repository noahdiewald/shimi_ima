// # Index listing.
//
// *Implicit depends:* DOM
//
// Displays a listing of user created indexes.

// Variable Definitions

var templates = require('templates');
var ajax = require('ajax');

// Exported functions

// Initialize the listing of user created indexes.
var init = function () {
  'use strict';

  var url = 'indexes';
  var target = document.getElementById('index-index-listing');
  var listing;

  ajax.get(url, function (req) {
    listing = templates['index-listing'](req.response);
    target.innerHTML = listing;
  });

  return true;
};

exports.init = init;
