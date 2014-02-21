// # Index listing.
//
// *Implicit depends:* DOM, JQuery
//
// Displays a listing of user created indexes.

// Variable Definitions

var templates = require('templates.js');
var ajax = require('../ajax.js');

// Exported functions

// Initialize the listing of user created indexes.
var init = function () {
  'use strict';

  var url = 'indexes';
  var target = $('#index-index-listing');
  var listing;

  ajax.get(url, function (req) {
    listing = templates['index-listing'](req.response);
    target.html(listing);
  });

  return true;
};

exports.init = init;
