// # Index listing.
//
// *Implicit depends:* DOM, JQuery, Hogan, templates
//
// Displays a listing of user created indexes.

// Exported functions

// Initialize the listing of user created indexes.
var init = function ()
{
  var url = 'indexes';
  var target = $('#index-index-listing');
  var listing;

  $.getJSON(url, function (data)
  {
    listing = templates['index-listing'].render(data);
    target.html(listing);
  });

  return true;
};

exports(init);
