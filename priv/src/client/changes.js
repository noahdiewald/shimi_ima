$(document).on('change', '#document-search-exclude', function (e)
{
  'use strict';

  shimi.searchui.toggleExclusion();
  return true;
});

$(document).on('change', '#document-search-invert', function (e)
{
  'use strict';

  shimi.searchui.toggleInversion();
  return true;
});
