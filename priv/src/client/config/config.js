shimi.upgradeButton = function (target)
{
  'use strict';

  $.post('config/upgrade');
  window.alert('Upgrade In Progress');
};

shimi.initTabs = function ()
{
  'use strict';

  shimi.doctypeTab.init();
  $('#main-tabs').tabs();
  shimi.charseqTab.init();

  return true;
};
