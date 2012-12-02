shimi.upgradeButton = function (target) {
  $.post("config/upgrade");
  window.alert("Upgrade In Progress");
};

shimi.initTabs = function () {
  shimi.doctypeTab.init();
  $("#main-tabs").tabs();
  shimi.charseqTab.init();

  return true;
};