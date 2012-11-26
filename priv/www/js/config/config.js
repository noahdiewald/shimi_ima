shimi.upgradeButton = function(target) {
  $.post("config/upgrade");
  window.alert("Upgrade In Progress");
};

shimi.initTabs = function() {
  shimi.doctypeTab().init();
  $("#main-tabs").tabs();
  shimi.charseqTab().init();
  
  return true;
};

// Hide the help text and set toggle on click events
// TODO use the click dispatcher

shimi.initHelpText = function() {
  $("#doctype-info").hide();
  $("#charseq-info").hide();

  $("#doctype-info-toggle").click(function() {
    $("#doctype-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#charseq-info-toggle").click(function() {
    $("#charseq-info").toggle("blind", {}, 500);
    return false;
  });
  
  return true;
};
