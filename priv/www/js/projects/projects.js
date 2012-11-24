
$(function () {
  shimi.pui().init();
  
  $("#create-project").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    shimi.pui().addProjectDialog().dialog("open");
  });
  
});