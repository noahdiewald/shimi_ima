function initTabs() {
  $("#main-tabs").tabs();
  //populateDoctypeTabs();
  
  return false;
}

function initHelpText() {
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
  
  return false;
}

$(function () {
  initTabs(); 
  initHelpText();
  $('#query-builder-dialog').hide();
  
  //getIndex();
  //initEdit();
  
  //$('#index-filter-form input').keyup(function() {
  //  getIndex();
  //});
  
  //$('#index-filter-form select').change(function() {
  //  getIndex();
  //});
  
  $('#all-query-container').accordion({ 
    collapsible: true,
    autoHeight: false
  });
});
