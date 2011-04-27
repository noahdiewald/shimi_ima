function populateFields(url) {
  $.get(url.toString(), function(fields) {
    var fieldContainer = $("#fields-" + url.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
  });
}

function populateFieldsets(url) {
  var fieldDoctype = $("#field-doctype-input");
  var fieldFieldset = $("#field-fieldset-input");
            
  $.get(url.toString(), function(fieldsets) {
    var fieldsetContainer = $("#fieldsets-" + url.doctype);
    
    fieldsetContainer.empty();
    fieldsetContainer.accordion("destroy");
    fieldsetContainer.html(fieldsets);
    
    fieldsetContainer.accordion({
      autoHeight: false,
      collapsible: true,
      active: false
    });
  });
}

function populateDoctypeTabs() {
  var url = "config/doctypes";
  
  $.get(url, function(doctypes) {
    var fieldsetDoctype = $("#fieldset-doctype-input");
    var loadFun = function(event, ui) {
      var fieldsetsUrl = buildUrl($(ui.panel).children('div[data-fieldset-doctype]'), "fieldset");
      populateFieldsets(fieldsetsUrl);
    };
    
    $("#doctype-tabs-headings").empty();
    $("#doctype-tabs-headings + .ui-tabs-panel").remove();
    $("#doctype-tabs").tabs("destroy");
    $("#doctype-tabs-headings").html(doctypes);
    $("#doctype-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
  });
}

function initTabs() {
  $("#doctype-tabs").tabs();
  populateDoctypeTabs();
  $("#main-tabs").tabs();
  $("#charseq-tabs").tabs();
  
  return true;
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
  
  return true;
}

$(function () {
  $('body').click(function(e) {clickDispatch(e)});
  initTabs(); 
  initHelpText();
});