function getData(value, elem) {
  var dataElem = elem.attr('data-group-id');
  return $('#' + dataElem).attr('data-' + value);
}

function populateFields(path) {
  path.field = false;
  
  $.get(path.toString(), function(fields) {
    var fieldContainer = $("#fields-" + path.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
    $('.link-button').button();
  });
}

function populateFieldsets(url) {
  $.get(url.toString(), function(fieldsets) {
    var fieldsetContainer = $("#fieldsets-" + url.doctype);
    
    fieldsetContainer.empty();
    fieldsetContainer.accordion("destroy");
    fieldsetContainer.html(fieldsets);
    $('.link-button').button();
    
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
    
    $("#doctype-tabs-headings").empty();
    $("#doctype-tabs-headings + .ui-tabs-panel").remove();
    $("#doctype-tabs").tabs("destroy");
    $("#doctype-tabs-headings").html(doctypes);
    $('.link-button').button();
    
    var loadFun = function(event, ui) {
      var source = $(ui.panel).children('div[data-fieldset-doctype]');
      var fieldsetsPath = path(source, "fieldset");
      populateFieldsets(fieldsetsPath);
    };
    
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
  $('.link-button').button();
});