// hide common options for path

function cpath(source, category) {
  return path(source, category, "config");
}

// Populate the listing of fields

function populateFields(path) {
  path.field = false;
  
  $.get(path.toString(), function(fields) {
    var fieldContainer = $("#fields-" + path.fieldset);
    fieldContainer.empty();
    fieldContainer.html(fields);
    $('.link-button').button();
  });
}

// Populate the listing of fieldsets

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

// populate the tabs listing the doctypes

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
      var fieldsetsPath = path(source, "fieldset", "config");
      populateFieldsets(fieldsetsPath);
    };
    
    $("#doctype-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
  });
}

// populate the tabs listing the charseqs

function populateCharseqTabs() {
  var url = "config/charseqs";
  
  $.get(url, function(charseqs) {
    $("#charseq-tabs-headings").empty();
    $("#charseq-tabs-headings + .ui-tabs-panel").remove();
    $("#charseq-tabs").tabs("destroy");
    $("#charseq-tabs-headings").html(charseqs);
    
    var loadFun = function(event, ui) {
      $('.link-button').button();
    };
    
    $("#charseq-tabs").tabs({load: function(e, ui) {loadFun(e, ui)}});
  });
}

// Initialize the tabs on the config page

function initTabs() {
  $("#doctype-tabs").tabs();
  populateDoctypeTabs();
  $("#main-tabs").tabs();
  $("#charseq-tabs").tabs();
  populateCharseqTabs();
  
  return true;
}

// Hide the help text and set toggle on click events
// TODO use the click dispatcher

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

// Code to be run on page load

$(function () {
  initTabs(); 
  initHelpText();
  $('.link-button').button();
  $('.simple-tabs').tabs();
});