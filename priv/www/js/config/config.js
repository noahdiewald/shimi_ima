
function initDoctypeAddButton() {
  $("#doctype-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    initDoctypeAddDialog().dialog("open");
  });
  
  return false;
}

function encodeDefaults(subcategory, defaults) {
  switch (subcategory) {
    case "docmultiselect":
    case "multiselect":
      return defaults.join(", ");
    default:
      return defaults;
  }
}

function decodeDefaults(subcategory, defaults) {
  switch (subcategory) {
    case "docmultiselect":
    case "multiselect":
      return defaults.split(",").map(function (item) {return item.trim()});
    default:
      return defaults;
  }
}

function initCharsecAddDialog() {
  $("#charseq-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add": function() {},
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  return true;
}

function initCharsecAddButton() {
  $("#charseq-add-button").button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    $("#charseq-add-dialog").dialog("open");
  });

  return true;
}

function populateFields(doctypeId, fieldsetId) {
  var url = "config/doctypes/" + doctypeId + 
            "/fieldsets/" + fieldsetId + 
            "/fields";
        
  $.get(url, function(fields) {
    var fieldContainer = $("#fields-" + fieldsetId);
    fieldContainer.empty();
    fieldContainer.html(fields);
  });
}

function populateFieldsets(doctypeId) {
  var fieldDoctype = $("#field-doctype-input");
  var fieldFieldset = $("#field-fieldset-input");
  var url = "config/doctypes/" + doctypeId + 
            "/fieldsets";
            
  $.get(url, function(fieldsets) {
    var fieldsetContainer = $("#fieldsets-" + doctypeId);
    
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
      doctype = $(ui.panel).children()[0].id;
      populateFieldsets(doctype);
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
  $('#doctype-dialog').hide();
  $("#fieldset-dialog").hide();
  $("#field-dialog").hide();
  initDoctypeAddButton();
  initCharsecAddDialog();
  initCharsecAddButton();
});