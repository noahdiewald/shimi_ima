function initQueryChooseButton() {
  $('#choose-query-button').button({
    icons: {primary: "ui-icon-arrowreturnthick-1-s"}
  }).click(function() {
    $('#query-index-listing').slideToggle();
  });
  
  return false;
}

function initQueryNewButton() {
  $('#new-query-button').button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    initQueryNewDialog().dialog("open");
  });
  
  return false;
}

function initConditionRemoveButtons(tableBody) {
  tableBody.find('.remove-condition-button').button({
    icons: {primary: "ui-icon-minus"}
  }).click(function(e) {
    $(e.target).closest('tr').remove();
  });
  
  return false;
}

function initQueryDeleteButton(button, buttonData) {
  button.button(
    {icons: {primary: "ui-icon-trash"}
  }).click(function (e) {
    var bData = buttonData();
    
    if (!bData.length < 1) {
      var deleteButton = $(e.target);
      var queryId = bData.attr('data-query-id');
      var queryRev = bData.attr('data-query-rev');
      var completeMessage = "Your query has been deleted.";
      var completeFunction = function() {
        button.parent('div').parent('div').empty();
        initQueryIndex();
      };
      
      if (confirm("Are you sure?")) {
        deleteQuery(queryId, queryRev, completeMessage, completeFunction);
      }
    } else {
      flashHighlight("Info", "No query has been chosen to delete.");
    }
  });
  
  return false;
}

function initQueryAddConditionButton(button, buttonData) {
  button.button({
    icons: {primary: "ui-icon-plus"}
  }).click(function (e) {
    var bData = buttonData();
    
    if (!bData.length < 1) {
      initQueryBuilderDialog(bData.attr('data-query-doctype')).dialog("open");
    } else {
      flashHighlight("Info", "You must choose a query first.");
    }
  });

  return false;
}
