var initIndexNewButton = function() {
  $('#new-index-button').button(
    {
      icons: {primary: "ui-icon-plus"}
    }).click(function() {
               initIndexNewDialog().dialog("open");
             });
  
  return false;
};

var initConditionRemoveButtons = function(tableBody) {
  tableBody.find('.remove-condition-button').button(
    {
      icons: {primary: "ui-icon-minus"}
    }).click(function(e) {
               $(e.target).closest('tr').remove();
             });
  
  return false;
};

var initIndexDeleteButton = function(button, buttonData) {
  button.button(
    {icons: {primary: "ui-icon-trash"}
    }).click(function (e) {
               var bData = buttonData();
               
               if (!bData.length < 1) {
                 var deleteButton = $(e.target);
                 var indexId = bData.attr('data-index-id');
                 var indexRev = bData.attr('data-index-rev');
                 var completeMessage = "Your index has been deleted.";
                 var completeFunction = function() {
                   $('#index-conditions').empty();
                   initIndexIndex();
                 };
                 
                 if (confirm("Are you sure?")) {
                   deleteIndex(indexId, indexRev, completeMessage, 
                               completeFunction);
                 }
               } else {
                 flashHighlight("Info", "No index has been chosen to delete.");
               }
             });
  
  return false;
};

var initIndexAddConditionButton = function(button, buttonData) {
  button.button({
                  icons: {primary: "ui-icon-plus"}
                }).click(function (e) {
                           var bData = buttonData();
                           
                           if (!bData.length < 1) {
                             initIndexBuilderDialog(
                               bData.attr('data-index-doctype')).dialog("open");
                           } else {
                             flashHighlight("Info", 
                                            "You must choose an index first.");
                           }
                         });

  return false;
};

var initReplaceButton = function(button, buttonData) {
  button.button({
                  icons: {primary: "ui-icon-shuffle"}
                }).click(function (e) {
                           var bData = buttonData();
                           
                           if (!bData.length < 1) {
                             initReplaceDialog().dialog("open");
                           } else {
                             flashHighlight("Info", 
                                            "You must choose an index first.");
                           }
                         });

  return false;
};
