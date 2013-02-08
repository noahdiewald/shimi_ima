$('#document-worksheets-form').live("keydown", function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("worksheet-form-submit");
    return false;
  }
  return true;
});
$('#document-sets-form').live("keydown", function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("sets-form-submit");
    return false;
  }
  return true;
});
$('#new-set-form').live("keydown", function (e) {
  if (e.which === 13) {
    shimi.dispatch.send("new-set-form-submit");
    return false;
  }
  return true;
});