shimi.dispatch = (function () {
  var mod = {};

  mod.send = function (message) {
    switch (message) {
    case "bad-session-state":
      shimi.documents.clearSession();
      break;
    case "doctype-info-ready":
      shimi.documents.makeLabels();
      break;
    case "labels-ready":
      shimi.searchui.loadSearchVals();
      shimi.worksheetui.buildTemplate();
      break;
    case "new-set-form-submit":
      shimi.setsui.saveSelected();
      break;
    case "sets-changed":
      shimi.setsui.updateSelection();
      break;
    case "sets-form-submit":
      shimi.setsui.performOp();
      break;
    case "session-cleared":
      shimi.documents.setVersion();
      shimi.documents.loadDoctype();
      break;
    case "worksheet-form-submit":
      shimi.worksheetui.fillWorksheet();
      break;
    }

    return false;
  };

  return mod;
})();