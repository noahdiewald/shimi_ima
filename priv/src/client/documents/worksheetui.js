shimi.worksheetui = (function () {
  var mod = {};
  var sets = shimi.sets;

  var worksheetsSet = function () {
    return $("#document-worksheets-set-input");
  };
  var worksheetsArea = function () {
    return $("#worksheet-area");
  };
  var worksheetName = function () {
    return shimi.documents.identifier() + "_worksheet-template";
  };

  mod.toggleFieldset = function (fsid) {
    $('.handle-column.field.' + fsid).toggle();

    return mod;
  };

  mod.showField = function (fid) {
    $('.field-column.' + fid).show();

    return mod;
  };

  mod.hideField = function (fid) {
    $('.field-column.' + fid).hide();

    return mod;
  };

  mod.buildTemplate = function () {
    var doctypeInfo = shimi.documents.info();
    var metaTemp = "{{=<% %>=}}\n" + templates['worksheet'].render(doctypeInfo);
    shimi.globals[worksheetName()] = Hogan.compile(metaTemp);

    return mod;
  };

  mod.fillWorksheet = function () {
    var setName = worksheetsSet().val();
    var url = "ws?set=";

    if (!setName.isBlank()) {
      var thisSet = sets.getSet(setName)[1];
      var setIds = thisSet.map(function (x) {
        return x[1];
      });
      url = url + JSON.stringify(setIds);

      $.getJSON(url, function (data) {
        var ws = shimi.globals[worksheetName()].render(data);
        worksheetsArea().html(ws);
      });
    }

    return mod;
  };

  return mod;
})();