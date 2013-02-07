shimi.worksheetui = (function (args) {
  var mod = {};
  var getDoctype = function () {
    return shimi.store($("#all-document-container")).d("doctype");
  };
  var getProject = function () {
    return shimi.store($("#container")).get("project-id");
  };
  var getIdentifier = function () {
    return getProject() + "_" + getDoctype();
  };
  var getDoctypeInfo =  function () {
    return JSON.parse(sessionStorage.getItem(getIdentifier + "_info"));
  };

  mod.buildTemplate = function () {
    var doctypeInfo = getDoctypeInfo();
    var metaTemp = "<!-- {{=<% %>=}} -->\n" + templates['worksheet'].render(doctypeInfo);
    shimi.globals["worksheet-template"] = hogan.compile(metaTemp);
    
    return mod;
  };
  
  mod.fillWorksheet = function () {
    return mod;
  };
  
  return mod;
})();
