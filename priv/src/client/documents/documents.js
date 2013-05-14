// Shared document editing stuff plus initialization.
shimi.documents = (function () {
  var mod = {};

  var indexForm = function () {
     $('#index-filter-form select').change(function () {
      shimi.indexui.get();
    });

    return mod;
  };
  var loadHash = function (urlHash) {
    if (urlHash) {
      shimi.viewui.get(urlHash);
    }

    return mod;
  };
  var allDocContainer = function () {
    return $("#all-document-container");
  };
  var versionKey = function () {
    return mod.identifier() + "_version";
  };
  var infoKey = function () {
    return mod.identifier() + "_info";
  };
  var labelsKey = function () {
    return mod.identifier() + "_labels";
  };
  var storeDoctype = function (doctype) {
    sessionStorage.setItem(infoKey(), doctype);
    shimi.dispatch.send('doctype-info-ready');

    return mod;
  };

  mod.getVersion = function () {
    return sessionStorage.getItem(versionKey());
  };

  mod.getCurrentVersion = function () {
    return shimi.store(allDocContainer()).d("version");
  };

  mod.isCurrentVersionStored = function () {
    return (mod.getVersion() && mod.getVersion() === mod.getCurrentVersion());
  };

  mod.setVersion = function () {
    sessionStorage.setItem(versionKey(), mod.getCurrentVersion());
    shimi.dispatch.send("version-set");

    return mod;
  };

  mod.clearSession = function () {
    sessionStorage.clear();
    shimi.dispatch.send("session-cleared");

    return mod;
  };

  mod.checkVersion = function () {
    if (mod.isCurrentVersionStored()) {
      shimi.dispatch.send("labels-ready");
    } else {
      shimi.dispatch.send("bad-session-state");
    }

    return mod;
  };

  mod.name = function () {
    return shimi.store($("#all-document-container")).d("doctype");
  };

  mod.project = function () {
    return shimi.store($("#container")).get("project-id");
  };

  mod.identifier = function () {
    return mod.project() + "_" + mod.name();
  };

  mod.info = function () {
    return JSON.parse(sessionStorage.getItem(infoKey()));
  };

  mod.loadDoctype = function () {
    $.getJSON("./", function (data) {
      storeDoctype(JSON.stringify(data));
    });

    return mod;
  };

  mod.makeLabels = function () {
    var info = mod.info();
    var labels = {};

    info.fieldsets.forEach(function (fieldset) {
      fieldset.fields.forEach(function (field) {
        labels[field._id] = [fieldset.label, field.label];
      });
    });

    sessionStorage.setItem(labelsKey(), JSON.stringify(labels));
    shimi.dispatch.send("labels-ready");

    return mod;
  };

  mod.init = function () {
    $('form').on('submit', function () {
      return false;
    });
    mod.checkVersion();
    shimi.setsui.updateSelection();
    shimi.indexui.iOpts().get();
    indexForm();
    shimi.editui.init();
    loadHash($(location)[0].hash.split("#")[1]);
  };

  return mod;
})();