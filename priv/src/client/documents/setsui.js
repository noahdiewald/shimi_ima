shimi.setsui = (function () {
  var mod = {};
  var sets = shimi.sets;
  var utils = shimi.utils();
  var setA = function () {
    return $("#document-set-a-input");
  };
  var setB = function () {
    return $("#document-set-b-input");
  };
  var worksheetsSet = function () {
    return $("#document-worksheets-set-input");
  };
  var op = function () {
    return $("#document-set-operation-input");
  };
  var setListing = function () {
    return $("#set-listing");
  };

  var selectedToArray = function (target) {
    var retval = [];

    switch (target) {
    case "search":
      retval = selectedSaveResultsToArray();
      break;
    case "sets":
      retval = selectedElementsToArray();
      break;
    }

    return retval;
  };

  var selectedElementsToArray = function () {
    var retval;
    var selected = $("input.set-element-selection:checked");

    retval = $.map(selected, function (elem) {
      var anchor = $(elem).parent("td").next("td").find("a").first();
      var id = anchor.first().attr("href").replace(/^#/, "");
      var context = anchor.html().trim();
      return [[context, id]];
    });
    return retval;
  };

  var selectedSaveResultsToArray = function () {
    var retval;
    var selected = $("table.selected-for-save tr");

    retval = $.map(selected, function (elem) {
      var id = $(elem).find("th a").first().attr("href").replace(/^#/, "");
      var context = $(elem).find("td.search-result-context a").first().html().trim();
      return [[context, id]];
    });

    return retval;
  };

  var render = function (setElems) {
    var total = setElems.length;
    var elems = setElems.map(function (x) {
      return {
        id: x[1],
        context: x[0]
      };
    });
    var listing = templates['set-listing'].render({
      elements: elems,
      total: total
    });
    setListing().html(listing);
    return mod;
  };

  var view = function (setName) {
    var elems = sets.getSet(setName)[1];
    render(elems);
    return mod;
  };

  var remove = function (setName) {
    sets.removeSet(setName);
    render([]);
    shimi.dispatch.send("sets-changed");
    return mod;
  };

  var union = function (setNameA, setNameB) {
    var newSet = sets.union(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var intersection = function (setNameA, setNameB) {
    var newSet = sets.intersection(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var symetricDifference = function (setNameA, setNameB) {
    var newSet = sets.symetricDifference(setNameA, setNameB);
    render(newSet);
    return mod;
  };

  var relativeComplement = function (setName1, setName2) {
    var newSet = sets.relativeComplement(setName1, setName2);
    render(newSet);
    return mod;
  };

  mod.performOp = function () {
    switch (op().val()) {
    case "view-a":
      view(setA().val());
      break;
    case "view-b":
      view(setB().val());
      break;
    case "remove-a":
      remove(setA().val());
      break;
    case "remove-b":
      remove(setB().val());
      break;
    case "union":
      union(setA().val(), setB().val());
      break;
    case "intersection":
      intersection(setA().val(), setB().val());
      break;
    case "symetric-difference":
      symetricDifference(setA().val(), setB().val());
      break;
    case "relative-complement-b-in-a":
      relativeComplement(setA().val(), setB().val());
      break;
    case "relative-complement-a-in-b":
      relativeComplement(setB().val(), setA().val());
      break;
    default:
      break;
    }
    return mod;
  };

  mod.updateSelection = function () {
    var currNames = sets.getSetNames();
    var newOptions = templates['set-options'].render({
      names: currNames
    });
    setA().html(newOptions);
    setB().html(newOptions);
    worksheetsSet().html(newOptions);
    return mod;
  };

  mod.saveSelected = function () {
    var dialog = $("#new-set-dialog");
    var name = $("#new-set-input").val();
    var target = $("#new-set-target-input").val();
    var selected;
    var newSet;

    if (!utils.isBlank(name)) {
      dialog.hide();
      selected = selectedToArray(target);
      newSet = [name, selected];
      sets.setSet(newSet);
      $("#new-set-input").val("");
      shimi.dispatch.send("sets-changed");
      shimi.flash("Success:", "Set '" + name + "' saved.").highlight();
    } else {
      shimi.flash("Input invalid:", "You must supply a valid name.").error();
    }

    return mod;
  };

  mod.toggleSelectAll = function (target) {
    if ($(target).is(":checked")) {
      $("input.set-element-selection").attr("checked", true);
    } else {
      $("input.set-element-selection").attr("checked", false);
    }
    return mod;
  };

  return mod;
})();