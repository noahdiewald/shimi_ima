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
  var sessionKey = function () {
    return shimi.documents.identifier() + "_sets";
  };

  var member = function (arr, x) {
    return arr.some(function (y) {
      return x[0] === y[0] && x[1] === y[1];
    });
  };

  var processSet = function (set) {
    var name = set[0];
    var arr = sets.unique(set[1], member);
    var procSet = [name, arr];
    return procSet;
  };

  var union = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = sets.union(setElemsA, setElemsB, member);
    render(newSet);
    return mod;
  };

  var intersection = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = sets.intersection(setElemsA, setElemsB, member);
    render(newSet);
    return mod;
  };

  var relativeComplement = function (setName1, setName2) {
    var setElems1 = mod.getSet(setName1)[1];
    var setElems2 = mod.getSet(setName2)[1];
    var newSet = sets.relativeComplement(setElems1, setElems2, member);
    render(newSet);
    return mod;
  };

  var symmetricDifference = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = sets.symmetricDifference(setElemsA, setElemsB, member);
    render(newSet);
    return mod;
  };

  var getSets = function () {
    var curr = window.sessionStorage.getItem(sessionKey());
    var retval = [];

    if (curr !== null) {
      retval = JSON.parse(curr);
    }

    return retval;
  };

  var view = function (setName) {
    var elems = mod.getSet(setName)[1];
    render(elems);
    return mod;
  };

  var remove = function (setName) {
    removeSet(setName);
    render([]);
    shimi.dispatch.send("sets-changed");
    return mod;
  };

  mod.getSet = function (setName) {
    var retval;
    var curr = getSets();
    retval = curr.filter(function (x) {
      return x[0] === setName;
    })[0];
    return retval;
  };

  var removeSet = function (setName) {
    var nnew;
    var curr = getSets();
    nnew = curr.filter(function (x) {
      return x[0] !== setName;
    });
    setSets(nnew);
    return mod;
  };

  var getSetNames = function () {
    var curr = getSets();
    return curr.map(function (x) {
      return x[0];
    });
  };

  var setSets = function (nnew) {
    var procSets;
    if (Array.isArray(nnew)) {
      procSets = nnew.map(function (x) {
        return processSet(x);
      });
      window.sessionStorage.setItem(sessionKey(), JSON.stringify(procSets));
    } else {
      window.sessionStorage.settem(sessionKey(), "[]");
    }

    return mod;
  };

  var setSet = function (nnew) {
    if (Array.isArray(nnew) && nnew.length === 2) {
      var curr = getSets();
      var newName = nnew[0];
      var filtered = curr.filter(function (x) {
        return x[0] !== newName;
      });
      setSets(filtered.concat([nnew]));
    }
    return mod;
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
    case "symmetric-difference":
      symmetricDifference(setA().val(), setB().val());
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
    var currNames = getSetNames();
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
      setSet(newSet);
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
      $("input.set-element-selection").prop("checked", true);
    } else {
      $("input.set-element-selection").prop("checked", false);
    }
    return mod;
  };

  return mod;
})();