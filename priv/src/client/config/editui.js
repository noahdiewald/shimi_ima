// # Config Editor
//
// *Implicit depends:* DOM
//
// All code for working with the editor.

// ## Variable Definitions

var formalize = require('../formalize.js');
var ajax = require('../ajax.js');
var S = require('../sender.js');

// ## Internal Functions

// Providing a shorter name to call this function.
var forEach = Array.prototype.forEach;

// Toggle the visibility of a group.
var toggle = function (node) {
  'use strict';

  node.classList.toggle('collapsed');
  node.nextSibling.classList.toggle('hidden');

  return 'toggled-subgroup';
};

// Update the default attribute when the value property changes.
var updateDefaults = function (e) {
  'use strict';

  var t = e.target;
  var val = t.value;

  if (t.type === 'text') {
    t.setAttribute('value', val);
  }

  return t;
};

// Get the editor form object.
var editForm = function () {
  'use strict';

  return document.getElementById('edit-form');
};

// Update the attributes associated with the text of the label.
var updateLabelAttributes = function (e) {
  'use strict';

  var label = e.target;
  var elem = document.getElementById(label.nextSibling.id);

  label.title = label.textContent;

  if (label.classList.contains('span-title')) {
    elem.title = label.textContent;
  } else {
    elem.name = label.textContent;
  }
};

// Initialize the form labels.
var formLabelsInit = function (form) {
  'use strict';

  forEach.call(form.getElementsByTagName('span'), function (item) {
    item.contentEditable = true;
    item.oninput = updateLabelAttributes;
  });
};

// Initialize the form inputs.
var formInputsInit = function (form) {
  'use strict';

  forEach.call(form.getElementsByTagName('input'), function (item) {
    item.onchange = updateDefaults;
  });
};

// Remove the class from all instances.
var removeClass = function (items, className) {
  'use strict';

  forEach.call(items, function (item) {
    item.classList.remove(className);
  });
};

// Initialize form elements.
var formElementsInit = function (form) {
  'use strict';

  forEach.call(form.getElementsByTagName('li'), function (item) {
    forEach.call(item.children, function (child) {
      // Keep track of last element with focus.
      child.onfocus = function (e) {
        var oldMark = document.getElementsByClassName('marked');
        var oldLine = document.getElementsByClassName('marked-line');
        removeClass(oldMark, 'marked');
        removeClass(oldLine, 'marked-line');
        item.classList.add('marked-line');
        e.target.classList.add('marked');
      };
    });
  });
};

// Initialize the form.
var formInit = function (form) {
  'use strict';

  formInputsInit(form);
  formElementsInit(form);
  formLabelsInit(form);
};

// Given some json, create a form, perform initialization and display
// it in the editor area.
var fillForm = function (json, options) {
  'use strict';

  if (!options) {
    options = {
      spanLabel: true
    };
  } else {
    options.spanLabel = true;
  }

  var formHTML = formalize.toForm(json, options);
  var form = editForm();

  form.innerHTML = formHTML;
  formInit(form);

  return 'form-filled';
};

// Find the target placement for a new element and return a function
// that will place it there.
var findTarget = function () {
  'use strict';

  var marked = document.getElementsByClassName('marked')[0];
  var markedLine = document.getElementsByClassName('marked-line')[0];
  var retval;

  if (markedLine) {
    retval = function (elem) {
      if (markedLine.nextSibling) {
        markedLine.parentNode.insertBefore(elem, parent.nextSibling);
      } else {
        markedLine.parentNode.appendChild(elem);
      }
    };
  } else {
    retval = function (elem) {
      var firstObj = editForm().getElementsByTagName('ul')[0];
      firstObj.appendChild(elem);
    };
  }

  return retval;
};

// Add an element given JSON.
var addElement = function (json) {
  'use strict';

  var markedLine = document.getElementsByClassName('marked-line')[0];
  var tmp = document.createElement('div');
  var tmpForm = formalize.toForm(json, {
    spanLabel: true
  });
  var targ = findTarget();
  var newElem;

  tmp.innerHTML = tmpForm;
  formInit(tmp);

  newElem = tmp.getElementsByTagName('li')[0];

  // Array elements don't have labels.
  if (markedLine && markedLine.matches('#edit-form ol > li')) {
    newElem.removeChild(newElem.getElementsByTagName('span')[0]);
    newElem.firstChild.removeAttribute('name');
    newElem.firstChild.removeAttribute('title');
  }

  targ(newElem);
};

// ## Exported Functions

// Get the specified stored document and load it into the editor.
var get = function (url) {
  'use strict';

  var complete = function (req) {
    return fillForm(JSON.stringify(req.response));
  };

  ajax.get(url, complete);

  return 'object-loaded';
};

// Load an empty object into the editor.
var fresh = function () {
  'use strict';

  fillForm('{}');

  return 'empty-object-loaded';
};

var create = function () {
  'use strict';

  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var complete = function () {
    S.sender('config-' + category + '-created');
  };

  ajax.post('config/' + category + 's', json, complete);

  return 'object-created';
};

var update = function (args) {
  'use strict';

  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var identifier = obj._id;
  var revision = obj._rev;
  var url = 'config/' + category + 's/' + identifier + '?rev=' + revision;
  var complete = function () {
    get('doctypes/' + obj._id);

    S.sender('config-' + category + '-updated');
  };

  ajax.put(url, json, complete);

  return 'object-updated';
};

var remove = function (args) {
  'use strict';

  var answer = window.confirm('Are you sure you want to delete this?');
  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var obj = JSON.parse(json);
  var category = obj.category;
  var identifier = obj._id;
  var revision = obj._rev;
  var url = 'config/' + category + 's/' + identifier + '?rev=' + revision;
  var complete = function () {
    S.sender('config-' + category + '-deleted');
  };

  if (answer) {
    ajax.del(url, complete);
  }

  return 'object-removed';
};

var restore = function (args) {
  'use strict';

  return 'object-restored';
};

// Move and element up in the tree.
var elementUp = function () {
  'use strict';

  var targ = document.getElementsByClassName('marked-line')[0];
  var prev = targ.previousSibling;

  if (prev) {
    targ.parentElement.insertBefore(targ, prev);
  }

  return 'element-moved-up';
};

// Move and element down in the tree.
var elementDown = function () {
  'use strict';

  var targ = document.getElementsByClassName('marked-line')[0];
  var next = targ.nextSibling;

  if (next) {
    targ.parentElement.insertBefore(targ, next.nextSibling);
  } else {
    targ.parentElement.appendChild(targ);
  }

  return 'element-moved-down';
};

// Remove and element from the tree.
var elementDelete = function (identifier) {
  'use strict';

  var targ = document.getElementsByClassName('marked-line')[0];

  targ.parentElement.removeChild(targ);

  return 'element-removed';
};

// Add an object element to the form.
var addObjectElement = function () {
  'use strict';

  addElement('{"_blank_":{"_first_":""}}');

  return 'text-element-added';
};

// Add an array element to the form.
var addArrayElement = function () {
  'use strict';

  addElement('{"_blank_":[""]}');

  return 'text-element-added';
};

// Add a text element to the form.
var addTextElement = function () {
  'use strict';

  addElement('{"_blank_":""}');

  return 'text-element-added';
};

// Initialize the editor, loading a fresh object.
var init = function (json) {
  'use strict';

  if (json) {
    fillForm(json);
  } else {
    fresh();
  }

  return 'editor-initialized';
};

exports.init = init;
exports.get = get;
exports.fresh = fresh;
exports.update = update;
exports.create = create;
exports.remove = remove;
exports.restore = restore;
exports.toggle = toggle;
exports.elementUp = elementUp;
exports.elementDown = elementDown;
exports.elementDelete = elementDelete;
exports.addObjectElement = addObjectElement;
exports.addArrayElement = addArrayElement;
exports.addTextElement = addTextElement;
