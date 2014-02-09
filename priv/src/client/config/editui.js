// # Config Editor
//
// *Implicit depends:* DOM
//
// All code for working with the editor.

// ## Variable Definitions

var formalize = require('../formalize.js');
var ajax = require('../ajax.js');
var sess = require('../sess.js');
var S = require('../sender.js');
var toggle;
var init;

// ## Internal Functions

// Toggle collapsed class
var toggleCollapseNode = function (node) {
  'use strict';

  node.classList.toggle('collapsed');

  return node;
};

// Toggle hide class
var toggleHideNode = function (node) {
  'use strict';

  node.classList.toggle('hidden');

  return node;
};

// Providing a shorter name to call this function.
var forEach = function (list, fun) {
  'use strict';

  Array.prototype.forEach.call(list, fun);
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

  forEach(form.getElementsByTagName('span'), function (item) {
    if (!item.classList.contains('array-element-handle')) {
      item.contentEditable = true;
      item.oninput = updateLabelAttributes;
    }
  });
};

// Initialize the form inputs.
var formInputsInit = function (form) {
  'use strict';

  forEach(form.getElementsByTagName('input'), function (item) {
    item.onchange = updateDefaults;
  });
};

// Remove the class from all instances.
var removeClass = function (item, className) {
  'use strict';

  if (item) {
    item.classList.remove(className);
  }

  return item;
};

// Get previous mark.
var getMark = function () {
  'use strict';

  return {
    mark: document.getElementsByClassName('marked')[0],
    line: document.getElementsByClassName('marked-line')[0]
  };
};

// Remove previous mark.
var removeMark = function () {
  'use strict';

  var old = getMark();
  removeClass(old.mark, 'marked');
  removeClass(old.line, 'marked-line');

  return old;
};

var addMark = function (line, item) {
  'use strict';

  removeMark();
  line.classList.add('marked-line');
  item.classList.add('marked');

  return getMark();
};

// Keep track of last element with focus.
var markItem = function (item) {
  'use strict';

  return function (e) {
    addMark(item, e.target);
  };
};

// Initialize form elements.
var formElementsInit = function (form) {
  'use strict';

  forEach(form.getElementsByTagName('li'), function (item) {
    forEach(item.children, function (child) {
      child.onfocus = markItem(item);
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

// Set the default options.
var setDefaultOptions = function (options) {
  'use strict';
  options = options ? options : {};
  options.spanLabel = true;

  return options;
};

// Given some json, create a form, perform initialization and display
// it in the editor area.
var fillForm = function (json, options) {
  'use strict';

  options = setDefaultOptions(options);
  var formHTML = formalize.toForm(json, options);
  var form = editForm();

  form.innerHTML = formHTML;
  formInit(form);

  return 'form-filled';
};

// Is the element an unordered list element?
var isHTMLUList = function (elem) {
  'use strict';

  return elem instanceof HTMLUListElement;
};

// Is the element an ordered list element?
var isHTMLOList = function (elem) {
  'use strict';

  return elem instanceof HTMLOListElement;
};

// Is the element a list element?
var isHTMLList = function (elem) {
  'use strict';

  return isHTMLOList(elem) || isHTMLUList(elem);
};

// Is this the child of a `ul` list?
var isChildOfHTMLOLList = function (elem) {
  'use strict';

  return isHTMLOList(elem.parentElement);
};

// Array elements don't have labels.
var maybeRemoveLabel = function (elem, targ) {
  'use strict';

  if (targ && isHTMLOList(targ)) {
    elem.removeChild(elem.getElementsByTagName('span')[0]);
    elem.firstChild.removeAttribute('name');
    elem.firstChild.removeAttribute('title');
  }

  return elem;
};

// Get the last child of this node.
var getLastChild = function (node) {
  'use strict';

  return Array.prototype.slice.call(node.children, -1)[0];
};

// Predicate function to determine if marked item is a UL or OL.
var markedIsHTMLList = function () {
  'use strict';

  var markedLine = getMark().line;
  var lastChild = getLastChild(markedLine);

  return isHTMLList(lastChild);
};

// Find the target placement for a new element and return a function
// that will place it there.
var findTarget = function (asChild) {
  'use strict';

  var markedLine = getMark().line;
  var targ = markedLine.parentNode;
  var retval;

  if (markedLine) {
    // When the item should be added as a child to another item.
    if (asChild) {
      if (markedIsHTMLList()) {
        targ = getLastChild(markedLine);
      } else {
        // This is the wrong type of target element for adding a child
        // to.
        asChild = false;
      }
    }

    retval = function (elem) {
      elem = maybeRemoveLabel(elem, targ);

      if (markedLine.nextSibling && !asChild) {
        targ.insertBefore(elem, markedLine.nextSibling);
      } else {
        targ.appendChild(elem);
      }
    };
  } else {
    retval = function (elem) {
      elem = maybeRemoveLabel(elem, targ);

      var firstObj = editForm().getElementsByTagName('ul')[0];
      firstObj.appendChild(elem);
    };
  }

  return retval;
};

// Add an element given JSON.
var addElement = function (json, asChild) {
  'use strict';

  var markedLine = getMark().line;
  var tmp = document.createElement('div');
  var tmpForm = formalize.toForm(json, setDefaultOptions());
  var targ = findTarget(asChild);
  var newElem;

  tmp.innerHTML = tmpForm;
  formInit(tmp);
  newElem = tmp.getElementsByTagName('li')[0];
  targ(newElem);
};

var defaultToggle = function () {
  'use strict';

  forEach(document.querySelectorAll('#edit-form span.span-title'), function (x) {
    toggle('title', x);
  });
  forEach(document.querySelectorAll('#edit-form ol > li'), function (x) {
    toggle('array-elem', x);
  });

  return 'default-toggle-applied';
};

// ## Exported Functions

// Get the specified stored document and load it into the editor.
var get = function (url) {
  'use strict';

  var complete = function (req) {
    return init(JSON.stringify(req.response));
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

  var targ = getMark().line;
  var prev = targ.previousSibling;

  if (prev) {
    targ.parentElement.insertBefore(targ, prev);
  }

  return 'element-moved-up';
};

// Move and element down in the tree.
var elementDown = function () {
  'use strict';

  var targ = getMark().line;
  var next = targ.nextSibling;

  if (next) {
    targ.parentElement.insertBefore(targ, next.nextSibling);
  } else {
    targ.parentElement.appendChild(targ);
  }

  return 'element-moved-down';
};

// Remove the marked element from the tree.
var elementDelete = function () {
  'use strict';

  var targ = getMark().line;

  targ.parentElement.removeChild(targ);

  return 'element-removed';
};

// Add an object element to the form.
var addObjectElement = function (asChild) {
  'use strict';

  addElement('{"_blank_":{"_first_":""}}', asChild);

  return 'object-element-added';
};

// Add an array element to the form.
var addArrayElement = function (asChild) {
  'use strict';

  addElement('{"_blank_":[""]}', asChild);

  return 'array-element-added';
};

// Add a text element to the form.
var addTextElement = function (asChild) {
  'use strict';

  addElement('{"_blank_":""}', asChild);

  return 'text-element-added';
};

// Add an object element to the form.
var addChildObjectElement = function () {
  'use strict';

  addObjectElement(true);

  return 'child-object-element-added';
};

// Add a child array element to an object or array.
var addChildArrayElement = function () {
  'use strict';

  addArrayElement(true);

  return 'child-array-element-added';
};

// Add a child text element to an object or array.
var addChildTextElement = function () {
  'use strict';

  addTextElement(true);

  return 'child-text-element-added';
};

// Mark a "line" aka an entire li. This is invoked when ol > li is
// clicked.
var markLine = function (line) {
  'use strict';

  addMark(line, line);

  return 'line-marked';
};

// Toggle the visibility of a group.
toggle = function (kind, node) {
  'use strict';

  var hideNode = false;

  if (kind === 'title') {
    hideNode = node.nextSibling;
  } else if (isHTMLList(node.children[0])) {
    hideNode = node.children[0];
  }

  if (hideNode) {
    toggleCollapseNode(node);
    toggleHideNode(hideNode);
  }

  return 'toggled-subgroup';
};

// Paste a node plus children in new context.
var paste = function (asChild) {
  'use strict';

  var copied = sess.get('shimi-ima-copied');
  var tmp;
  var tmpForm;
  var tmpWrap;
  var copiedChild;
  var json;

  if (copied !== null) {
    tmp = document.createElement('div');
    tmpForm = document.createElement('form');
    tmpWrap = document.createElement('ul');
    tmpWrap.innerHTML = copied.html;

    if (copied.parentWasOL) {
      copiedChild = tmpWrap.firstChild.firstChild;

      if (isHTMLList(copiedChild)) {
        copiedChild.setAttribute('title', '_blank_');
      } else {
        copiedChild.setAttribute('name', '_blank_');
      }
    }

    tmpForm.appendChild(tmpWrap);
    tmp.appendChild(tmpForm);

    json = formalize.fromForm(tmp.innerHTML);

    addElement(json, asChild);
  }

  return 'pasted';
};

// Paste a node plus children in new context as a child of the currently
// marked element.
var pasteChild = function () {
  'use strict';

  paste(true);

  return 'child-pasted';
};

// Copy the marked item into session storage
var copy = function () {
  'use strict';

  var markedLine = getMark().line;
  var copyInfo = {
    _id: 'shimi-ima-copied',
    html: markedLine.outerHTML,
    parentWasOL: isChildOfHTMLOLList(markedLine)
  };

  sess.replace(copyInfo);

  return 'copied';
};

// Copy the marked item into session storage before deleting it.
var cut = function () {
  'use strict';

  copy();
  elementDelete();

  return 'cut';
};

// Move marked to a higher tier.
var promote = function () {
  'use strict';

  return 'promoted';
};

// Move marked to a lower obj/UL tier.
var demote = function () {
  'use strict';

  return 'demoted';
};

// Initialize the editor, loading a fresh object.
init = function (json) {
  'use strict';

  if (json) {
    fillForm(json);
    defaultToggle();
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
exports.addChildObjectElement = addChildObjectElement;
exports.addChildArrayElement = addChildArrayElement;
exports.addChildTextElement = addChildTextElement;
exports.markLine = markLine;
exports.copy = copy;
exports.cut = cut;
exports.paste = paste;
exports.pasteChild = pasteChild;
