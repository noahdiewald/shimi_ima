// # JSON to
//
// Convert JSON to something else.

var r = require('./recurse.js');
var json_parse = require('./json_parse.js');

// ## Internal Functions

// Insert the strings in the acc object.
var insert = function (lr, acc) {
  'use strict';

  acc.left = acc.left + lr[0];
  acc.right = lr[1] + acc.right;

  return acc;
};

// Process an item. This is where `complex` and `simple` supplied
// functions are called.
var process = function (item, acc, funs, options) {
  'use strict';

  if (item && item.value !== undefined) {
    if (item.type !== 'array' && item.type !== 'object') {
      acc = insert(funs.simple(item, options), acc);
    } else if (item.type && (item.type === 'object' || item.type === 'array')) {
      acc = insert(funs.complex(item, options), acc);
    }
  }

  return acc;
};

// Pop the accstack and insert the current acc left and right on the
// left of the poped object.
var accInsert = function (accstack, acc) {
  'use strict';

  var acc2 = accstack.pop();
  acc2.left = acc2.left + acc.left + acc.right;

  return acc2;
};

// The produce an output based on the AST, functions and options.
var to = function (ast, funs, options) {
  'use strict';

  var acc = insert(funs.context(options), {
    left: '',
    right: ''
  });
  var result;

  // This will be used to recurse the AST. The `head` variable is the
  // current portion of the AST to process. The `rest` variable is the
  // remaining portion. The `acc` is an accumulator that stores the right
  // and left ends of a string that is built up during processing. `stack`
  // is used to save state while descending complex objects. `accstack`
  // is used to store `acc` context when descending. `id` is a reference
  // to the identity function to define the base case.
  var _to = function (head, rest, acc, stack, accstack, id) {
    var isNotObject = head && (head.type !== 'array' && head.type !== 'object');
    var done = rest.length === 0;
    var depleted = stack.length === 0;
    var next;
    var acc2;

    // There are no more items, the stack is depleted and the value
    // doesn't need to be descended, so return the accumulator.
    if (!head && done && depleted) {
      return id.r(acc);
    }
    // If there is more on the stack, process it. We'll want to ignore
    // objects so that they can be handled in the else clause. We'll also
    // want to continue if `head` is undefined, which will indicate that
    // we've hit the end of object or array values.
    else if (!depleted && done && (isNotObject || !head)) {
      // Pop the next group of stored information off the stack where
      // it was previously stored by the 'else' clause below.
      next = stack.pop();

      // This will change the acc depending on `head` information.
      acc = process(head, acc, funs, options);

      // This will nest the current acc string values inside the
      // parent.
      acc2 = accInsert(accstack, acc);

      // Use next instead of `head` and `rest`, `rest` was already
      // depleted in this step.
      return _to.r(next[0], next.slice(1), acc2, stack, accstack, id);
    }
    // Unless it is a complex value (such as array or object), process
    // the value and move on to the next field.
    else if (isNotObject) {
      acc = process(head, acc, funs, options);

      return _to.r(rest[0], rest.slice(1), acc, stack, accstack, id);
    }
    // Otherwise descend the complex value.
    else {
      acc2 = {
        left: '',
        right: ''
      };

      // Push the remaining values onto the stack so they will be
      // processed later.
      stack.push(rest);
      // Push the accumulated left and right strings on to the
      // stack. This will allow them to be pulled out to nest acc2
      // later on.
      accstack.push(acc);

      acc = process(head, acc2, funs, options);

      // Now use the values to specify `head` and `rest`.
      return _to.r(head.value[0], head.value.slice(1), acc2, stack, accstack, id);
    }
  };

  // When the original object isn't null and there is a root.
  if (ast.not_null && ast.root) {
    acc = insert(funs.root(options), acc);

    // If there is more than just an empty list of fields, begin
    // recursion.
    if (ast.root && ast.root.length > 0) {
      result = _to.t(ast.root[0], ast.root.slice(1), acc, [], [], r.identity);
    }
  }

  return acc.left + acc.right;
};

// ## External Functions

// Given a valid subset of JSON (see `json_parse.js`), transform the JSON.
//
// The transformation is determined by functions supplied in the second
// argument. The following must be supplied:
//
// * context * root * simple * complex
//
// All of the above take the `options` argument supplied to this function
// as their final arguement.
//
// `context` will be part of the output even if the JSON provided is
// `null`. It takes only the default argument.
//
// `root` wraps the root of the JSON object. It takes only the default
// argument.
//
// `simple` is for values that are not arrays or objects. It takes the
// current item as an argument. (See `json_parse.js` for an example of
// the form of the item object.)
//
// `complex` wraps objects and arrays. It takes the current item as
// an argument.
//
// The content of options is specified by the caller. It is passed to
// the supplied functions.
var transform = function (json, funs, options) {
  'use strict';

  options = options ? options : {};
  var ast = json_parse.parse(json);

  ast.not_null = json !== 'null';

  return to(ast, funs, options);
};

exports.transform = transform;
