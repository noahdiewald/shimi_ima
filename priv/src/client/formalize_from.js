// # Form from
//
// Convert an HTML form to JSON.

// ## Variable Definitions

var htmlparser = require('htmlparser2');

// ## Internal Functions

// Reject non-strings or strings of length zero.
var validateFromArg = function (html) {
  'use strict';

  if (typeof html !== 'string') {
    throw 'invalid HTML: non-string';
  } else if (html.length === 0) {
    throw 'invalid HTML: ""';
  }

  return html;
};

var curr = function (state) {
  'use strict';

  return state.state[state.state.length - 1];
};

var showState = function (state) {
  'use strict';

  return ' [' + state.state.join(',') + ']';
};

var openForm = function (state) {
  'use strict';

  if (curr(state) === 'start') {
    state.state.push('open');
    state.acc = 'null';
  } else {
    throw 'invalid form: only one form allowed' + showState(state);
  }

  return state;
};

var addTextValue = function (state, text) {
  'use strict';

  state.acc = state.acc + '"' + text.replace(/\n/g, '\\n') + '"';

  return state;
};

var addKey = function (state, name) {
  'use strict';

  state.acc = addTextValue(state, name).acc + ':';

  return state;
};

var addComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.acc = state.acc + ',';
  } else {
    state.state.push('need-comma');
  }

  return state;
};

var openObject = function (state, attribs) {
  'use strict';

  if (state.acc === 'null') {
    state.acc = '';
  }

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-object');

  state.acc = state.acc + '{';

  return state;
};

var openArray = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.title) {
    addKey(state, attribs.title);
  }

  state.state.push('open-array');

  state.acc = state.acc + '[';

  return state;
};

var addCorrectlyTypedValue = function (state, value) {
  'use strict';

  switch (value) {
  case 'null':
  case 'true':
  case 'false':
    state.acc = state.acc + value;
    break;
  default:
    addTextValue(state, value);
  }

  return state;
};

var addValue = function (state, attribs) {
  'use strict';

  addComma(state);

  if (attribs.name) {
    addKey(state, attribs.name);
  }

  if (attribs.type === 'text') {
    addCorrectlyTypedValue(state, attribs.value);
  } else if (attribs.type === 'number') {
    state.acc = state.acc + attribs.value;
  }

  return state;
};

var openTextareaValue = function (state, attribs) {
  'use strict';

  addComma(state);
  addKey(state, attribs.name);
  state.state.push('open-text');

  return state;
};

var addTextareaValue = function (state, str) {
  'use strict';

  if (curr(state) === 'open-text') {
    state.state.pop();
    addTextValue(state, str);
  }

  return state;
};

var noComma = function (state) {
  'use strict';

  if (curr(state) === 'need-comma') {
    state.state.pop();
  }

  return state;
};

var klose = function (state, targ, callback) {
  'use strict';

  var current;

  noComma(state);
  current = curr(state);

  if (current === targ) {
    callback(state);
  } else {
    throw 'invalid form: tag mismatch' + showState(state);
  }

  return state;
};

var closeForm = function (state) {
  'use strict';

  klose(state, 'open', function (state) {
    state.state = ['done'];

    return state;
  });

  return state;
};

var closeObject = function (state) {
  'use strict';

  klose(state, 'open-object', function (state) {
    state.state.pop();
    state.acc = state.acc + '}';

    return state;
  });

  return state;
};

var closeArray = function (state) {
  'use strict';

  klose(state, 'open-array', function (state) {
    state.state.pop();
    state.acc = state.acc + ']';

    return state;
  });

  return state;
};

// Main HTML parsing function. It uses the helper functions openForm,
// openObject, addValue and openTextareaValue.
var tryParseHTML = function (html) {
  'use strict';

  var state = {
    state: ['start'],
    acc: ''
  };
  var parser;

  parser = new htmlparser.Parser({
    onopentag: function (name, attribs) {
      switch (name) {
      case 'form':
        openForm(state);
        break;
      case 'ul':
        openObject(state, attribs);
        break;
      case 'ol':
        openArray(state, attribs);
        break;
      case 'input':
        addValue(state, attribs);
        break;
      case 'textarea':
        openTextareaValue(state, attribs);
        break;
      }
    },
    ontext: function (str) {
      if (!str.match(/^\s+$/)) {
        addTextareaValue(state, str);
      }
    },
    onclosetag: function (tagname) {
      switch (tagname) {
      case 'form':
        closeForm(state);
        break;
      case 'ul':
        closeObject(state);
        break;
      case 'ol':
        closeArray(state);
        break;
      }
    }
  });

  parser.write(html);
  parser.end();

  switch (state.state.pop()) {
  case 'start':
    throw 'invalid form: no form found' + showState(state);
  case 'open':
    throw 'invalid form: no closing tag' + showState(state);
  default:
    return state.acc;
  }
};

// ## External Functions

var transform = function (html) {
  'use strict';

  var json;

  validateFromArg(html);
  json = tryParseHTML(html);

  return json;
};

exports.transform = transform;
