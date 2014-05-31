require=(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

(function (Hogan) {
  // Setup regex  assignments
  // remove whitespace according to Mustache spec
  var rIsWhitespace = /\S/,
      rQuot = /\"/g,
      rNewline =  /\n/g,
      rCr = /\r/g,
      rSlash = /\\/g,
      tagTypes = {
        '#': 1, '^': 2, '/': 3,  '!': 4, '>': 5,
        '<': 6, '=': 7, '_v': 8, '{': 9, '&': 10
      };

  Hogan.scan = function scan(text, delimiters) {
    var len = text.length,
        IN_TEXT = 0,
        IN_TAG_TYPE = 1,
        IN_TAG = 2,
        state = IN_TEXT,
        tagType = null,
        tag = null,
        buf = '',
        tokens = [],
        seenTag = false,
        i = 0,
        lineStart = 0,
        otag = '{{',
        ctag = '}}';

    function addBuf() {
      if (buf.length > 0) {
        tokens.push(new String(buf));
        buf = '';
      }
    }

    function lineIsWhitespace() {
      var isAllWhitespace = true;
      for (var j = lineStart; j < tokens.length; j++) {
        isAllWhitespace =
          (tokens[j].tag && tagTypes[tokens[j].tag] < tagTypes['_v']) ||
          (!tokens[j].tag && tokens[j].match(rIsWhitespace) === null);
        if (!isAllWhitespace) {
          return false;
        }
      }

      return isAllWhitespace;
    }

    function filterLine(haveSeenTag, noNewLine) {
      addBuf();

      if (haveSeenTag && lineIsWhitespace()) {
        for (var j = lineStart, next; j < tokens.length; j++) {
          if (!tokens[j].tag) {
            if ((next = tokens[j+1]) && next.tag == '>') {
              // set indent to token value
              next.indent = tokens[j].toString()
            }
            tokens.splice(j, 1);
          }
        }
      } else if (!noNewLine) {
        tokens.push({tag:'\n'});
      }

      seenTag = false;
      lineStart = tokens.length;
    }

    function changeDelimiters(text, index) {
      var close = '=' + ctag,
          closeIndex = text.indexOf(close, index),
          delimiters = trim(
            text.substring(text.indexOf('=', index) + 1, closeIndex)
          ).split(' ');

      otag = delimiters[0];
      ctag = delimiters[1];

      return closeIndex + close.length - 1;
    }

    if (delimiters) {
      delimiters = delimiters.split(' ');
      otag = delimiters[0];
      ctag = delimiters[1];
    }

    for (i = 0; i < len; i++) {
      if (state == IN_TEXT) {
        if (tagChange(otag, text, i)) {
          --i;
          addBuf();
          state = IN_TAG_TYPE;
        } else {
          if (text.charAt(i) == '\n') {
            filterLine(seenTag);
          } else {
            buf += text.charAt(i);
          }
        }
      } else if (state == IN_TAG_TYPE) {
        i += otag.length - 1;
        tag = tagTypes[text.charAt(i + 1)];
        tagType = tag ? text.charAt(i + 1) : '_v';
        if (tagType == '=') {
          i = changeDelimiters(text, i);
          state = IN_TEXT;
        } else {
          if (tag) {
            i++;
          }
          state = IN_TAG;
        }
        seenTag = i;
      } else {
        if (tagChange(ctag, text, i)) {
          tokens.push({tag: tagType, n: trim(buf), otag: otag, ctag: ctag,
                       i: (tagType == '/') ? seenTag - ctag.length : i + otag.length});
          buf = '';
          i += ctag.length - 1;
          state = IN_TEXT;
          if (tagType == '{') {
            if (ctag == '}}') {
              i++;
            } else {
              cleanTripleStache(tokens[tokens.length - 1]);
            }
          }
        } else {
          buf += text.charAt(i);
        }
      }
    }

    filterLine(seenTag, true);

    return tokens;
  }

  function cleanTripleStache(token) {
    if (token.n.substr(token.n.length - 1) === '}') {
      token.n = token.n.substring(0, token.n.length - 1);
    }
  }

  function trim(s) {
    if (s.trim) {
      return s.trim();
    }

    return s.replace(/^\s*|\s*$/g, '');
  }

  function tagChange(tag, text, index) {
    if (text.charAt(index) != tag.charAt(0)) {
      return false;
    }

    for (var i = 1, l = tag.length; i < l; i++) {
      if (text.charAt(index + i) != tag.charAt(i)) {
        return false;
      }
    }

    return true;
  }

  function buildTree(tokens, kind, stack, customTags) {
    var instructions = [],
        opener = null,
        token = null;

    while (tokens.length > 0) {
      token = tokens.shift();
      if (token.tag == '#' || token.tag == '^' || isOpener(token, customTags)) {
        stack.push(token);
        token.nodes = buildTree(tokens, token.tag, stack, customTags);
        instructions.push(token);
      } else if (token.tag == '/') {
        if (stack.length === 0) {
          throw new Error('Closing tag without opener: /' + token.n);
        }
        opener = stack.pop();
        if (token.n != opener.n && !isCloser(token.n, opener.n, customTags)) {
          throw new Error('Nesting error: ' + opener.n + ' vs. ' + token.n);
        }
        opener.end = token.i;
        return instructions;
      } else {
        instructions.push(token);
      }
    }

    if (stack.length > 0) {
      throw new Error('missing closing tag: ' + stack.pop().n);
    }

    return instructions;
  }

  function isOpener(token, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].o == token.n) {
        token.tag = '#';
        return true;
      }
    }
  }

  function isCloser(close, open, tags) {
    for (var i = 0, l = tags.length; i < l; i++) {
      if (tags[i].c == close && tags[i].o == open) {
        return true;
      }
    }
  }

  Hogan.generate = function (tree, text, options) {
    var code = 'var _=this;_.b(i=i||"");' + walk(tree) + 'return _.fl();';
    if (options.asString) {
      return 'function(c,p,i){' + code + ';}';
    }

    return new Hogan.Template(new Function('c', 'p', 'i', code), text, Hogan, options);
  }

  function esc(s) {
    return s.replace(rSlash, '\\\\')
            .replace(rQuot, '\\\"')
            .replace(rNewline, '\\n')
            .replace(rCr, '\\r');
  }

  function chooseMethod(s) {
    return (~s.indexOf('.')) ? 'd' : 'f';
  }

  function walk(tree) {
    var code = '';
    for (var i = 0, l = tree.length; i < l; i++) {
      var tag = tree[i].tag;
      if (tag == '#') {
        code += section(tree[i].nodes, tree[i].n, chooseMethod(tree[i].n),
                        tree[i].i, tree[i].end, tree[i].otag + " " + tree[i].ctag);
      } else if (tag == '^') {
        code += invertedSection(tree[i].nodes, tree[i].n,
                                chooseMethod(tree[i].n));
      } else if (tag == '<' || tag == '>') {
        code += partial(tree[i]);
      } else if (tag == '{' || tag == '&') {
        code += tripleStache(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag == '\n') {
        code += text('"\\n"' + (tree.length-1 == i ? '' : ' + i'));
      } else if (tag == '_v') {
        code += variable(tree[i].n, chooseMethod(tree[i].n));
      } else if (tag === undefined) {
        code += text('"' + esc(tree[i]) + '"');
      }
    }
    return code;
  }

  function section(nodes, id, method, start, end, tags) {
    return 'if(_.s(_.' + method + '("' + esc(id) + '",c,p,1),' +
           'c,p,0,' + start + ',' + end + ',"' + tags + '")){' +
           '_.rs(c,p,' +
           'function(c,p,_){' +
           walk(nodes) +
           '});c.pop();}';
  }

  function invertedSection(nodes, id, method) {
    return 'if(!_.s(_.' + method + '("' + esc(id) + '",c,p,1),c,p,1,0,0,"")){' +
           walk(nodes) +
           '};';
  }

  function partial(tok) {
    return '_.b(_.rp("' +  esc(tok.n) + '",c,p,"' + (tok.indent || '') + '"));';
  }

  function tripleStache(id, method) {
    return '_.b(_.t(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function variable(id, method) {
    return '_.b(_.v(_.' + method + '("' + esc(id) + '",c,p,0)));';
  }

  function text(id) {
    return '_.b(' + id + ');';
  }

  Hogan.parse = function(tokens, text, options) {
    options = options || {};
    return buildTree(tokens, '', [], options.sectionTags || []);
  },

  Hogan.cache = {};

  Hogan.compile = function(text, options) {
    // options
    //
    // asString: false (default)
    //
    // sectionTags: [{o: '_foo', c: 'foo'}]
    // An array of object with o and c fields that indicate names for custom
    // section tags. The example above allows parsing of {{_foo}}{{/foo}}.
    //
    // delimiters: A string that overrides the default delimiters.
    // Example: "<% %>"
    //
    options = options || {};

    var key = text + '||' + !!options.asString;

    var t = this.cache[key];

    if (t) {
      return t;
    }

    t = this.generate(this.parse(this.scan(text, options.delimiters), text, options), text, options);
    return this.cache[key] = t;
  };
})(typeof exports !== 'undefined' ? exports : Hogan);

},{}],2:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

// This file is for use with Node.js. See dist/ for browser files.

var Hogan = require('./compiler');
Hogan.Template = require('./template').Template;
module.exports = Hogan; 
},{"./compiler":1,"./template":3}],3:[function(require,module,exports){
/*
 *  Copyright 2011 Twitter, Inc.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

var Hogan = {};

(function (Hogan, useArrayBuffer) {
  Hogan.Template = function (renderFunc, text, compiler, options) {
    this.r = renderFunc || this.r;
    this.c = compiler;
    this.options = options;
    this.text = text || '';
    this.buf = (useArrayBuffer) ? [] : '';
  }

  Hogan.Template.prototype = {
    // render: replaced by generated code.
    r: function (context, partials, indent) { return ''; },

    // variable escaping
    v: hoganEscape,

    // triple stache
    t: coerceToString,

    render: function render(context, partials, indent) {
      return this.ri([context], partials || {}, indent);
    },

    // render internal -- a hook for overrides that catches partials too
    ri: function (context, partials, indent) {
      return this.r(context, partials, indent);
    },

    // tries to find a partial in the curent scope and render it
    rp: function(name, context, partials, indent) {
      var partial = partials[name];

      if (!partial) {
        return '';
      }

      if (this.c && typeof partial == 'string') {
        partial = this.c.compile(partial, this.options);
      }

      return partial.ri(context, partials, indent);
    },

    // render a section
    rs: function(context, partials, section) {
      var tail = context[context.length - 1];

      if (!isArray(tail)) {
        section(context, partials, this);
        return;
      }

      for (var i = 0; i < tail.length; i++) {
        context.push(tail[i]);
        section(context, partials, this);
        context.pop();
      }
    },

    // maybe start a section
    s: function(val, ctx, partials, inverted, start, end, tags) {
      var pass;

      if (isArray(val) && val.length === 0) {
        return false;
      }

      if (typeof val == 'function') {
        val = this.ls(val, ctx, partials, inverted, start, end, tags);
      }

      pass = (val === '') || !!val;

      if (!inverted && pass && ctx) {
        ctx.push((typeof val == 'object') ? val : ctx[ctx.length - 1]);
      }

      return pass;
    },

    // find values with dotted names
    d: function(key, ctx, partials, returnFound) {
      var names = key.split('.'),
          val = this.f(names[0], ctx, partials, returnFound),
          cx = null;

      if (key === '.' && isArray(ctx[ctx.length - 2])) {
        return ctx[ctx.length - 1];
      }

      for (var i = 1; i < names.length; i++) {
        if (val && typeof val == 'object' && names[i] in val) {
          cx = val;
          val = val[names[i]];
        } else {
          val = '';
        }
      }

      if (returnFound && !val) {
        return false;
      }

      if (!returnFound && typeof val == 'function') {
        ctx.push(cx);
        val = this.lv(val, ctx, partials);
        ctx.pop();
      }

      return val;
    },

    // find values with normal names
    f: function(key, ctx, partials, returnFound) {
      var val = false,
          v = null,
          found = false;

      for (var i = ctx.length - 1; i >= 0; i--) {
        v = ctx[i];
        if (v && typeof v == 'object' && key in v) {
          val = v[key];
          found = true;
          break;
        }
      }

      if (!found) {
        return (returnFound) ? false : "";
      }

      if (!returnFound && typeof val == 'function') {
        val = this.lv(val, ctx, partials);
      }

      return val;
    },

    // higher order templates
    ho: function(val, cx, partials, text, tags) {
      var compiler = this.c;
      var options = this.options;
      options.delimiters = tags;
      var text = val.call(cx, text);
      text = (text == null) ? String(text) : text.toString();
      this.b(compiler.compile(text, options).render(cx, partials));
      return false;
    },

    // template result buffering
    b: (useArrayBuffer) ? function(s) { this.buf.push(s); } :
                          function(s) { this.buf += s; },
    fl: (useArrayBuffer) ? function() { var r = this.buf.join(''); this.buf = []; return r; } :
                           function() { var r = this.buf; this.buf = ''; return r; },

    // lambda replace section
    ls: function(val, ctx, partials, inverted, start, end, tags) {
      var cx = ctx[ctx.length - 1],
          t = null;

      if (!inverted && this.c && val.length > 0) {
        return this.ho(val, cx, partials, this.text.substring(start, end), tags);
      }

      t = val.call(cx);

      if (typeof t == 'function') {
        if (inverted) {
          return true;
        } else if (this.c) {
          return this.ho(t, cx, partials, this.text.substring(start, end), tags);
        }
      }

      return t;
    },

    // lambda replace variable
    lv: function(val, ctx, partials) {
      var cx = ctx[ctx.length - 1];
      var result = val.call(cx);

      if (typeof result == 'function') {
        result = coerceToString(result.call(cx));
        if (this.c && ~result.indexOf("{\u007B")) {
          return this.c.compile(result, this.options).render(cx, partials);
        }
      }

      return coerceToString(result);
    }

  };

  var rAmp = /&/g,
      rLt = /</g,
      rGt = />/g,
      rApos =/\'/g,
      rQuot = /\"/g,
      hChars =/[&<>\"\']/;


  function coerceToString(val) {
    return String((val === null || val === undefined) ? '' : val);
  }

  function hoganEscape(str) {
    str = coerceToString(str);
    return hChars.test(str) ?
      str
        .replace(rAmp,'&amp;')
        .replace(rLt,'&lt;')
        .replace(rGt,'&gt;')
        .replace(rApos,'&#39;')
        .replace(rQuot, '&quot;') :
      str;
  }

  var isArray = Array.isArray || function(a) {
    return Object.prototype.toString.call(a) === '[object Array]';
  };

})(typeof exports !== 'undefined' ? exports : Hogan);


},{}],4:[function(require,module,exports){
self.onmessage = function (e) {
  'use strict';

  self.postMessage(e.data);
  self.close();
};

},{}],"mkFiG5":[function(require,module,exports){
var Hogan = require('hogan.js');
var t = {
  'changelog-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doc",c,p,1),c,p,0,8,777,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,73,188,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a");_.b("\n" + i);_.b("      href=\"#");_.b(_.v(_.f("document_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"view-document-link\">");_.b("\n" + i);_.b("      ");if(_.s(_.f("head_values",c,p,1),c,p,0,298,305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.d(".",c,p,0)));});c.pop();}_.b("\n" + i);_.b("    </a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("change_type",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("user",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("timestamp",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n" + i);if(_.s(_.f("changes",c,p,1),c,p,0,459,764,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <tr class=\"change-change\">");_.b("\n" + i);_.b("    <th>");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("fieldsetLabel",c,p,0)));_.b(": ");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("\n" + i);_.b("    </th>");_.b("\n" + i);_.b("    <td colspan=3>");_.b("\n" + i);if(!_.s(_.f("originalValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\n" + i);_.b("      →");_.b("\n" + i);if(!_.s(_.f("newValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("newValue",c,p,0)));_.b("\n" + i);_.b("    </td>");_.b("\n" + i);_.b("  </tr>");_.b("\n");});c.pop();}});c.pop();}return _.fl();;}),
  'charseqs-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'config-maintenance' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div id=\"maintenance\">");_.b("\n" + i);_.b("  <h3>Upgrade Project</h3>");_.b("\n" + i);_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Clicking the button below will initiate an upgrade of the project");_.b("\n" + i);_.b("    core design document to the latest version available on your");_.b("\n" + i);_.b("    system.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Be aware that this may cause significant slowness on your system");_.b("\n" + i);_.b("    while view indexes are rebuilt.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("\n" + i);_.b("  <a id=\"maintenance-upgrade-button\" class=\"maintenance-upgrade-button link-button\">Upgrade</a>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'doctypes-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"edit-doctype-link\">");_.b(_.v(_.f("key",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-edit' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<h2 class=\"header\">Edit</h2>");_.b("\n" + i);_.b("\n" + i);if(_.s(_.f("has_rows",c,p,1),c,p,0,43,1862,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"edit-document-form\" class=\"ui-widget ui-corner-all\">");_.b("\n" + i);_.b("  ");_.b("\n" + i);_.b("    <div id=\"edit-tabs\">");_.b("\n" + i);_.b("      <div id=\"tabs-container\">");_.b("\n" + i);_.b("        <ul id=\"tab-list\">");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,219,338,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <li>");_.b("\n" + i);_.b("            <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              ");_.b(_.v(_.f("label",c,p,0)));_.b("\n" + i);_.b("            </a>");_.b("\n" + i);_.b("          </li>");_.b("\n");});c.pop();}_.b("        </ul>");_.b("\n" + i);_.b("      </div>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,400,1448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <fieldset");_.b("\n" + i);_.b("          id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          class=\"ui-widget ui-widget-content ui-corner-all fieldset\"");_.b("\n" + i);_.b("          data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          data-field-project=\"project-");_.b(_.v(_.f("project_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-doctype=\"");_.b(_.v(_.f("doctype",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("          data-fieldset-multiple=\"");_.b(_.v(_.f("multiple",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-collapse=\"");_.b(_.v(_.f("collapse",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-label=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-order=\"");_.b(_.v(_.f("order",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-project=\"project-");_.b(_.v(_.f("project_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-fieldset-doctype=\"");_.b(_.v(_.f("doctype_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <p>");_.b(_.v(_.f("description",c,p,0)));_.b("</p>");_.b("\n" + i);_.b("          <div ");_.b("\n" + i);_.b("            id=\"container-");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("            class=\"fieldset-container\"");_.b("\n" + i);_.b("            data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"></div>");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,1279,1408,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("              <a ");_.b("\n" + i);_.b("                class=\"add-button link-button\" ");_.b("\n" + i);_.b("                data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">Add</a>");_.b("\n");});c.pop();}_.b("        </fieldset>");_.b("\n");});c.pop();}_.b("    </div>");_.b("\n" + i);_.b("    <div id=\"submit-button-area\">");_.b("\n" + i);_.b("      <a id=\"clear-document-button\" class=\"clear-button link-button\">Clear Form</a>");_.b("\n" + i);_.b("      <a data-group-id=\"all-document-container\" id=\"create-document-button\" class=\"create-button link-button\">Create as New</a>");_.b("\n" + i);_.b("      <a data-group-id=\"all-document-container\" id=\"save-document-button\" class=\"save-button link-button hidden\">Save</a>");_.b("\n" + i);_.b("    </div>");_.b("\n" + i);_.b("  </div>");_.b("\n");});c.pop();}if(!_.s(_.f("has_rows",c,p,1),c,p,1,0,0,"")){_.b("<p>");_.b("\n" + i);_.b("  You must add fields and fieldsets before you can create a document of this type.");_.b("\n" + i);_.b("</p>");_.b("\n");};return _.fl();;}),
  'document-search-results' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr>");_.b("\n" + i);_.b("  <th>");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"view-document-link\">");_.b(_.v(_.f("key",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td class=\"search-result-context\">");_.b("\n" + i);_.b("    <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\" class=\"view-document-link\">");_.b(_.v(_.f("value",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-search' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("are_results",c,p,1),c,p,0,16,1213,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div class=\"total-rows-info\">");_.b("\n" + i);_.b("    <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("  </div>");_.b("\n" + i);_.b("  <div id=\"save-search-results\">");_.b("\n" + i);_.b("    <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("  </div>");_.b("\n" + i);if(_.s(_.f("index_listing",c,p,1),c,p,0,191,477,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <div class=\"search-results\">");_.b("\n" + i);_.b("      <input type=\"checkbox\" class=\"select-results\" name=\"select-results\" />");_.b("\n" + i);_.b("      <label for=\"select-results\">Select Results</label>");_.b("\n" + i);_.b("      <table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,390,439,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-search-results",c,p,"          "));});c.pop();}_.b("      </table>");_.b("\n" + i);_.b("    </div>");_.b("\n");});c.pop();}if(!_.s(_.f("index_listing",c,p,1),c,p,1,0,0,"")){if(_.s(_.f("rows",c,p,1),c,p,0,530,1182,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <h5 class=\"search-result-field-id toggler\"");_.b("\n" + i);_.b("        data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("        data-target=\"results-for-field-");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("        title=\"Click to display\">");_.b("\n" + i);_.b("      <a href=\"#\" title=\"Double click to add as search option\">");_.b("\n" + i);_.b("        ");_.b(_.v(_.f("id",c,p,0)));_.b("\n" + i);_.b("      </a> ");_.b("\n" + i);_.b("      (");_.b(_.v(_.f("total_rows",c,p,0)));_.b(")");_.b("\n" + i);_.b("    </h5>");_.b("\n" + i);_.b("    ");_.b("\n" + i);_.b("    <div class=\"search-results\"");_.b("\n" + i);_.b("         id=\"results-for-field-");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("      <input type=\"checkbox\" class=\"select-results\" name=\"select-results-");_.b(_.v(_.d("field.id",c,p,0)));_.b("\" />");_.b("\n" + i);_.b("      <label for=\"select-results-");_.b(_.v(_.f("id",c,p,0)));_.b("\">Select Results</label>");_.b("\n" + i);_.b("      <table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,1093,1142,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-search-results",c,p,"          "));});c.pop();}_.b("      </table>");_.b("\n" + i);_.b("    </div>");_.b("\n");});c.pop();}};});c.pop();}if(!_.s(_.f("are_results",c,p,1),c,p,1,0,0,"")){_.b("  <em>No Results</em>");_.b("\n");};return _.fl();;}),
  'document-view-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li ");_.b("\n" + i);_.b("  class=\"field-view ");_.b("\n" + i);_.b("    ");if(_.s(_.f("changed",c,p,1),c,p,0,42,49,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("  data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("instance",c,p,1),c,p,0,108,150,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b("  data-field-value=\"");_.b(_.v(_.f("json_value",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("changed",c,p,1),c,p,0,235,343,"{{ }}")){_.rs(c,p,function(c,p,_){if(!_.s(_.f("newfield",c,p,1),c,p,1,0,0,"")){_.b("<span class=\"small-control view-field-change\" title=\"");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\">→</span>");};});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("is_textarea",c,p,1),c,p,0,375,426,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <span class=\"retain-white\">");_.b(_.v(_.f("value",c,p,0)));_.b("</span>");_.b("\n");});c.pop();}if(!_.s(_.f("is_textarea",c,p,1),c,p,1,0,0,"")){_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n");};_.b("</li>");_.b("\n");return _.fl();;}),
  'document-view-tree' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("previous_revision",c,p,1),c,p,0,22,76,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"revision-message\">Previous Revision</div>");_.b("\n");});c.pop();}_.b("\n" + i);if(_.s(_.f("deleted_",c,p,1),c,p,0,113,163,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"deleted-message\"><b>Deleted</b></div>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<ul>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,197,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <li");_.b("\n" + i);_.b("    class=\"fieldset-view");_.b("\n" + i);_.b("      ");if(_.s(_.f("collapse",c,p,1),c,p,0,248,257,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapsed");});c.pop();}_.b("\n" + i);_.b("      ");if(_.s(_.f("altered",c,p,1),c,p,0,289,296,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("    data-fieldset-fieldset=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("addition",c,p,1),c,p,0,414,468,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset added\" class=\"addition\">+</span>");});c.pop();}if(_.s(_.f("removal",c,p,1),c,p,0,493,548,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset removed\" class=\"removal\">−</span>");});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,579,818,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <ol>");_.b("\n" + i);if(_.s(_.f("multifields",c,p,1),c,p,0,613,785,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <ul class=\"multifield\">");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,684,737,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"              "));});c.pop();}_.b("          </ul>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("      </ol>");_.b("\n");});c.pop();}if(!_.s(_.f("multiple",c,p,1),c,p,1,0,0,"")){_.b("      <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,880,925,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"          "));});c.pop();}_.b("      </ul>");_.b("\n");};_.b("  </li>");_.b("\n");});c.pop();}_.b("</ul>");_.b("\n" + i);_.b("\n" + i);_.b("<div class=\"timestamps\">");_.b("\n" + i);_.b("  <dl>");_.b("\n" + i);_.b("    <dt>Created At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("created_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Created By</dt><dd>");_.b(_.v(_.f("created_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("updated_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated By</dt><dd>");_.b(_.v(_.f("updated_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>ID</dt><dd>");_.b(_.v(_.f("_id",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("  </dl>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'document-view' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doctype_info",c,p,1),c,p,0,17,187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <h2 class=\"header\">View</h2>");_.b("\n" + i);_.b("\n" + i);_.b("  <form id=\"view-jump\">");_.b("\n" + i);_.b("    <label for=\"view-jump-id\">Id</label>");_.b("\n" + i);_.b("    <input type=\"text\" id=\"view-jump-id\" name=\"view-jump-id\">");_.b("\n" + i);_.b("  </form>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<div id=\"document-view-info\"");_.b("\n" + i);_.b("     data-document-deleted=\"");_.b(_.v(_.f("deleted_",c,p,0)));_.b("\"");_.b("\n" + i);_.b("     data-document-document=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("     data-document-rev=\"");_.b(_.v(_.f("_rev",c,p,0)));_.b("\"></div>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-restore-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button hidden\">Restore</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-edit-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Edit</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-delete-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Delete</a>");_.b("\n" + i);_.b("\n" + i);_.b("<nav id=\"history\">");_.b("\n" + i);if(_.s(_.f("revs_info",c,p,1),c,p,0,716,991,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("status",c,p,1),c,p,0,732,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("         class=\"revision-link\"");_.b("\n" + i);_.b("         data-group-id=\"document-view-info\"");_.b("\n" + i);if(_.s(_.f("first",c,p,1),c,p,0,854,900,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("         id=\"current-revision-link\"");_.b("\n");});c.pop();}_.b("         data-document-oldrev=\"");_.b(_.v(_.f("rev",c,p,0)));_.b("\">");_.b(_.v(_.f("count",c,p,0)));_.b("</a>");_.b("\n");});c.pop();}});c.pop();}_.b("</nav>");_.b("\n" + i);_.b("\n" + i);_.b("<div id=\"document-view-tree\">");_.b("\n" + i);_.b(_.rp("document-view-tree",c,p,"  "));_.b("</div>");_.b("\n");return _.fl();;}),
  'fields' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("fields",c,p,1),c,p,0,11,5710,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div ");_.b("\n" + i);_.b("    class=\"field-container\" ");_.b("\n" + i);_.b("    data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <label for=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\">");_.b("\n" + i);_.b("      <span class=\"label-text\">");_.b(_.v(_.f("label",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("      <span ");_.b("\n" + i);_.b("        class=\"ui-icon ui-icon-help\" ");_.b("\n" + i);_.b("        title=\"");if(_.s(_.f("date",c,p,1),c,p,0,237,264,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("Format date as: yyyy-mm-dd.");});c.pop();}_.b(" ");_.b(_.v(_.f("description",c,p,0)));_.b("\"></span>");_.b("\n" + i);_.b("    </label>");_.b("\n" + i);if(_.s(_.f("text",c,p,1),c,p,0,327,602,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field text ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,435,511,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("integer",c,p,1),c,p,0,628,905,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field number ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,738,814,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("rational",c,p,1),c,p,0,935,1212,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field number ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"text\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1045,1121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("date",c,p,1),c,p,0,1239,1525,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input ");_.b("\n" + i);_.b("      class=\"field date field-text ui-widget ui-corner-all\" ");_.b("\n" + i);_.b("      type=\"date\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1358,1434,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      value=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}if(!_.s(_.f("default_exists",c,p,1),c,p,1,0,0,"")){_.b("      value=\"\"");_.b("\n");};});c.pop();}if(_.s(_.f("boolean",c,p,1),c,p,0,1551,1711,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <input");_.b("\n" + i);_.b("      class=\"boolean field ui-widget ui-corner-all\"");_.b("\n" + i);_.b("      type=\"checkbox\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1664,1687,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        checked");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("openboolean",c,p,1),c,p,0,1744,1924,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field open-boolean ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,1846,1900,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("select",c,p,1),c,p,0,1956,2130,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field select ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2052,2106,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("docselect",c,p,1),c,p,0,2160,2334,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field select ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2256,2310,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("file",c,p,1),c,p,0,2362,2534,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        class=\"field file ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2456,2510,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("multiselect",c,p,1),c,p,0,2564,2765,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        multiple=true");_.b("\n" + i);_.b("        class=\"field multiselect ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2687,2741,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("docmultiselect",c,p,1),c,p,0,2805,3006,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <select ");_.b("\n" + i);_.b("        multiple=true");_.b("\n" + i);_.b("        class=\"field multiselect ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,2928,2982,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}if(_.s(_.f("textarea",c,p,1),c,p,0,3043,3221,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <textarea ");_.b("\n" + i);_.b("        class=\"field textarea ui-widget ui-corner-all\"");_.b("\n" + i);if(_.s(_.f("default_exists",c,p,1),c,p,0,3143,3197,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          data-field-default=\"");_.b(_.v(_.f("default",c,p,0)));_.b("\"");_.b("\n");});c.pop();}});c.pop();}_.b("    name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-field-subcategory=\"");_.b(_.v(_.f("subcategory",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-charseq=\"");_.b(_.v(_.f("charseq",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-label=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-order=\"");_.b(_.v(_.f("order",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-head=\"");_.b(_.v(_.f("head",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-required=\"");_.b(_.v(_.f("required",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-reversal=\"");_.b(_.v(_.f("reversal",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-name=\"");_.b(_.v(_.f("name",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-min=\"");_.b(_.v(_.f("min",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-max=\"");_.b(_.v(_.f("max",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-regex=\"");_.b(_.v(_.f("regex",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("text",c,p,1),c,p,0,3793,3803,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("integer",c,p,1),c,p,0,3827,3837,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("rational",c,p,1),c,p,0,3865,3875,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("date",c,p,1),c,p,0,3900,3910,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    />");_.b("\n");});c.pop();}if(_.s(_.f("boolean",c,p,1),c,p,0,3934,3965,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    /> Check for true<br />");_.b("\n");});c.pop();}if(_.s(_.f("openboolean",c,p,1),c,p,0,3996,4252,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);_.b("      <option value=\"null\" ");if(_.s(_.f("is_null",c,p,1),c,p,0,4042,4055,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b("></option>");_.b("\n" + i);_.b("      <option value=\"false\" ");if(_.s(_.f("is_false",c,p,1),c,p,0,4119,4132,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">False</option>");_.b("\n" + i);_.b("      <option value=\"true\" ");if(_.s(_.f("value",c,p,1),c,p,0,4198,4211,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">True</option>");_.b("\n" + i);_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("select",c,p,1),c,p,0,4282,4545,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4412,4516,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,4462,4475,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("docselect",c,p,1),c,p,0,4573,4836,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4703,4807,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,4753,4766,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("file",c,p,1),c,p,0,4862,5121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(!_.s(_.f("required",c,p,1),c,p,1,0,0,"")){_.b("      <option value=\"\" ");if(!_.s(_.f("default",c,p,1),c,p,1,0,0,"")){_.b("selected=true");};_.b("></option>");_.b("\n");};if(_.s(_.f("allowed",c,p,1),c,p,0,4992,5092,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5040,5053,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("key",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("multiselect",c,p,1),c,p,0,5149,5305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(_.s(_.f("allowed",c,p,1),c,p,0,5172,5276,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5222,5235,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("docmultiselect",c,p,1),c,p,0,5343,5499,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");_.b("\n" + i);if(_.s(_.f("allowed",c,p,1),c,p,0,5366,5470,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <option value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\" ");if(_.s(_.f("is_default",c,p,1),c,p,0,5416,5429,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("selected=true");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}_.b("    </select>");_.b("\n");});c.pop();}if(_.s(_.f("textarea",c,p,1),c,p,0,5534,5683,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    >");if(_.s(_.f("default",c,p,1),c,p,0,5552,5565,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("default",c,p,0)));});c.pop();}_.b("</textarea>");_.b("\n" + i);_.b("    <span title=\"Expand/Shrink Text Box\" class=\"expander\" data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"></span>");_.b("\n");});c.pop();}_.b("  </div>");_.b("\n");});c.pop();}return _.fl();;}),
  'fieldset' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div ");_.b("\n" + i);_.b("  data-group-id=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("  class=\"fields ui-widget ui-widget-content ui-corner-all padded\">");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,116,181,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <a href=\"#\" class=\"remove-button link-button\">Remove</a >");_.b("\n");});c.pop();}_.b("</div>");_.b("\n");return _.fl();;}),
  'index-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,362,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("            class=\"view-document-link\">");_.b(_.v(_.d(".",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,455,487,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'index-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table>");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <th>Name</th>");_.b("\n" + i);_.b("    <th>Doctype</th>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,91,210,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <th><a href=\"#\" data-index-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</a></th> ");_.b("\n" + i);_.b("      <td>");_.b(_.v(_.d("key.0",c,p,0)));_.b("</td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");return _.fl();;}),
  'index-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,27,74,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'paged-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<nav class=\"pager\">");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\" ");_.b("\n" + i);_.b("  title=\"Previous Page\"");_.b("\n" + i);_.b("  id=\"previous-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b(">Prev</a> ");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\"");_.b("\n" + i);_.b("  title=\"Next Page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b("  id=\"next-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);if(_.s(_.f("lastpage",c,p,1),c,p,0,322,351,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-last-page=\"true\"");_.b("\n");});c.pop();}if(_.s(_.f("lastrow",c,p,1),c,p,0,379,448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-startkey=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-startid=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b(">Next</a>");_.b("\n" + i);_.b("</nav>");_.b("\n" + i);_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,567,595,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("listed-element",c,p,"    "));});c.pop();}_.b("</table>");_.b("\n");return _.fl();;}),
  'preview-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,279,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'project-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr>");_.b("\n" + i);_.b("  <td><a href=\"/projects/");_.b(_.v(_.f("id",c,p,0)));_.b("/doctypes/main\">");if(_.s(_.f("doc",c,p,1),c,p,0,62,72,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("name",c,p,0)));});c.pop();}_.b("</a></td>");_.b("\n" + i);_.b("  <td>");if(_.s(_.f("doc",c,p,1),c,p,0,104,121,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.f("description",c,p,0)));});c.pop();}_.b("</td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <a href=\"/projects/");_.b(_.v(_.f("id",c,p,0)));_.b("/config\" ");_.b("\n" + i);_.b("       class=\"project-configure-button link-button\">Configure</a>");_.b("\n" + i);_.b("    <a href=\"#\" ");_.b("\n" + i);_.b("       class=\"project-delete-button link-button\" ");_.b("\n" + i);_.b("       id=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">Delete</button>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'project-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("rows",c,p,1),c,p,0,9,34,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("project-element",c,p,"  "));});c.pop();}return _.fl();;}),
  'search-field-item' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;}),
  'set-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: <span id=\"total-set-rows\">");_.b(_.v(_.f("total",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<div id=\"save-set-results\">");_.b("\n" + i);_.b("  <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table id=\"set-elements\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" id=\"select-all-set-elements\" title=\"Click to select or deselect all elements\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>");_.b("\n" + i);_.b("        Elements");_.b("\n" + i);_.b("      </th>");_.b("\n" + i);_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("elements",c,p,1),c,p,0,435,674,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" class=\"set-element-selection\" title=\"Click to select element\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <a class=\"view-document-link\" href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.f("context",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;}),
  'set-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("names",c,p,1),c,p,0,28,66,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.d(".",c,p,0)));_.b("\">");_.b(_.v(_.d(".",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'simple-to-form-array' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<ol>");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,17,51,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"    "));});c.pop();}_.b("</ol>");_.b("\n");return _.fl();;}),
  'simple-to-form-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li>");_.b("\n" + i);if(_.s(_.f("key",c,p,1),c,p,0,15,63,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <label for=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("key",c,p,0)));_.b("</label>");_.b("\n");});c.pop();}if(_.s(_.f("text",c,p,1),c,p,0,83,156,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <textarea ");if(_.s(_.f("key",c,p,1),c,p,0,106,122,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(">");_.b(_.v(_.f("value",c,p,0)));_.b("</textarea>");_.b("\n");});c.pop();}if(_.s(_.f("string",c,p,1),c,p,0,179,260,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"text\" ");if(_.s(_.f("key",c,p,1),c,p,0,211,227,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("bool",c,p,1),c,p,0,283,364,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"text\" ");if(_.s(_.f("key",c,p,1),c,p,0,315,331,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("number",c,p,1),c,p,0,387,470,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <input type=\"number\" ");if(_.s(_.f("key",c,p,1),c,p,0,421,437,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\"");});c.pop();}_.b(" value=\"");_.b(_.v(_.f("value",c,p,0)));_.b("\"/>");_.b("\n");});c.pop();}if(_.s(_.f("array",c,p,1),c,p,0,494,691,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("key",c,p,1),c,p,0,507,617,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <fieldset>");_.b("\n" + i);_.b("        <legend>");_.b(_.v(_.f("key",c,p,0)));_.b("</legend>");_.b("\n" + i);_.b(_.rp("simple-to-form-array",c,p,"        "));_.b("      </fieldset>");_.b("\n");});c.pop();}if(_.s(_.f("index",c,p,1),c,p,0,640,678,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-array",c,p,"      "));});c.pop();}});c.pop();}if(_.s(_.f("object",c,p,1),c,p,0,715,914,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("key",c,p,1),c,p,0,728,839,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <fieldset>");_.b("\n" + i);_.b("        <legend>");_.b(_.v(_.f("key",c,p,0)));_.b("</legend>");_.b("\n" + i);_.b(_.rp("simple-to-form-object",c,p,"        "));_.b("      </fieldset>");_.b("\n");});c.pop();}if(_.s(_.f("index",c,p,1),c,p,0,862,901,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-object",c,p,"      "));});c.pop();}});c.pop();}_.b("</li>");_.b("\n");return _.fl();;}),
  'simple-to-form-object' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<ul>");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,17,51,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"    "));});c.pop();}_.b("</ul>");_.b("\n");return _.fl();;}),
  'simple-to-form' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<form>");_.b("\n" + i);if(_.s(_.f("obj",c,p,1),c,p,0,17,110,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,44,86,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"        "));});c.pop();}_.b("    </ul>");_.b("\n");});c.pop();}_.b("</form>");_.b("\n");return _.fl();;}),
  'worksheet' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table id=\"worksheet-table\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr class=\"header-row\">");_.b("\n" + i);_.b("      <td id=\"select-all-worksheet-rows-cell\"");_.b("\n" + i);_.b("        class=\"select-column\">");_.b("\n" + i);_.b("        <input ");_.b("\n" + i);_.b("          id=\"select-all-worksheet-rows\"");_.b("\n" + i);_.b("          type=\"checkbox\"");_.b("\n" + i);_.b("          title=\"Click to select all rows\">");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,303,1494,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <th ");_.b("\n" + i);_.b("          class=\"worksheet-handle-header fieldset handle-column ");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <div>");_.b("\n" + i);_.b("            <span>");_.b("\n" + i);_.b("              <a class=\"fieldset-handle\" ");_.b("\n" + i);_.b("                data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("        </th>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,634,1476,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <th");_.b("\n" + i);_.b("            class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,681,689,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,716,724,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" worksheet-handle-header field handle-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <div>");_.b("\n" + i);_.b("              <span>");_.b("\n" + i);_.b("                <a class=\"field-handle\" ");_.b("\n" + i);_.b("                  data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                  href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("          </th>");_.b("\n" + i);_.b("          <th");_.b("\n" + i);_.b("            class=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <a class=\"field-header\" ");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("            <input ");_.b("\n" + i);_.b("              class=\"select-worksheet-column\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              type=\"checkbox\"");_.b("\n" + i);_.b("              title=\"Click to select column\">");_.b("\n" + i);_.b("          </td>");_.b("\n");});c.pop();}});c.pop();}_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);_.b("    <%#rows%>");_.b("\n" + i);_.b("      <tr id=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("        class=\"body-row\">");_.b("\n" + i);_.b("        <td class=\"select-column\">");_.b("\n" + i);_.b("          <input ");_.b("\n" + i);_.b("            class=\"select-worksheet-row\"");_.b("\n" + i);_.b("            data-row=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("            type=\"checkbox\"");_.b("\n" + i);_.b("            title=\"Click to select row\">");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,1865,2890,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <td class=\"");_.b(_.v(_.f("_id",c,p,0)));_.b(" fieldset handle-column\"></td>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1948,2870,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("            <td class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,1985,1993,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2020,2028,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" field handle-column\"></td>");_.b("\n" + i);_.b("            <td");_.b("\n" + i);_.b("              class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,2144,2152,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2179,2187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("              data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              <%#");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("                <%#multiple%>");_.b("\n" + i);_.b("                <ol>");_.b("\n" + i);_.b("                  <%#items%>");_.b("\n" + i);_.b("                    <li");_.b("\n" + i);_.b("                      data-field-fieldset_instance=\"<% fieldset_instance %>\"");_.b("\n" + i);_.b("                      data-field-field_instance=\"<% field_instance %>\"><% value %></li>");_.b("\n" + i);_.b("                  <%/items%>");_.b("\n" + i);_.b("                </ol>");_.b("\n" + i);_.b("                <%/multiple%>");_.b("\n" + i);_.b("                <%#single%>");_.b("\n" + i);_.b("                  <span><% value %></span>");_.b("\n" + i);_.b("                <%/single%>");_.b("\n" + i);_.b("              <%/");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("            </td>");_.b("\n");});c.pop();}});c.pop();}_.b("      </tr>");_.b("\n" + i);_.b("    <%/rows%>");_.b("\n" + i);_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;})
},
r = function(n) {
  var tn = t[n];
  return function(c, p, i) {
    return tn.render(c, p || t, i);
  }
};
module.exports = {
  templates : t,
  'changelog-element' : r('changelog-element'),
  'charseqs-element' : r('charseqs-element'),
  'config-maintenance' : r('config-maintenance'),
  'doctypes-element' : r('doctypes-element'),
  'document-edit' : r('document-edit'),
  'document-search-results' : r('document-search-results'),
  'document-search' : r('document-search'),
  'document-view-field' : r('document-view-field'),
  'document-view-tree' : r('document-view-tree'),
  'document-view' : r('document-view'),
  'fields' : r('fields'),
  'fieldset' : r('fieldset'),
  'index-element' : r('index-element'),
  'index-listing' : r('index-listing'),
  'index-options' : r('index-options'),
  'paged-listing' : r('paged-listing'),
  'preview-element' : r('preview-element'),
  'project-element' : r('project-element'),
  'project-listing' : r('project-listing'),
  'search-field-item' : r('search-field-item'),
  'set-listing' : r('set-listing'),
  'set-options' : r('set-options'),
  'simple-to-form-array' : r('simple-to-form-array'),
  'simple-to-form-field' : r('simple-to-form-field'),
  'simple-to-form-object' : r('simple-to-form-object'),
  'simple-to-form' : r('simple-to-form'),
  'worksheet' : r('worksheet')
};
},{"hogan.js":2}],"templates.js":[function(require,module,exports){
module.exports=require('mkFiG5');
},{}]},{},[4])