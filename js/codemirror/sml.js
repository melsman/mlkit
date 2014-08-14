// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// SML mode by Ken Friis Larsen

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";




CodeMirror.defineMode('sml', function(_config, parserConfig) {
  var keywords = {
      'abstype': 'keyword',
      'and': 'keyword',
      'andalso': 'keyword',
      'as': 'keyword',
      'case': 'keyword',
      'datatype': 'keyword',
      'do': 'keyword',
      'else': 'keyword',
      'eqtype': 'keyword',
      'end': 'keyword',
      'exception': 'keyword',
      'fn': 'keyword',
      'fun': 'keyword',
      'functor': 'keyword',
      'handle': 'keyword',
      'if': 'keyword',
      'in': 'keyword',
      'include': 'keyword',
      'infix': 'keyword',
      'infixr': 'keyword',
      'let': 'keyword',
      'local': 'keyword',
      'nonfix': 'keyword',
      'of': 'keyword',
      'op': 'keyword',
      'open': 'keyword',
      'orelse': 'keyword',
      /* Moscow ML specific exclude for now */
      // 'prim_eqtype': 'keyword',
      // 'prim_EQtype': 'keyword',
      // 'prim_type': 'keyword',
      // 'prim_val': 'keyword',
      'raise': 'keyword',
      'rec': 'keyword',
      'sharing': 'keyword',
      'sig': 'keyword',
      'signature': 'keyword',
      'struct': 'keyword',
      'structure': 'keyword',
      'then': 'keyword',
      'type': 'keyword',
      'val': 'keyword',
      'where': 'keyword',
      'while': 'keyword',
      'with': 'keyword',
      'withtype': 'keyword',
      /* We leave out symbolic keywords for now */
      // '#': 'keyword',
      // '->': 'keyword',
      // '|': 'keyword',
      // ':>': 'keyword',
      // ':': 'keyword',
      // '=>': 'keyword',
      // '=': 'keyword'
  };

  function tokenBase(stream, state) {
    var ch = stream.next();

    if (ch === '"') {
      state.tokenize = tokenString;
      return state.tokenize(stream, state);
    }
    if (ch === '(') {
      if (stream.eat('*')) {
        state.commentDepth++;
        state.tokenize = tokenComment;
        return state.tokenize(stream, state);
      }
      return null;
    }
    if (ch === '~') {
        if (stream.peek() === '0') {
            stream.next(); // drop '0'
            if (stream.peek() === 'x' ) {
                stream.next();  // drop 'x' 
                if (stream.eatWhile(/[A-Fa-f0-9]/)) {
                    return 'number';
                }
                return 'error';
            }
            if (stream.peek() === 'w') {
                stream.next();  // drop 'w'
                if (stream.peek() === 'x' ) {
                    stream.next();  // drop 'x' 
                    if (stream.eatWhile(/[A-Fa-f0-9]/)) {
                        return 'number';
                    }
                } else if (stream.eatWhile(/[\d]/)) {
                    return 'number';
                }
                return 'error';
            }
            stream.eatWhile(/[\d]/);
            if (stream.eat('.')) {
                stream.eatWhile(/[\d]/);
            }
            return 'number';
        }
        if (stream.eatWhile(/[\d]/)) {
            if (stream.eat('.')) {
                stream.eatWhile(/[\d]/);
            }
            if (stream.eat(/[eE]/)) {
                stream.eat('~');
                stream.eatWhile(/[\d]/);
            }
            return 'number';
        }
        return null;
    }
    if (ch === '0' && stream.peek() === 'x' ) {
        stream.next();  // drop 'x' 
        if (stream.eatWhile(/[A-Fa-f0-9]/)) {
            return 'number';
        }
        return 'error';
    }
    if (ch === '0' && stream.peek() === 'w') {
        stream.next();  // drop 'w'
        if (stream.peek() === 'x' ) {
            stream.next();  // drop 'x' 
            if (stream.eatWhile(/[A-Fa-f0-9]/)) {
                return 'number';
            }
        } else if (stream.eatWhile(/[\d]/)) {
            return 'number';
        }
        return 'error';
    }
    if (/\d/.test(ch)) {
      stream.eatWhile(/[\d]/);
      if (stream.eat('.')) {
        stream.eatWhile(/[\d]/);
      }
      if (stream.eat(/[eE]/)) {
          stream.eat('~');
          stream.eatWhile(/[\d]/);
      }
      return 'number';
    }

    if (ch === '*') {
      if (stream.eat(')')) {
        return 'error';
      }
      return null;
    }

    if (ch === '#') {
        var peek = stream.peek();
        if (peek === '"') {
            stream.next();
            state.tokenize = tokenString;
            return state.tokenize(stream, state);
        } else if (/\d/.test(peek)) {
            stream.eatWhile(/[\d]/);
            return 'keyword';
        }
        return null;
    } 

    // if ( /[+\-*&%=<>!?|]/.test(ch)) {
    //   return 'operator';
    // }

    stream.eatWhile(/\w/);
    var cur = stream.current();
    return keywords[cur] || 'variable';
  }

  function tokenString(stream, state) {
    var next, end = false, escaped = false, errors = false;
    while ((next = stream.next()) != null) {
      if (next === '"' && !escaped) {
        end = true;
        break;
      }
      escaped = !escaped && next === '\\';
    }
    if (end && !escaped) {
      state.tokenize = tokenBase;
    }

    errors = (!end) || errors;

    return 'string'+(errors ? ' error' : '');
  };

  function tokenComment(stream, state) {
    var prev, next;
    while(state.commentDepth > 0 && (next = stream.next()) != null) {
      if (prev === '(' && next === '*') state.commentDepth++;
      if (prev === '*' && next === ')') state.commentDepth--;
      prev = next;
    }
    if (state.commentDepth <= 0) {
      state.tokenize = tokenBase;
    }
    return 'comment';
  }

  return {
    startState: function() {return {tokenize: tokenBase, 
                                    commentDepth: 0};},
    token: function(stream, state) {
      if (stream.eatSpace()) return null;
      return state.tokenize(stream, state);
    },

    blockCommentStart: "(*",
    blockCommentEnd: "*)",
  };
});

CodeMirror.defineMIME('text/x-sml', {
    name: 'sml'
});


});
