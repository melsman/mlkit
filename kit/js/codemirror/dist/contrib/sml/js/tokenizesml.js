/* Tokenizer for SML code. 
 *
 * This tokenizer is a simplification of the C# tokenizer, with
 * special support for nested comments as supported by Standard ML.
 */

var tokenizeSML = (function() {
  // Advance the stream until the given character (not preceded by a
  // backslash) is encountered, or the end of the line is reached.
  function nextUntilUnescaped(source, end) {
    var escaped = false;
    var next;
    while (!source.endOfLine()) {
      var next = source.next();
      if (next == end && !escaped)
        return false;
      escaped = !escaped && next == "\\";
    }
    return escaped;
  }

  // A map of keywords. The style information included in these
  // objects is used by the highlighter to pick the correct CSS style
  // for a token.
  var keywords = function(){
    var keyword = {type: "keyword", style: "sml-keyword"};
    return {"abstype": keyword, "and": keyword, "andalso": keyword,
	    "as": keyword, "case": keyword, "do": keyword,
	    "datatype": keyword, "else": keyword, "end": keyword,
	    "eqtype": keyword, "exception": keyword, "fn": keyword,
	    "fun": keyword, "functor": keyword, "handle": keyword,
	    "if": keyword, "in": keyword, "include": keyword, "infix": keyword,
	    "infixr": keyword, "let": keyword, "local": keyword, "nonfix": keyword,
	    "of": keyword, "op": keyword, "open": keyword, "orelse": keyword,
	    "raise": keyword, "rec": keyword, "sharing": keyword, "sig": keyword,
	    "signature": keyword, "struct": keyword, "structure" : keyword,
	    "then": keyword, "type": keyword, "val": keyword, "where": keyword,
	    "with": keyword, "withtype": keyword, "while": keyword
	    };
  }();

  // Some helper regexps
  var isOperatorChar = /[$/~+\-*&%=<>!?|#@^:]/;
  var isHexDigit = /[0-9A-Fa-f]/;
  var isWordChar = /[\w_]/;

  // Wrapper around jsToken that helps maintain parser state (whether
  // we are inside of a multi-line comment and whether the next token
  // could be a regular expression).
  function jsTokenState(inside, insidelevel, regexp) {
    return function(source, setState) {
      var newInside = inside;
      var newInsideLevel = insidelevel;
      var setLevel = function(lev) { newInsideLevel = lev; }
      var getLevel = function() { return newInsideLevel; }
      var setInside = function(c) {
	  if (newInside == "(*") {
	      if (c == null) {
		  if (newInsideLevel <= 0) {
		      newInside = null;
		  }
	      }
	  } else {
	      newInside = c;
	  }
      }
      var type = jsToken(inside, getLevel, regexp, source, setInside, setLevel);
      var newRegexp = type.type == "operator" || type.type.match(/^[\[{}\(,;]$/);
      if (newRegexp != regexp || newInside != inside || newInsideLevel != insidelevel)
	  setState(jsTokenState(newInside, newInsideLevel, newRegexp));
      return type;
    };
  }

  // The token reader, inteded to be used by the tokenizer from
  // tokenize.js (through jsTokenState). Advances the source stream
  // over a token, and returns an object containing the type and style
  // of that token.
  function jsToken(inside, getLevel, regexp, source, setInside, setLevel) {
    function readHexNumber(){
      source.next(); // skip the 'x'
      source.nextWhileMatches(isHexDigit);
      return {type: "number", style: "sml-atom"};
    }

    function readInteger() {
      source.nextWhileMatches(/[0-9]/);
      return {type: "number", style: "sml-atom"};
    }
    function readNumber() {
      source.nextWhileMatches(/[0-9]/);
      if (source.equals(".")){
        source.next();
        source.nextWhileMatches(/[0-9]/);
      }
      if (source.equals("e") || source.equals("E")){
        source.next();
        if (source.equals("~"))
          source.next();
        source.nextWhileMatches(/[0-9]/);
      }
      return {type: "number", style: "sml-atom"};
    }
    // Read a word, look it up in keywords. If not found, it is a
    // variable, otherwise it is a keyword of the type found.
    function readWord() {
      source.nextWhileMatches(isWordChar);
      var word = source.get();
      var known = keywords.hasOwnProperty(word) && keywords.propertyIsEnumerable(word) && keywords[word];
      return known ? {type: known.type, style: known.style, content: word} :
      {type: "variable", style: "sml-variable", content: word};
    }
    function readRegexp() {
      nextUntilUnescaped(source, "/");
      source.nextWhileMatches(/[gi]/);
      return {type: "regexp", style: "sml-string"};
    }
    // Mutli-line comments are tricky. We want to return the newlines
    // embedded in them as regular newline tokens, and then continue
    // returning a comment token for every line of the comment. So
    // some state has to be saved (inside) to indicate whether we are
    // inside a (* *) sequence.
    function readMultilineComment(start,level){
      var newInside = "(*";
      var maybeEnd = (start == "*");
      var maybeNew = (start == "(");
      while (true) {
        if (source.endOfLine())
          break;
        var next = source.next();
        if (next == ")" && maybeEnd){
	    setLevel(level-1);
	    newInside = null;
	    break;
        }
	if (next == "*" && maybeNew){
	    setLevel(level+1);
	    break;
	}
	maybeNew = (next == "(");
        maybeEnd = (next == "*");
      }
      setInside(newInside);
      return {type: "comment", style: "sml-comment"};
    }
    function readOperator() {
      source.nextWhileMatches(isOperatorChar);
      return {type: "operator", style: "sml-operator"};
    }
    function readString() {
      var endBackSlash = nextUntilUnescaped(source, "\"");
      setInside(endBackSlash ? "\"" : null);
      return {type: "string", style: "sml-string"};
    }

    // Fetch the next token. Dispatches on first character in the
    // stream, or first two characters when the first is a slash.
    if (inside == "\"")
      return readString();
    var ch = source.next();
    if (inside == "(*")
	return readMultilineComment(ch,getLevel());
    else if (ch == "\"")
      return readString();
    else if (ch == "(" && source.equals("*")) { 
	return readMultilineComment(ch,0); 
    } 
    // with punctuation, the type of the token is the symbol itself
    else if (/[\[\]{}\(\),;\.]/.test(ch))
      return {type: ch, style: "sml-punctuation"};
    else if (ch == "0") {
	if (source.equals("x")) {
	    return readHexNumber();
	} else if (source.equals("w")) {
	    source.next();
	    if (source.equals("x")) {
		return readHexNumber();
	    }
	    return readInteger();
	}
	return readNumber();
    }		
    else if (/[0-9]/.test(ch))
      return readNumber();
    else if (ch == "#" && source.equals("\"")) {
	source.next();
        return readString();
    }
    else if (isOperatorChar.test(ch))
      return readOperator();
    else
      return readWord();
  }

  // The external interface to the tokenizer.
  return function(source, startState) {
      return tokenizer(source, startState || jsTokenState(false, 0, true));
  };
})();
