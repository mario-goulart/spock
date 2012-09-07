/* runtime.js - SPOCK runtime (javascript part) */


SPOCK.modules = {};
SPOCK.symbolTable = {};
SPOCK.stack = 0;
SPOCK.limit = SPOCK.STACKSIZE;
SPOCK.debug = false;
SPOCK.running = false;
SPOCK.runHook = [];
SPOCK.inBrowser = "document" in this;
SPOCK.global = this;

SPOCK.Continuation = function(func, args) {
    this.k_callee = func;
    this.k_arguments = args;
};

SPOCK.Result = function(val) {
    this.value = val;
};

SPOCK.Symbol = function(name) {
    this.name = name;
    this.plist = {};
};

SPOCK.Pair = function(car, cdr) {
    this.car = car;
    this.cdr = cdr;
};

SPOCK.String = function(chars) {
    if(typeof chars === "string") {
	this.parts = [chars];
	this.length = chars.length;
    }
    else if(typeof chars === "number") this.parts = [chars.toString()];
    else this.parts = chars;	// assumes chars is array
};

SPOCK.Char = function(str) {
    this.character = str.charAt(0);
};

SPOCK.Port = function(direction, methods) {
    var port = this;
    var read = methods.read || function() {
	SPOCK.error("reading from non-input port", port);
    };

    function doread(n) {
	if(n === 0) return "";
	else if(this.peeked) {
	    var p = this.peeked;
	    this.peeked = false;

	    if(n === 1) return p;
	    else return p + read(n - 1);
	}
	else return read(n);
    }

    this.peeked = false;
    this.direction = direction;
    this.read = doread;
    this.write = methods.write || function() { 
	SPOCK.error("writing to non-output port", port) 
    };
    this.close = methods.close || function() {};
    this.flush = methods.flush || function() {};
    this.ready = methods.ready || function() { return true; };
    this.closed = false;
};

SPOCK.Promise = function(thunk) {
    this.thunk = thunk;
};

SPOCK.EndOfFile = function() {};
SPOCK.EOF = new SPOCK.EndOfFile();

SPOCK.check = function(val, type, loc) {
    if(typeof type === "function" && val instanceof type) return val;
    if(typeof val === type) return val;
    else SPOCK.error((loc ? "(" + loc + ") " : "") +
		     "bad argument type" +
		     (typeof type === "string" ? " - expected `" + type + "'" : ""),
		     val);
};

SPOCK.intern = function(str) {
    var old = SPOCK.symbolTable[ str ];

    if(old) return old;
    else return SPOCK.symbolTable[ str ] = new SPOCK.Symbol(str);
};

SPOCK.stringify = function(x, readable) {
    if(readable === undefined) readable = true;

    if(typeof x === "function") return "#<procedure>";
    else if(x === undefined) return "#<undefined>";
    else if(x === null) return "()";
    else if(x instanceof SPOCK.Continuation) return "#<continuation>";
    else if(x instanceof SPOCK.Symbol) return x.name;
    else if(x instanceof SPOCK.Pair) {
	var str = "(";
	var f = false;

	for(var p = x; p !== null && p instanceof SPOCK.Pair; p = p.cdr) {
	    if(f) str += " ";
	
	    str += SPOCK.stringify(p.car, readable);
	    f = true;
	}

	if(p !== null) str += " . " + SPOCK.stringify(p.cdr, readable);

	return str + ")";
    }
    else if(x instanceof Array) {
	var str = "#(";
	var f = false;

	for(var i in x) {
	    if(f) str += " ";
	
	    str += SPOCK.stringify(x[ i ], readable);
	    f = true;
	}

	return str + ")";
    }
    else if(x instanceof SPOCK.String) {
	if(readable)
	    return "\"" + x.normalize() + "\""; // XXX does not escape embedded characters
	else return x.normalize();
    }
    else if(x instanceof SPOCK.Char) {
	if(readable) return x.character;

	switch(x.character) {
	case "\n": return "#\\newline";
	case "\t": return "#\\tab";
	case "\r": return "#\\return";
	case " ": return "#\\space";
	default: return "#\\" + x.character;
	}
    }
    else if(x instanceof SPOCK.Port)
	return "#<" + x.direction + " port" + 
	    (x.name ? (" \"" + x.name + "\">") : ">");
    else if(x instanceof SPOCK.Promise) return "#<promise>";
    else if(x instanceof SPOCK.EndOfFile) return "#<eof>";
    else return x.toString();
};

SPOCK.error = function(msg) {
    var args = Array.prototype.splice.call(arguments, 1);

    function argstr(x) {
	return SPOCK.stringify(x, true);
    }

    if(args.length > 0)
	msg = msg + ":\n  " + SPOCK.map(argstr, args).join("\n  ");

    throw new Error(msg);
};

if(this.quit) SPOCK.exit = quit;
else SPOCK.exit = function(code) { 
	SPOCK.error("no suitable primitive available for `exit'");
    };

SPOCK.String.prototype.normalize = function() {
    if(this.parts.length === 0) return "";

    this.parts = [this.parts.join("")];
    return this.parts[ 0 ];
};

SPOCK.jstring = function(x) {
    if(typeof x === "string") return x;
    else if(x instanceof SPOCK.String) return x.normalize();
    else return x;
};

SPOCK.list = function() {
    var lst = null;
    var len = arguments.length;

    for(var i = len - 1; i >= 0; --i)
	lst = new SPOCK.Pair(arguments[ i ], lst);

    return lst;
};

SPOCK.length = function(lst) {
    for(var n = 0; lst instanceof SPOCK.Pair; ++n)
	lst = lst.cdr;

    return n;
};

SPOCK.map = function(func, array) {
    var len = array.length;
    var a2 = new Array(len);

    for(var i in array)
	a2[ i ] = func(array[ i ]);

    return a2;
};

SPOCK.eqvp = function(x, y) {
    if(x === y) return true;
    else if(x instanceof SPOCK.Char) 
	return y instanceof SPOCK.Char && x.character === y.character;
    else return false;
};

SPOCK.equalp = function(x, y) {
    if(x === y) return true;
    else if(x instanceof SPOCK.Pair)
	return y instanceof SPOCK.Pair &&
	    SPOCK.equalp(x.car, y.car) &&
	    SPOCK.equalp(x.cdr, y.cdr);
    else if(x instanceof Array) {
	var len = x.length;
	if(!(y instanceof Array) || y.length != len) return false;
	for(var i = 0; i < len; ++i) {
	    if(!SPOCK.equalp(x[ i ], y[ i ])) return false;
	}
	return true;
    }
    else if(x instanceof SPOCK.Char) 
	return y instanceof SPOCK.Char && x.characters === y.characters;
    else if(x instanceof SPOCK.String) {
	var s1 = x.normalize();

	if(y instanceof SPOCK.String) return s1 === y.normalize();
	else if(typeof y === 'string') return s1 === y;
	else return false;
    }
    else if(typeof x === 'string') {
	if(y instanceof SPOCK.String) return x === y.normalize();
	else if(typeof y === 'string') return x === y;
	else return false;
    }
    else return false;
};

SPOCK.count = function(args, loc) {
    if(--SPOCK.stack <= 0) 
	return new SPOCK.Continuation(args.callee, Array.prototype.slice.call(args));
    else return false;
};

SPOCK.rest = function(args, count, loc) {
    var rest = null;
    
    // this will not unwind, but decrease the counter
    SPOCK.count(args, loc);

    for(var i = args.length - 1; i >= count; --i)
	rest = new SPOCK.Pair(args[ i ], rest);

    return rest;
};

SPOCK.statistics = function() {};

SPOCK.run = function(func) {	// optional arguments
    function terminate(result) {
	return new SPOCK.Result(result);
    }

    var k = terminate;
    var args = [k].concat(Array.prototype.slice.call(arguments, 1));
    var oldstack = SPOCK.stack;
    var oldlimit = SPOCK.limit;
    var oldrunning = SPOCK.running;
    SPOCK.limit = Math.max(10, oldlimit - oldstack);
    SPOCK.stack = SPOCK.limit;
    SPOCK.running = true;

    function restore() {
	SPOCK.stack = oldstack;
	SPOCK.limit = oldlimit;
	SPOCK.running = oldrunning;

	if(!oldrunning) {
	    for(var i in SPOCK.runHook)
		(SPOCK.runHook[ i ])(false);
	}
    }

    var result;

    if(!oldrunning) {
	for(var i in SPOCK.runHook)
	    (SPOCK.runHook[ i ])(true);
    }

    while(true) {
	result = func.apply(SPOCK.global, args);

	if(result instanceof SPOCK.Continuation) {
	    SPOCK.stack = SPOCK.STACKSIZE;
	    func = result.k_callee;
	    args = result.k_arguments;
	}
	else if(result instanceof SPOCK.Result) {
	    restore();
	    return result.value;
	}
	else {
	    restore();
	    SPOCK.error("unexpected return of non-continuation", result);
	}
    }

    return result;
};

SPOCK.callback = function(proc) {
    return function() {
	var args = Array.prototype.slice.call(arguments);
	args.unshift(proc);
	return SPOCK.run.apply(this, args);
    };
};

SPOCK.callbackMethod = function(proc) {
    var g = this;
    return function() {
	var args = Array.prototype.slice.call(arguments);
	args.unshift(this);
	args.unshift(proc);
	return SPOCK.run.apply(g, args);
    };
};

SPOCK.go = function(proc) {
    (SPOCK.callback(proc))();
};

if("java" in this) {				    // rhino
    SPOCK.makeJavaInputPort = function(jp) {
	return new SPOCK.Port("input", {
		read: function(n) {
		    var buffer = ""; 

		    while(n--) {
			var b = jp.read();
			
			if(b === -1) break;
			else buffer += String.fromCharCode(b);
		    }

		    return buffer === "" ? SPOCK.EOF : buffer;
		},

		close: function() { jp.close(); }
	    });
    };
    
    SPOCK.makeJavaOutputPort = function(jp) {
	return new SPOCK.Port("output", {
		write: function(s) {
		    var len = s.length;

		    for(var i = 0; i < len; ++i)
			jp.write(s.charCodeAt(i));
		},

		flush: function() { jp.flush(); },
		close: function() { jp.close(); }
	    });
    };

    SPOCK.log = function() {
	java.lang.System.err.println(Array.prototype.slice.call(arguments).join(""));
    };

    SPOCK.stdin = SPOCK.makeJavaInputPort(java.lang.System[ "in" ]);
    SPOCK.stdout = SPOCK.makeJavaOutputPort(java.lang.System.out);
    SPOCK.stderr = SPOCK.makeJavaOutputPort(java.lang.System.err);
    SPOCK.stderr.name = "[stderr]";
}
else {
    if("console" in this) SPOCK.log = console.log; // firebug
    else if(SPOCK.inBrowser)       // inside browser
	SPOCK.log = function() {
	    var msg = arguments.join(" ");
	
	    if(msg.charAt(msg.length - 1) == "\n")
		msg = msg.substring(0, msg.length - 1);

	    this.defaultStatus = msg;
	};
    else if("print" in this) SPOCK.log = print; // spidermonkey/v8
    else SPOCK.error("no suitable output primitive available");

    (function() {
	var buffer = [];

	function flush() {
	    if(buffer.length > 0) {
		SPOCK.log(buffer.join(""));
		buffer = [];
	    }
	}

	function write(s) {
	    var parts = SPOCK.stringify(s, false).split("\n");
	    var len = parts.length - 1;

	    if(len > 0) {		// contains newline?
		buffer.push(parts[ 0 ]);
		flush();

		if(len > 1) {
		    for(var i = 1; i < len; ++i)
			SPOCK.log(parts[ i ]);
		}

		buffer.push(parts[ len ]);
	    }
	    else buffer.push(parts[ 0 ]);
	}

	SPOCK.stdout = new SPOCK.Port("output", { write: write, flush: flush });
	var inp;
	var ibuffer = "";

	if(this.prompt) {
	    inp = function(n) {
		while(true) {
		    if(ibuffer.length <= n) {
			var part = ibuffer.slice(0, n);
			ibuffer = ibuffer.slice(n);
			return part;
		    }

		    var input = prompt("Expecting input for " + this.toString());
	    
		    if(input === null) return SPOCK.EOF;
		    else ibuffer += input;
		}
	    };
	}
	else {
	    inp = function(n) {
		SPOCK.error("no input possible for standard input port");
	    };
	}

	SPOCK.stdin = new SPOCK.Port("input", { read: inp });
	SPOCK.stderr = SPOCK.stdout;
    })();
}

SPOCK.stdin.name = "[stdin]";
SPOCK.stdout.name = "[stdout]";

SPOCK.flush = function() {
    // note that this always prints a newline when console.log or print is used
    SPOCK.stdout.flush();
    
    if(SPOCK.stderr !== SPOCK.stdout)
	SPOCK.stderr.flush();

    SPOCK.statistics();
};

if(this.gc) SPOCK.gc = gc;
else SPOCK.gc = function() {};

SPOCK.openInputUrlHook = function(url) {
    SPOCK.error("can not open", url);
};

SPOCK.openOutputUrlHook = function(url) {
    SPOCK.error("can not open", url);
};

if("java" in this) {
    SPOCK.openInputFile = function(filename) {
	var stream;

	try {
	    stream = new java.io.FileInputStream(filename);
	}
	catch(e) {
	    SPOCK.error(e.message);
	}

	var port = SPOCK.makeJavaInputPort(stream);
	port.name = filename;
	return port;
    };

    SPOCK.openOutputFile = function(filename) {
	var stream;

	try {
	    stream = new java.io.FileOutputStream(filename);
	}
	catch(e) {
	    SPOCK.error(e.message);
	}

	var port = SPOCK.makeJavaOutputPort(stream);
	port.name = filename;
	return port;
    };

    SPOCK.fileExists = function(filename) {
	return (new java.io.File(filename)).exists();
    };
}
else {
    if(SPOCK.inBrowser) {
	SPOCK.openInputFile = function(filename) {
	    if(filename.match(/^[a-z0-9]+:/)) 
		return SPOCK.openInputUrlHook(filename);
	    
	    var cookies = document.cookie.split("; ");
	    var buffer = null;
	    
	    for(var i = 0; i < cookies.length; ++i) {
		var c = cookies[ i ];
		var p = c.indexOf("=");
		
		if(filename === c.substring(0, p)) {
		    buffer = c.substring(p + 1);
		    break;
		}
	    }

	    if(!buffer) SPOCK.error("can not open file", filename);
	    
	    var pos = 0;
	    
	    return new SPOCK.Port("input", {
		    read: function(n) {
			if(pos >= buffer.length) return SPOCK.EOF;
			else if(pos + len >= buffer.length) 
			    return buffer.substring(pos);
			
			var p1 = pos;
			pos += n;
			return buffer.substring(p1, p1 + n);
		    },
			
			ready: function() { return pos < buffer.length; }
	    });
	};

	SPOCK.openOutputFile = function(filename, expiry) {
	    if(filename.match(/^[a-z0-9]+:/)) 
		return SPOCK.openOutputUrlHook(filename);

	    return new SPOCK.Port("output", {
		    write: function(s) { buffer += s; },
			close: function() {
			var now = (new Date()).getTime();
			var exp = now + (expiry || (1000 * 60 * 60 * 24 * 365));
			document.cookie = filename + "=" + encodeURIComponent(buffer) +
			    "; expires=" + (new Date(exp)).toGMTString();
		    }
	    });
	};
    }
    else {
	SPOCK.openInputFile = function(filename) {
	    SPOCK.error("file-I/O not available");
	}

	SPOCK.openOutputFile = function(filename) {
	    SPOCK.error("file-I/O not available");
	}
    }

    SPOCK.fileExists = function(filename) {
	SPOCK.error("`file-exists?' not available");
    };
}

if("document" in this) {	// browser?
    SPOCK.load = function(url, k) {
	// http://www.nczonline.net/blog/2009/07/28/the-best-way-to-load-external-javascript/
	var script = document.createElement("script")
	
	script.type = "text/javascript";
	k = k || function() {};

	if (script.readyState){  //IE
	    script.onreadystatechange = function(){
		if (script.readyState == "loaded" || script.readyState == "complete"){
		    script.onreadystatechange = null;
		    k(url);
		}
	    };
	} 
	else {  //Others
	    script.onload = function(){
		k(url);
	    };
	}

	script.src = url;
	document.getElementsByTagName("head")[0].appendChild(script);
    };
}
else if("load" in this) {	// rhino/SM
    SPOCK.load = function(filename, k) {
	load(filename);
	
	if(k) k(filename);
    };
}
