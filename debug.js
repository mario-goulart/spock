/* debug.js - debugging-support for SPOCK runtime */


SPOCK.debug = true;
SPOCK.toString = function() { return "#<SPOCK>"; };
SPOCK.restartCount = 0;
SPOCK.traceBuffer = [];
SPOCK.traceOutput = false;
SPOCK.traceHook = [];
SPOCK.hasAlert = SPOCK.inBrowser;

// Overrides SPOCK.error
SPOCK.error = function(msg) {	// "msg" may be a string or an error object
    var args = Array.prototype.splice.call(arguments, 1);
    var err;
    var text;

    if(typeof msg !== "string") { // an object?
	err = msg;
	msg = err.message;
    }

    text = msg;

    if(args.length > 1) 
	text += ":\n  " + SPOCK.map(SPOCK.stringify, args).join("\n  ");

    if(SPOCK.traceBuffer.length > 0) 
	text += "\n\nCall trace:\n\n" + SPOCK.getTrace();

    if(SPOCK.hasAlert) {
	alert("Error: " + text);
	SPOCK.hasAlert = false;	// disable to avoid endless repetition of alerts
    }
    else if(err) throw new (err.constructor)(text);
    else throw new Error(text);
};

// Overrides SPOCK.count
SPOCK.count = function(args, loc) {
    if(--SPOCK.stack <= 0) {
	++SPOCK.restartCount;
	return new SPOCK.Continuation(args.callee, Array.prototype.slice.call(args));
    }

    if(loc) SPOCK.trace(loc, args);

    return false;
};

SPOCK.trace = function(name, args) {
    var tb = SPOCK.traceBuffer;

    for(var i in SPOCK.traceHook) 
	(SPOCK.traceHook[ i ])(name, args);

    if(SPOCK.traceOutput)
	SPOCK.log("[" + SPOCK.stack + "] " + name);

    if(tb.length >= SPOCK.TRACELENGTH) tb.shift();

    tb.push([name, Array.prototype.slice.call(args, 1)]); // skip continuation argument
};

SPOCK.getTrace = function() {
    var tb = SPOCK.traceBuffer;
    var trace = [];

    for(var i in tb) {
	var e = tb[ i ];
	trace.push("  (" + e[ 0 ] + " " +  
		   SPOCK.map(SPOCK.stringify, e[ 1 ]).join(" ") + ")");
    }

    SPOCK.traceBuffer = [];
    return trace.join("\n") + "    <---";
};

// Overrides empty SPOCK.statistics
SPOCK.statistics = function() {
    //if(SPOCK.restartCount > 0)
    //   SPOCK.log("restarts:         ", SPOCK.restartCount);

    SPOCK.traceBuffer = [];
};

// Overrides SPOCK.callback
(function() {
    var old = SPOCK.callback;

    SPOCK.callback = function(proc) {
	var cb = old(proc);
	return function() {
	    var args = Array.prototype.slice.call(arguments);
	    SPOCK.trace("<callback>", args);
	    return cb.apply(this, args);
	};
    };
})();
