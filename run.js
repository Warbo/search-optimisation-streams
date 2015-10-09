var s = stringify = JSON.stringify;/*function(x) {
    if (typeof x === typeof [] && x.length !== undefined) return '['+x.map(stringify).toString()+']';
    if (typeof x === typeof []) return JSON.stringify(x);
    if (typeof x === typeof undefined) return 'undefined';
    
    return x.toString();
};*/

var p = function(x) {
    if (typeof x === 'function') print(x);
    else print(stringify(x));
};

var a = function(arg) {
    load('run.js');
    load('search.js');
    load('tests.js');
    p(tests(arg));
};

