var curry_test;
var tests = function(num) {
    if (typeof num === typeof undefined) num = +Infinity;
    var ts = [
        curry_test = function() {
	    // Use uncurried take and map, since we're testing curry!
            var take = function(n, stream) {
		var result = [];
		for (var i = 0; i < n; i++) result.push(stream());
		return result;
	    };
	    var map = function(f, comb) {
		return function() {
		    return f(comb());
		};
	    };
	    var args;
	    var start = 'var a = c(function(';
	    var mid = ') { return ';
	    var end = '; })';
	    var s;
	    var i = 1;
	    return function() {
		args = take(i, map(
		    function(x) { return take(x+1, constant('b')).join(''); },
		    counter()
		));
		s = start + args.join(', ') + mid + args.join(' + ') + end;
		eval(s);
		var f = a;
		for (var j = 1; j < args.length; j++) f = f(1);
                var val = f(1);
		if (
		    typeof val !== typeof 1 ||
		    val != args.length
		) return 'curry';
		i = i+1 % 1000;
	    };
	},  // curry
	function() {
	    var summer = function() {
            var result = 0;
            for (var i = 0; i < arguments.length; i++) result += arguments[i];
	        return result;
            };
	    var i = 0;
	    return function() {
		var args = [];
		for (var j = 0; j < i; j++) args.push(1);
		var val = apply(summer, args);
		if (
		    typeof val !== typeof 0 ||
	            val !== i
		) return 'apply';
	    };
	},  // apply
	function() {
	    var i = 0;
	    return function() {
		var a = Math.random() * i;
		var b = Math.random() * i;
		if (plus(a, b) !== a + b) return 'plus';
		i++;
	    };
	},  // plus
	function() {
	    var i = 0;
	    return function() {
		var a = Math.random() * i;
		if (negate(a) !== a * -1) return 'negate';
		i++;
	    };
	},  // negate
	function() {
	    var i = 0;
	    return function() {
		var a = Math.random() * i;
		var b = Math.random() * i;
		if (multiply(a, b) !== a * b) return 'multiply';
		i++;
	    };
	},  // multiply
	function() {
	    var i = 1;
	    return function() {
		var a = Math.random() * i;
		var b = (1 - Math.random()) * i;  // Avoids division by zero
		if (divide(a, b) !== a / b) return 'divide';
		i++;
	    };
	},  // divide
	function() {
	    var i = 1;
	    return function() {
		var a = Math.ceil(Math.random() * i);
		var b = Math.ceil((1 - Math.random()) * i);
		if (modulus(a, b) !== a % b) return 'modulus';
		i++;
	    };
	},  // modulus
	function() {
	    var i = 1;
	    return function() {
		var arr = take(i, counter());
		for (var j = 0; j < i; j++) {
		    if (
			subscript(arr, j) !== arr[j] ||
			subscript(arr, j) !== j
		    ) return 'subscript';
		}
		i++;
	    };
	},  // subscript
	function() {
	    var i = 0;
	    return function() {
		var a = take(i, Math.random);
		var b = take(i, Math.random);
		if (!array_compare(catenate(a, b), a.concat(b))) return 'catenate';
		i++;
	    };
	},  // catenate
	function() {
	    var i = 1;
	    return function() {
		var head = Math.random();
		var tail = take(i, Math.random);
		if (!array_compare(cons(head, tail), [head, tail])) return 'cons';
	    };
	},  // cons
	function() {
	    return function() {
		var bit = Math.round(Math.random());
		if (choose_direction(bit) !== (2 * bit) - 1) return 'choose_direction';
	    };
	},  // choose_direction
	function() {
	    var i = 1;
	    return function() {
		var base = Math.ceil((1.1 - Math.random()) * i);
		var arr = take(i, map(Math.floor, map(multiply(base), Math.random)));
		var result = 0;
		for (var j = 0; j < arr.length; j++) result = (base * result) + arr[j];
                while (arr.length > 1 && arr[0] === 0) arr.shift();
		if (!array_compare(rebase(result, base), arr)) return 'rebase';
		i = (i + 1) % 13 + 1;  // "result" loses precision once we hit about 13^13
	    };
	},  // rebase
	function() {
	    var i = 0;
	    return function() {
		var arr = take(i, zip_with(cons, product(plus, counter(), Math.random), Math.random));
		var limit = Math.round(Math.random() * i);
		var pruned = tagged_pruner(arr, limit);
		while (arr.length > limit) arr.shift();
		if (!array_compare(pruned, arr)) return 'tagged_pruner';
	        i++;
	    };
	},  // tagged_pruner
	function() {
	    var i = 0;
	    return function() {
		var arr = [];
		for (var j = 0; j < Math.round(i*(1+Math.random())); j++) arr.push(Math.random());
		var amount = Math.round(Math.random()*i);
		var k = 0;
		var taken = take(amount, function() { return arr[k++]; });
		while (arr.length > amount) arr.pop();
		if (!array_compare(arr, taken)) return 'take';
		i++;
	    };
	},  // take
	function() {
	    var dimensions = 2;
	    return function() {
		var coords = take(dimensions, Math.random);
		var length = 0;
		coords.forEach(function(x) { length = Math.sqrt(length*length + x*x); });
		coords.map(function(x) { return x / length; });
	    };
	},  // cartesian_from_polar
	function() {
	    var i = 0;
	    return function() {
		var val = Math.random();
		var arr = [];
		for (var j = 0; j < i; j++) arr.push(val);
		if (!array_compare(times(i, val), arr)) return 'times';
		i++;
	    };
	},  // times
	function() {
	    var i = 0;
	    return function() {/*
		var arr = take(Math.pow(2, i)+(3*i), delayer(take(i, random_bits()), zeros()));
		var m = {pc: 0, mem: arr, w: 2};
		var w = Math.max(1, Math.round(Math.random()*i));
		var result = bitbitjump_step()
		if (!array_compare())
		i++;
	    */};
	},  // bitbitjump_step
	function() {
	    return function() {
		
	    };
	},  // bbj_io
	function() {
	    return function() {
		
	    };
	},  // bbj_outputter*/
        function() {
	    var con;
	    var i = 0;
	    return function() {
                con = constant(i);
		for (var j = 0; j < 10; j++) {
		    if (con() !== i) return 'constant';
		}
		i++;
            };
	},  // constant
        function() {
	    var z = zeros();
            return function() {
                if (z() !== 0) return 'zeros';
            };
	},  // zeros
        function() {
            var o = ones();
            return function() {
                if (o() !== 1) return 'ones';
            };
        },  // ones
	function() {
	    var stream = ones();
	    var reducer = reduce(0, plus, stream);
	    var i = 0;
            return function () {
		if (reducer() !== i) return 'reduce';
		i++;
	    };
	},  // reduce
	function() {
	    var count = counter();
	    var i = 0;
	    return function() {
		if (count() !== i) return 'counter';
		i++;
	    };
	},  // counter
	function() {
	    var h = hoard(counter());
	    var arr = [];
	    var t;
	    var i = 0;
	    return function () {
		t = h();
		for (var j = 0; j < arr.length; j++) {
		    if (arr[j] !== t[j]) return 'hoard';
		}
		arr.push(i);
		i++;
	    };
	},  // hoard
	function() {
	    var arr = take(500, Math.random);
	    var d = delayer(arr, counter());
	    var i = 0;
	    return function() {
		if (i < 500 && d() !== arr[i]) return 'delayer';
		if (i >= 500 && d() !== i - 500) return 'delayer';
		i++;
	    };
	},  // delayer
	function() {
	    var d = delay(-1, counter());
	    var i = -1;
	    return function() {
		if (d() !== i) return 'delay';
		i++;
	    };
	},  // delay
	function() {
	    var m = map(multiply(2), counter());
	    var i = 0;
	    return function() {
		if (m() !== 2*i) return 'map';
		i++;
	    };
	},  // map
	function() {
	    var i = iterate(plus(1), 0);
	    var j = 1;
	    return function() {
		if (i() !== j) return 'iterate';
		j++;
	    };
	},  // iterate
        function() {
	    var s = simple();
            var length = 0;
            var val = -Infinity;
	    var temp;
            return function() {
                temp = s();
                if (temp.length <= length && parseInt(temp.join(''), 2) <= val) return 'simple';
                length = temp.length;
                val = parseInt(temp.join(''), 2);
            };
	},  // simple
	function() {
	    var f = fast([0, 1], bbj_outputter());
	    var val;
            return function() {
		val = f();
		if (typeof val !== typeof []) return 'fast';
		for (var j = 0; j < val.length; j++) {
		    if (val[j] !== 0 && val[j] !== 1) return 'fast';
		}
	    };
	},  // fast
	function() {
	    var f = fast_bbj_out();
	    var val;
	    return function() {
		val = f();
		if (
		    typeof val !== typeof [] || 
		    typeof val.length !== typeof 0
		) return 'fast_bbj_out';
		for (var j = 0; j < val.length; j++) {
		    if (j !== 0 && j !== 1) return 'fast_bbj_out';
		}
	    };
	},  // fast_bbj_out
	function() {
	    var f = fast_bbj_in(ones());
	    var val;
	    return function() {
		val = f();
		if (
		    typeof val !== typeof [] ||
		    typeof val.length !== typeof 0
		) return 'fast_bbj_in';
		for (var j = 0; j < val.length; j++) {
		    if (val[j] !== 0 && val[j] !== 1) return 'fast_bbj_in';
		}
	    };
	},  // fast_bbj_in
	function() {
	    var u = uniform_randoms();
	    var val;
	    return function (i) {
		val = u();
		if (
		    typeof val !== typeof 0.5 ||
		    0 > val ||
		    1 <= val
		) return 'uniform_randoms';
	    };
	},  // uniform_randoms
	function() {
	    var n;
	    var r = scaled_randoms(
		product(
		    function(x, y) { return n = x * y; },
		    counter(),
		    uniform_randoms()
		)
	    );
	    var val;
	    return function() {
		val = r();
		if (
		    typeof val !== typeof 0.5 ||
		    0 > val ||
		    (n <= val && n > 0)
		) return 'scaled_randoms';
	    };
	},  // scaled_randoms
	function() {
	    var n;
	    var i = random_ints(
		product(
		    function(x, y) {
			return n = x * y;
		    },
		    counter(),
		    scaled_randoms(counter())
		)
	    );
	    var val;
	    return function() {
		val = i();
		if (
		    typeof val != typeof 0 ||
		    0 > val ||
		    (n <= val && n > 0) ||
		    Math.floor(val) !== val
		) return 'random_ints';
	    };
	},  // random_ints
	function() {
	    var b = random_bits();
	    var val;
	    return function() {
		val = b();
		if (val !== 0 && val !== 1) return 'random_bits';
	    };
	},  // random_bits
	function() {
	    var a = random_steps();
	    var val;
	    return function() {
		val = a();
		if (val !== -1 && val !== 1) return 'random_steps';
	    };
	},  // random_steps
	function() {
	    var n;
	    var w = random_walker(
		product(
		    function(x, y) { n = x * y; return n; },
		    counter(),
		    scaled_randoms(counter())
		)
	    );
	    var current = w();
	    var val;
	    return function() {
		val = w();
		if (
		    typeof val !== typeof 0.5 ||
		    (val !== current + n && val !== current - n)
		) return 'random_walker';
		current = val;
	    };
	},  // random_walker
	function() {
	    var w = random_walk();
	    var current = w();
	    var val;
	    return function() {
		val = w();
		if (
		    typeof val !== typeof 0 ||
		    (val !== current - 1 && val !== current + 1)
		) return 'random_walk';
		current = val;
	    };
	},  // random_walk
	function() {
	    var p = product(plus, counter(), counter());
	    var m = map(multiply(2), counter());
	    return function() {
		if (p() !== m()) return 'product';
	    };
	},  // product
	function() {
	    var p = make_pareto(ones());
	    var val;
	    return function() {
		val = p();
		if (
		    typeof val !== typeof 0 ||
		    val <= 0
		) return 'make_pareto';
	    };
	},  // make_pareto
	function() {
	    var p = pareto();
	    var val;
	    return function() {
		
	    };
	},  // pareto
	function() {
	    var known = [];
	    var i = 0;
	    var stream = Math.random;
	    return function() {
		
	    };
	},  // tabu
	function() {
	    var known = [];
	    var i = 1;
	    var stream = map(Math.round, product(multiply, function() { return i++; }, Math.random));
	    stream = innovator(stream);
	    return function() {
		var val = stream();
		if (known.indexOf(val) !== -1) return 'innovator';
		known.push(val);
	    };
	},  // innovator
    ];

    var test_cases = ts.map(
	function(f) {
	    return f();
	}
    );
    var failed = [];
    var result;
    for (var i = 0; i < num; i++) test_cases.forEach(function(t) {
	var result = t();
        if (
            typeof result !== typeof undefined &&
	    failed.indexOf(result) === -1
	) {
	    failed.push(result);
	    test_cases = [];
	}
    });
    
    if (failed.length) return failed;
    return true;
};

var countdown = function(n, s) {
    while (n-- > 0) s();
};

var p2 = function(a) {
    p(a);
    return a;
}

var d;
var b = function(n) {countdown(n, filter(
    function(a)
    {
	p(a);
	return true;
    },innovator(
    //ratchet(
    //    tag(
    //        function(a)
    //        {
    //            return Math.sin(
    //                parseInt(
    //                    a.join(''),
    //                    2
    //                )
    //            );
    //        },
            fast_bbj_out()
    //    )
    //)
)))};