// Search and Optimisation Streams

// Search is the problem of finding a value which
// fits some criterion. Given a set of possible
// solutions and a test function, search returns
// solutions for which the test returns true.
// Optimisation generalises search. Instead of a
// test function, we have a fitness function which
// returns numbers. Optimisation tries to return
// the solution with the highest fitness.
// A stream is any function which takes no arguments
// and returns a result. This simple interface
// represents a computation that hasn't been run yet
// (also known as a "thunk"). Streams nicely capture
// the problems of search and optimisation , since:
// 1) Search/optimisation algorithms never halt,
//    they can always give another value (even if
//    it stabilises to the same one), so we need to
//    represent these computations without actually
//    performing them (which would take forever).
//    Streams are effectively lazy lists (or
//    generators, if you prefer) of the results.
// 2) Different search algorithms rely on different
//    numbers of parameters, and use variables at
//    multiple scope levels. A thunk interface
//    encapsulates all of this at the point of
//    definition, so that the resulting streams are
//    indistinguishable and can be composed
//    interchangably. We can still use parameters
//    and state, but they must be curried into the
//    stream functions. This minimises shared state
//    and other such nastiness.
// 
// As a consequence of this currying, we generally
// don't ending up define streams directly, but
// rather curryable functions (combinators, or
// 'stream factories', if you prefer) which take
// any required parameters as arguments, set up any
// required state and return a stream with these
// built-in.

///////////////////////
// Utility functions //
///////////////////////

var c = curry = function(f) {
    var add_args = function(old_args) {
        if (old_args.length >= f.length) return apply(f, old_args);
        return function() {
            var new_args = [];
            for (var i = 0; i < arguments.length; i++) new_args.push(arguments[i]);
            return add_args(old_args.concat(new_args));
        };
    };
    return add_args([]);
};

var apply = function(f, args) {
    // Returns f(args[0], args[1], ...)
    return eval('f('+args.map(function(val, key) {
        return 'args['+key+']';
    }).join(', ')+')');
};

var plus = c(function(a, b) {
    // Reifies + ("add" may get confused with
    // array.push)
    return a+b;
});

var negate = function(a) {
    // Reifies unary -
    return -1 * a;
};

var multiply = c(function(a, b) {
    // Likewise for * ("times" may get confused
    // with iterate(f) or range(n))
    return a*b;
});

var divide = c(function(a, b) {
    // Reifies /
    return a / b;
});

var modulus = c(function(a, b) {
    // Reifies %
    return a % b;
});

var subscript = c(function(a, b) {
    // Reifies []
    return a[b];
});

var catenate = c(function(head, tail) {
    // Reifies catenation (joining arrays)
    return head.concat(tail);
});

var cons = c(function(head, tail) {
    // Reifies pairing
    return [head, tail];
});

var choose_direction = function(a) {
    // Turns 0/1 into -1/1
    return 2*a - 1;
};

var rebase = c(function(n, b) {
    // Returns the first number in the base of the
    // second number. For example, rebase(12, 2)
    // gives [1, 1, 0, 0], which is 12 in base 2.
    // We use numbers 0 to b-1 as symbols.
    var result = [];
    var val;
    do {
	val = (b === 1)? Math.min(1, n) : n % b;
        result.unshift(val);
        n = (n - val) / b;
    } while (n);
    return result;
});

var tagged_pruner = c(function(pop, n) {
    // Simple function to keep the top n members of
    // the given population (which are prefixed with
    // their fitnesses)
    if (pop.length <= n) return pop;
    pop.sort(function(a, b) {
        if (a[0] > b[0]) return 1;
        if (a[0] < b[0]) return -1;
        return 0;
    });
    return pop.slice(pop.length - n);
});

var take = c(function(n, stream) {
    // Returns an array of n values taken
    // from the given stream.
    var result = [];
    for (var i = 0; i < n; i++) result.push(stream());
    return result;
});

var cartesian_from_polar = function(angles) {
    // Takes N-1 angles, which are interpreted as
    // polar coordinates in N-dimensional space (with unit radius).
    // The final element of the angle array will be taken mod 360,
    // the rest will be taken mod 180.
    // Returns the equivalent N components of this vector in
    // cartesian space.
    var normalised_angles = (function(these_angles) {
        if (these_angles.length == 0) return [1];
        var result = [];
        for (var i = 1; i < these_angles.length + 1; i++) {
            result[i] = Math.cos(these_angles[i]);
            for (var j = 0; j < i; j++) {
                result[i] *= Math.sin(these_angles[j]);
            }
        }
        return result;
    })(angles);
    return these_angles.map(function(v, k) {
        var result;
        if (k == these_angles.length - 1) {
            result = v % 360;
            if (result < 0) result = 360 + result;
        }
        else {
            result = v % 180;
            if (result < 0) result = 180 + result;
        }
        return result;
    });
};

var times = c(function(n, val) {
    var result = [];
    while (n--) result.push(val);
    return result;
});

var array_compare = c(function(a, b) {
    // Compare 1D arrays a and b for equality.
    // Uses ===, so won't work with nested arrays.
    if (Array.isArray(a) !== Array.isArray(b)) return false;
    if (!Array.isArray(a)) return a === b;
    if (a.length !== b.length) return false;
    for (var i = 0; i < a.length; i++) {
        if (!array_compare(a[i], b[i])) return false;
    }
    return true;
});

var identity = function(x) { return x; };

var n_point_crossover = c(function(points, first, second) {
    // Takes two array-like solutions (first and
    // second) and swaps their elements at n points,
    // chosen by rand.
    var new_first = first.slice(0);
    var new_second = second.slice(0);
    points.forEach(function(n) {
        var chunk1 = new_first.splice(n * new_first.length);
        var chunk2 = new_second.splice(n * new_second.length);
        new_first = new_first.concat(chunk1);
        new_second = new_second.concat(chunk2);
    });
    return [new_first, new_second];
});

var flip_bit = c(function(rand, string) {
    // Takes an array of booleans and a random number
    // between 0 and 1. Indexes the array using the
    // random number and flips the bit.
    if (string.length === 0) return string;
    var index = Math.floor(rand * string.length - 1);
    var copy = string.slice(0, index);
    copy.push(!string[index]);
    return copy.concat(string.slice(index));
});

/////////////////////////////
// Streams and combinators //
/////////////////////////////

var constant = function(val) {
    // A constant stream always gives the same
    // value. This combinator makes constant
    // streams.
    return function() {
        return val;
    };
};

var zeros = function() {
    // A stream of 0s
    return constant(0);
};

var ones = function() {
    // A stream of 1s
    return constant(1);
};

var reduce = c(function(init, f, stream) {
    // Reduce combines the results of a
    // stream using a reduction function.
    // For function f, initial value init
    // and stream values a, b, c, ...
    // reduce returns:
    // f(init, a)
    // f(f(init, a), b)
    // f(f(f(init, a), b), c)
    // ...
    var comb = function() {
        comb = function() {
            init = f(init, stream());
            return init;
        };
        return init;
    };
    return function() {
        return comb();
    };
});

var counter = function() {
    // A useful example of a reduction.
    // Produces a stream of the Naturals
    return reduce(0, plus, ones());
};

var hoard = function(comb) {
    // A reducer which appends all results to an
    // array
    return reduce([], catenate, comb);
};

var delayer = c(function(vals, stream) {
    // Returns each element of the given
    // array, then becomes the given stream
    var index = 0;
    return function() {
        if (index < vals.length) return vals[index++];
        else return stream();
    };
});

var delay = c(function(val, stream) {
    // Returns the given value, then becomes
    // the given stream
    return delayer([val], stream);
});

var map = c(function(f, comb) {
    // Maps the given function over the given
    // stream.
    return function() {
        return f(comb());
    };
});

var iterate = c(function(f, val) {
    // Takes a function f and a value x, returns
    // the stream f(x), f(f(x)), f(f(f(x))), ...
    var result = val;
    return function() {
        result = f(result);
        return result;
    };
});

var simple = function() {
    // SIMPLE, as defined by Jurgen Schmidhuber.
    // Returns every binary sequence in ascending
    // order.
    return enumerate([0, 1]);
};

var bbj = c(function(i, o, inc, input, n) {
    // A simple, potentially Turing Complete language,
    // BitBitJump. We have an unbounded memory full of
    // zeros, the start of which is initialised to the
    // binary form of n.
    // We have a program counter (initially 0), an
    // address length (initially 2) and a single
    // instruction which runs over and over:
    // Read 2 addresses x and y, starting at the
    // program counter; copy the bit at x to the bit
    // at y; read a third address from after the
    // program counter, and use this as the new
    // program counter.
    // This implementation uses a bignum (arbitrary
    // size integer) library to store the memory and
    // addresses, but the algorithm just uses +, -, *,
    // /, % and power-of.
    // We parameterise this implementation with input,
    // output and address-size-increment. i is a pair
    // [magic address, destination]; writing 1 to the
    // magic address will read a value from the input
    // stream and write it to the destination address.
    // Similarly o is [magic address, source]; writing
    // a 1 to the magic address will append whatever is
    // at the source address to the output we return.
    // inc is a magic address; writing a 1 to it will
    // increment the address size, which makes us
    // Turing Complete. Note that you can disable any
    // of these by using negative numbers as the magic
    // addresses (hence "potentially Turing Complete",
    // since we're not if inc is inaccessible address)
    load('biginteger.js');
    var one = BigInteger(1);
    var two = BigInteger(2);
    i[0] = BigInteger(i[0]);
    i[1] = BigInteger(i[1]);
    o[0] = BigInteger(o[0]);
    o[1] = BigInteger(o[1]);
    inc = BigInteger(inc);
    var pc = BigInteger(0);
    var mem;
    var w = BigInteger(2);
    if (Array.isArray(n)) mem = BigInteger.parse(n.join(''), 2);
    else mem = BigInteger(n);
    var output = [];
    return function() {
	// Read a word from the program counter
	var x = mem.remainder(
	    two.pow(
		pc.add(w)
	    )
	).subtract(
	    mem.remainder(
		two.pow(pc)
	    )
	).quotient(
	    two.pow(pc)
	);
	// Read the next word
	var y = mem.remainder(
	    two.pow(
		pc.add(w).add(w)
	    )
	).subtract(
	    mem.remainder(
		two.pow(
		    pc.add(w)
		)
	    )
	).quotient(
	    two.pow(
		pc.add(w)
	    )
	);
	// Split the memory into above and below the bit to set
	var higher = mem.subtract(
	    mem.remainder(
		two.pow(
		    y.add(one)
		)
	    )
	);
	var lower;
	if (y.compare(BigInteger(0))) lower = mem.remainder(
	    two.pow(
		y.subtract(one)
	    )
	);
	else lower = 0;
	// Read the bit we're copying
	var val = mem.remainder(
	    two.pow(
		x.add(one)
	    )
	).subtract(
	    mem.remainder(
		two.pow(x)
	    )
	).quotient(
	    two.pow(x)
	).valueOf();

	// Update the memory (or invoke magic)
	if (val && y.compare(i[0]) === 0) {
	    val = input();
	    y = i[1];
	}
	else if (val && y.compare(o[0]) === 0) {
	    output.push(
		mem.remainder(
		    two.pow(
			o[1].add(one)
		    )
		).subtract(
		    mem.remainder(
			two.pow(o[1])
		    )
		).quotient(
		    two.pow(o[1])
		).valueOf()
	    );
	    val = mem.remainder(
		two.pow(
		    y.add(one)
		)
	    ).subtract(
		mem.remainder(
		    two.pow(y)
		)
	    ).quotient(
		two.pow(y)
	    ).valueOf();
	}
	else if (val && y === inc) {
	    w = w.add(one);
	    val = mem.remainder(
		two.pow(
		    y.add(one)
		)
	    ).subtract(
		mem.remainder(
		    two.pow(y)
		)
	    ).divide(
		two.pow(y)
	    ).valueOf();
	}
	mem = higher.add(
	    two.pow(y).multiply(
		BigInteger(val)
	    )
	).add(lower);
	// Read the next word and jump where it says
	pc = mem.remainder(
	    two.pow(
		pc.add(
		    w.multiply(BigInteger(3))
		)
	    ).subtract(
		mem.remainder(
		    two.pow(
			pc.add(w).add(w)
		    )
		)
	    )
	).quotient(
	    two.pow(
		pc.add(w).add(w)
	    )
	);
	return output;
    };
});

var bbj_pure = bbj([-1, -1], [-1, -1], -1, zeros());

var bbj_io = bbj([0,1], [2,3], 4);

var bbj_outputter = bbj([-1, -2], [0, 1], 2, zeros());

var fast = c(function(symbols, run) {
    // FAST, as defined by Jurgen Schmidhuber.
    // Runs every program using the given
    // symbols for an ever-increasing number
    // of steps. Each result is the output of
    // the next program.
    // The step function should return a pair
    // [p, output] where p can be anything, as
    // long as it can be passed as step's
    // input. step should also accept an array
    // of symbols. If no output is generated
    // in a step, then make the output []
    var phase = 1;
    return map (
        function(a) {
	    var r = run(a);
	    skipper(
                constant(Math.pow(symbols.length, phase - a.length)),
                r
	    )();
	    return r();
        },
        phases(
            function() {
                return enumerate(symbols);
            },
            function () { return phase++; }
        )
    );
});

var fast_bbj_out = function() { return fast([0,1], bbj_outputter); };

var fast_bbj_in = function(input) { return fast([0,1], bbj_io(input)); };

var uniform_randoms = function() {
    // Generates random x where 0 <= x < 1
    return Math.random;
};

var scaled_randoms = function(ns) {
    // Generates random floats x where
    // 0 <= x < n
    return map(
        function(a) {
            return ns()*a;
	},
        uniform_randoms()
    );
};

var random_ints = function(ns) {
    // Generates random integers x where 0 <= x < n
    return map(
        Math.floor,
        scaled_randoms(ns)
    );
};

var random_bits = function() {
    // Generates random bits
    return random_ints(constant(2));
};

var random_steps = function() {
    // Chooses randomly between 1 and -1
    return map(
        plus(-1),
        map(
            multiply(2),
            random_bits()
        )
    );
};

var random_walker = function(steps) {
    // Unbiased 1D random walk generator, using a stream
    // of step sizes.
    return reduce(
        0,
        plus,
        product(
            multiply,
            random_steps(),
            steps
        )
    );
};

var random_walk = function() {
    // Standard 1D random walk
    return random_walker(ones());
};

var product = c(function(f, as, bs) {
    // Combines two streams using a function
    return function() {
	return f(as(), bs());
    };
});

var make_pareto = function(scale) {
    // Returns samples from a Pareto distribution,
    // an inverse power law.
    // This implements Math.pow(Math.random(), -1 / scale)
    return map(
	function(r) {
	    return Math.pow(1.0 - r, -1 /scale());
	},
	uniform_randoms()
    );
};

var pareto = function() {
    // Standard Pareto distribution
    make_pareto(ones());
};

var levy_flight = function() {
    // 1D Levy Flight; a random walk with random step
    // sizes, chosen with a heavy-tailed probability
    // density (in this case a Pareto distribution)
    return random_walker(pareto());
};

var zip_with = c(function(f, comb1, comb2) {
    // Combines two streams using the given function
    return function() {
        return f(comb1(), comb2());
    };
});

var guess = c(function(symbols, step) {
    // GUESS, as defined by Jurgen Schmidhuber.
    // Creates random programs from the given
    // symbols and runs them for a random
    // number of steps. Returns the output
    // generated by the program.
    // The step function should curry its
    // program, which is a stream of symbols,
    // to give a stream of output ([] meaning
    // no output at this step).

    // t controls each run to force halting
    var t;
    // A random stream of input symbols.
    // We decrease t as input is read, so
    // that longer programs are given less
    // time
    var input = map(
        function(x) {
            t /= symbols.length;
            return symbols[x];
        },
        random_ints(symbols.length)
    );
    var steps = filter(
        function(n) {
            if (n) return true;
            t *= symbols.length;
            return false;
        },
        random_ints(symbols.length)
    );
    return function() {
        t = 1;
        steps();
        return reduce(
	    [],
	    concat,
	    step(input)
	);
    };
});

var make_scattered = c(function(ns, nil, comb, choice) {
    // Creates arrays a with lengths taken from ns.
    // Element a[choice()] = comb(), everything else
    // is nil().
    return function() {
        var index = choice();
        var result = [];
        var length = ns();
        for (var i=0; i < length; i++) {
            if (i == index) result.push(comb());
            else result.push(nil());
        }
        return result;
    };
});

var make_manhattan_steps = c(function(n, directions, steps) {
    // Generalises a 1D stream of steps to N
    // dimensions. The steps are 0 in every direction
    // except for those chosen by a stream of integers
    // between 0 and N-1 inclusive.
    return make_scattered(
        constant(n),
        constant(0),
        steps,
        directions
    );
});

var chooser = c(function(streams, choices) {
    // Interleaves N streams according to a stream of
    // integer choices from 0 to N-1
    return function() {
        return (streams[choices()])();
    };
});

var interleave = function(streams) {
    // Turns an array of streams into a stream of their
    // results, cycling through the array.
    var choice = 0;
    return chooser(
        streams,
        function() { return choice = ++choice % streams.length; }
    );
};

var vectors = function(streams) {
    // Makes an N-dimensional vector from N scalars,
    // using one stream for each dimension.
    return function() {
        var result = [];
        var n = streams.length;
        for (var i=0; i < n; i++) {
            result.push((streams[i])());
        }
        return result;
    };
};

var parallel_map = c(function(fs, stream) {
    // Takes a stream of arrays and an array of functions, returning
    // arrays where element i is functions[i](input[i])
    return map(
        function(arr) {
            return arr.map(function(v, k) { return fs[k](v); });
        },
        stream
    );
});

var nested_map = c(function(f, stream) {
    // Takes a stream of arrays and maps the given function over
    // all of the members.
    return map(
        function(arr) { return arr.map(f); },
        stream
    );
});

var tag = c(function(f, comb) {
    // Returns pairs of [f(x), x] where x is drawn from the given
    // stream.
    return function() {
        var val = comb();
        return [f(val), val];
    };
});

var carrying_counter = function(n) {
    // A stream of all sequences of numbers which
    // are less than the given integer. For
    // example, given 3 we would get the stream:
    // [0], [1], [2], [0, 0], [0, 1], [0, 2],
    // [1. 0], [1, 1], etc.
    var result = [-1];
    var index = 0;
    return function() {
        result[index] = result[index] + 1;
        while (result[index] >= n) {
            result[index] = 0;
            index--;
            if (index < 0) {
                result.push(0);
                index = result.length - 1;
            }
            else result[index] = result[index] + 1;
        }
        index = result.length - 1;
        return result;
    };
};

var enumerate = function(alphabet) {
    // Returns all strings of the given alphabet, in
    // size-increasing lexicographic order. The
    // alphabet is an array of arbitrary symbols.
    var counter = carrying_counter(alphabet.length);
    return function() {
        return counter().map(function(v) {
            return alphabet[v];
        });
    };
};

var phases = c(function(stream_builder, ns) {
    // Takes a stream-of-streams-of-arrays. Also
    // takes a stream of numbers.
    // We take a number, and a stream of arrays,
    // and keep returning the arrays until one's
    // length is more than our number. We then
    // take the next number and stream, etc.
    var n = ns();
    var stream = stream_builder();
    return function() {
	var result = stream();
        while (result.length > n) {
	    n = ns();
	    stream = stream_builder();
	    result = stream();
	}
	return result;
    };
});

var filter = c(function(f, comb) {
    // Only returns those values from the given
    // combinator for which f(value) == true.
    // WARNING: This will only terminate once it
    // finds a value which passes the test! Only
    // use it when you know this will take a
    // finite number of attempts (or else make sure
    // to have another process ready to kill this
    // one!)
    return function() {
        var val = comb();
        while (!f(val)) {
             val = comb();
        }
        return val;
    };
});

var skipper = c(function(ns, comb) {
    // Discards n values from the given combinator,
    // then returns the next one. The number to
    // discard is taken from a combinator too.
    return function() {
        var n = ns();
        while (n--) {
            comb();
        }
        return comb();
    };
});

var chunk = c(function(ns, comb) {
    // Reads numbers from ns and returns arrays of
    // that many values from comb.
    return function() {
	return take(ns(), comb);
    };
});

var make_population = c(function(ns, source, kill) {
    // Maintains a population with sizes taken from ns.
    // The source combinator is used when new values are
    // needed, and the kill function is used to prune a
    // population, via kill(pop, n).
    var pop = [];
    return function() {
        var size = ns();
        while (pop.length < size) {
            pop.push(source());
        }
        while (pop.length > size) {
            pop = kill(pop, size);
        }
        return pop;
    };
});

var survivors = c(function(ns, comb, fitness) {
    // Makes a population where each has a fitness, and
    // keeps the fittest when pruning.
    return make_population(
        ns,
        tag(fitness, comb),
        tagged_pruner
    );
});

var make_breeders = c(function(ns, f, kill) {
    // Makes a population where the new members are a
    // function of the existing members. The population
    // size is set by ns, the function to make new
    // members is f and the pruning function is kill.
    var pop = [];
    var comb = make_population(
        ns,
        function() {
            return f(pop);
        },
        kill
    );
    return function() {
        pop = comb();
        return pop;
    };
});

var make_survival_breeders = c(function(ns, f, fitness) {
    // Makes a population where the new members are a
    // function of the existing members. Each is given a
    // fitness, and the fittest are kept during pruning.
    return make_breeders(
        ns,
        function(pop) {
            var val = f(pop);
            return [fitness(val), val];
        },
        tagged_pruner
    );
});

var make_fittest = function(comb) {
    // Returns the fittest individual from a population
    // of fitness-tagged values (ie. [f(x),x] )
    return function() {
        var pop = comb();
        var fittest = 0;
        var fitness = pop[0][0];
        for (var i=0; i < pop.length; i++) {
            if (pop[i][0] > fitness) {
                fittest = i;
                fitness = pop[i][0];
            }
        }
	return pop[fittest][1];
    };
};

var mutate = c(function(choices, step) {
    return map(function(a) {
        if (choices()) {
            return step(a);
        }
        return a;
    });
});

var extremal_optimiser = c(function(source, fitness, size) {
    // Performs extremal optimisation. This is similar
    // to a genetic algorithm but rather than finding
    // fit solutions and breeding them, we find weak
    // solutions and replace them. This results in a
    // hill climbing behaviour with occasional jumps
    // between hills.

    // We maintain a population of solutions drawn
    // from source, where the weakest are pruned
    // if the population size decreases. When called,
    // we set the population to size-1 (pruning the
    // weakest solution) then set it back to size and
    // return the result.
    return skipper(ones(),
	survivors(
	    interleave(constant(size-1), constant(size)),
	    source,
	    fitness
	)
    );
});

var informed_chooser = function(next) {
    // Returns arrays with elements taken from
    // the given function. Each time, we call
    // it with the partial array we've built
    // so far. It should return pairs [a, b]
    // where a is a boolean for whether to
    // stop and b is the value to push on the
    // array.
    return function() {
	var result = [];
	filter(
	    function(a) {
		if (!a[0]) result.push(a[1]);
		return a[0];
	    },
	    function() { return next(result); }
	)();
	return result;
    };
};

var levin_builder = function(matrix) {
    // Returns arrays of numbers based on their
    // probability, where the given matrix tells
    // us the probability of each symbol
    // based on the program so far. The
    // probability of ending the array is 1 -
    // the sum of the symbol probabilities.
    return informed_chooser(
	function(so_far) {
	    var probs = matrix['['+so_far.join(',')+']'];
            var sample = Math.random();
            for (var i = 0; i < probs.length; i++) {
		if (sample <= probs[i]) return [false, i];
		sample -= probs[i];
	    }
	    return [true, probs.length];
	}
    );
};

var levin_search = c(function(matrices, step) {
    // How do we get the number of steps to take?
    return levin_builder(matrices());
});

var oops = c(function(symbols, step, test) {
    // Schmidhuber's "Optimal Ordered Problem
    // Solver". Similar to FAST, but remembers the
    // last successful program. When given a new
    // problem, it interleaves new programs with
    // continuations of the old program, to
    // potentially speed up the search.
    var solution_stream = fast(symbols, step);
    return filter(
        function(a) {
	    if (test(a)) {
		solution_stream = interleave(
		    map(
			concat(a),
			phases(
			    function() {
				return enumerate(symbols);
			    },
			    counter()
			)
		    ),
		    fast(symbols, step)
		);
		return true;
	    }
	    return false;
	},
        function() {
	    return solution_stream();
	}
    );
});

var adaptive_levin = function() {
    // Runs a Levin Search, but our probability
    // matrix is updated whenever we find an
    // acceptable solution.
};

var ratchet = function(stream) {
    // Always returns the best solution found so far
    var best;
    var fittest = -Infinity;
    return map(
        function(a) {
            if (a[0] > fittest) {
                fittest = a[0];
                best = a[1];
            }
            return best;
        },
        stream
    );
};

var exhaustive = c(function(stream, symbols) {
    // Makes the given search stream exhaustive by
    // taking every other result from a brute-force
    // enumeration of the given symbols
    return interleave([stream, enumerate(symbols)]);
});

var tabu = function(ns, stream) {
    // Remembers up to n previous results,
    // and only gives out a value if it's
    // not in it's remembered list. n is
    // drawn from ns on each call.
    var mem = [];
    return function() {
	var n = ns();
	while (mem.length > n) mem.shift();
	var val;
	do {
	    val = stream();
	} while (mem.map(array_compare(val)).some(identity));
	mem.push(val);
	if (mem.length > n) mem.shift();
	return val;
    };
};

var innovator = function(stream) {
    // Remembers all previous values,
    // only gives out values it's not
    // seen before.
    // NOTE: Don't ask for more values
    // than there are in your domain.
    // For example, asking for 3 bits
    // will hang!
    return tabu(map(plus(1), counter()), stream);
};