// -- (function(scope){
// -- 'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aQ.ac === region.a3.ac)
	{
		return 'on line ' + region.aQ.ac;
	}
	return 'on lines ' + region.aQ.ac + ' through ' + region.a3.ac;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ch,
		impl.cU,
		impl.cP,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


/*
function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}

*/

/*
function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}

*/

/*
function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}

*/

/*
function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}

*/



// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		M: func(record.M),
		R: record.R,
		P: record.P
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.M;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.R;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.P) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ch,
		impl.cU,
		impl.cP,
		function(sendToApp, initialModel) {
			var view = impl.cV;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ch,
		impl.cU,
		impl.cP,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aP && impl.aP(sendToApp)
			var view = impl.cV;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aC);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.cS) && (_VirtualDom_doc.title = title = doc.cS);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.cD;
	var onUrlRequest = impl.cE;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aP: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bu === next.bu
							&& curr.bb === next.bb
							&& curr.bq.a === next.bq.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ch: function(flags)
		{
			return A3(impl.ch, flags, _Browser_getUrl(), key);
		},
		cV: impl.cV,
		cU: impl.cU,
		cP: impl.cP
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cd: 'hidden', b_: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cd: 'mozHidden', b_: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cd: 'msHidden', b_: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cd: 'webkitHidden', b_: 'webkitvisibilitychange' }
		: { cd: 'hidden', b_: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bD: _Browser_getScene(),
		bR: {
			bS: _Browser_window.pageXOffset,
			bT: _Browser_window.pageYOffset,
			cW: _Browser_doc.documentElement.clientWidth,
			cc: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cW: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		cc: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bD: {
				cW: node.scrollWidth,
				cc: node.scrollHeight
			},
			bR: {
				bS: node.scrollLeft,
				bT: node.scrollTop,
				cW: node.clientWidth,
				cc: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bD: _Browser_getScene(),
			bR: {
				bS: x,
				bT: y,
				cW: _Browser_doc.documentElement.clientWidth,
				cc: _Browser_doc.documentElement.clientHeight
			},
			b5: {
				bS: x + rect.left,
				bT: y + rect.top,
				cW: rect.width,
				cc: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.h) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.i);
		} else {
			var treeLen = builder.h * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.j) : builder.j;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.h);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.i);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{j: nodeList, h: (len / $elm$core$Array$branchFactor) | 0, i: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {a7: fragment, bb: host, bo: path, bq: port_, bu: protocol, bv: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$SelectionMode$AllowCustomOptions = 0;
var $author$project$SelectionMode$MultiSelect = function (a) {
	return {$: 1, a: a};
};
var $author$project$SelectionMode$NoCustomOptions = 1;
var $author$project$Main$ShowClearButton = 3;
var $author$project$Main$ShowDropdownIndicator = 2;
var $author$project$Main$ShowLoadingIndicator = 1;
var $author$project$Main$ShowNothing = 0;
var $author$project$SelectionMode$SingleSelect = function (a) {
	return {$: 0, a: a};
};
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $author$project$Option$EmptyOptionValue = {$: 1};
var $author$project$Option$getOptionValue = function (option) {
	switch (option.$) {
		case 0:
			var value = option.c;
			return value;
		case 1:
			var value = option.c;
			return value;
		default:
			return $author$project$Option$EmptyOptionValue;
	}
};
var $author$project$Option$findOptionByOptionValue = F2(
	function (optionValue, options) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			function (option) {
				return _Utils_eq(
					$author$project$Option$getOptionValue(option),
					optionValue);
			},
			options);
	});
var $author$project$Option$NoDescription = {$: 1};
var $author$project$Option$getOptionDescription = function (option) {
	switch (option.$) {
		case 0:
			var optionDescription = option.d;
			return optionDescription;
		case 1:
			return $author$project$Option$NoDescription;
		default:
			return $author$project$Option$NoDescription;
	}
};
var $author$project$Option$NoOptionGroup = {$: 1};
var $author$project$Option$getOptionGroup = function (option) {
	switch (option.$) {
		case 0:
			var optionGroup = option.e;
			return optionGroup;
		case 1:
			return $author$project$Option$NoOptionGroup;
		default:
			return $author$project$Option$NoOptionGroup;
	}
};
var $author$project$Option$getOptionLabel = function (option) {
	switch (option.$) {
		case 0:
			var label = option.b;
			return label;
		case 1:
			var label = option.b;
			return label;
		default:
			var label = option.b;
			return label;
	}
};
var $author$project$Option$getOptionValueAsString = function (option) {
	var _v0 = $author$project$Option$getOptionValue(option);
	if (!_v0.$) {
		var string = _v0.a;
		return string;
	} else {
		return '';
	}
};
var $author$project$Option$CustomOption = F4(
	function (a, b, c, d) {
		return {$: 1, a: a, b: b, c: c, d: d};
	});
var $author$project$Option$EmptyOption = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Option$Option = F6(
	function (a, b, c, d, e, f) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $author$project$Option$setDescription = F2(
	function (description, option) {
		switch (option.$) {
			case 0:
				var optionDisplay = option.a;
				var label = option.b;
				var optionValue = option.c;
				var group = option.e;
				var search = option.f;
				return A6($author$project$Option$Option, optionDisplay, label, optionValue, description, group, search);
			case 1:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				var optionValue = option.c;
				var search = option.d;
				return A4($author$project$Option$CustomOption, optionDisplay, optionLabel, optionValue, search);
			default:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				return A2($author$project$Option$EmptyOption, optionDisplay, optionLabel);
		}
	});
var $author$project$Option$setGroup = F2(
	function (optionGroup, option) {
		switch (option.$) {
			case 0:
				var optionDisplay = option.a;
				var label = option.b;
				var optionValue = option.c;
				var description = option.d;
				var search = option.f;
				return A6($author$project$Option$Option, optionDisplay, label, optionValue, description, optionGroup, search);
			case 1:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				var optionValue = option.c;
				var search = option.d;
				return A4($author$project$Option$CustomOption, optionDisplay, optionLabel, optionValue, search);
			default:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				return A2($author$project$Option$EmptyOption, optionDisplay, optionLabel);
		}
	});
var $author$project$Option$OptionValue = function (a) {
	return {$: 0, a: a};
};
var $author$project$OptionLabel$optionLabelToString = function (optionLabel) {
	var label = optionLabel.a;
	return label;
};
var $author$project$Option$setLabel = F2(
	function (label, option) {
		switch (option.$) {
			case 0:
				var optionDisplay = option.a;
				var optionValue = option.c;
				var description = option.d;
				var group = option.e;
				var search = option.f;
				return A6($author$project$Option$Option, optionDisplay, label, optionValue, description, group, search);
			case 1:
				var optionDisplay = option.a;
				var search = option.d;
				return A4(
					$author$project$Option$CustomOption,
					optionDisplay,
					label,
					$author$project$Option$OptionValue(
						$author$project$OptionLabel$optionLabelToString(label)),
					search);
			default:
				var optionDisplay = option.a;
				return A2($author$project$Option$EmptyOption, optionDisplay, label);
		}
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Basics$not = _Basics_not;
var $author$project$Option$optionListContainsOptionWithValue = F2(
	function (option, options) {
		var optionValue = $author$project$Option$getOptionValue(option);
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (option_) {
					return _Utils_eq(
						$author$project$Option$getOptionValue(option_),
						optionValue);
				},
				options));
	});
var $author$project$Option$OptionDisabled = 5;
var $author$project$Option$OptionHidden = 1;
var $author$project$Option$OptionSelected = 2;
var $author$project$Option$OptionSelectedHighlighted = 3;
var $author$project$Option$selectOption = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var description = option.d;
			var group = option.e;
			var search = option.f;
			switch (display) {
				case 0:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 1:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 2:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 3:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 4:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				default:
					return A6($author$project$Option$Option, 5, label, value, description, group, search);
			}
		case 1:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var search = option.d;
			switch (display) {
				case 0:
					return A4($author$project$Option$CustomOption, 2, label, value, search);
				case 1:
					return A4($author$project$Option$CustomOption, 1, label, value, search);
				case 2:
					return A4($author$project$Option$CustomOption, 2, label, value, search);
				case 3:
					return A4($author$project$Option$CustomOption, 3, label, value, search);
				case 4:
					return A4($author$project$Option$CustomOption, 2, label, value, search);
				default:
					return A4($author$project$Option$CustomOption, 5, label, value, search);
			}
		default:
			var display = option.a;
			var label = option.b;
			switch (display) {
				case 0:
					return A2($author$project$Option$EmptyOption, 2, label);
				case 1:
					return A2($author$project$Option$EmptyOption, 2, label);
				case 2:
					return A2($author$project$Option$EmptyOption, 2, label);
				case 3:
					return A2($author$project$Option$EmptyOption, 2, label);
				case 4:
					return A2($author$project$Option$EmptyOption, 2, label);
				default:
					return A2($author$project$Option$EmptyOption, 5, label);
			}
	}
};
var $author$project$Option$selectedOptions = function (options) {
	return A2(
		$elm$core$List$filter,
		function (option_) {
			switch (option_.$) {
				case 0:
					var display = option_.a;
					switch (display) {
						case 0:
							return false;
						case 1:
							return false;
						case 2:
							return true;
						case 3:
							return true;
						case 4:
							return false;
						default:
							return false;
					}
				case 1:
					var display = option_.a;
					switch (display) {
						case 0:
							return false;
						case 1:
							return false;
						case 2:
							return true;
						case 3:
							return true;
						case 4:
							return false;
						default:
							return false;
					}
				default:
					var display = option_.a;
					switch (display) {
						case 0:
							return false;
						case 1:
							return false;
						case 2:
							return true;
						case 3:
							return true;
						case 4:
							return false;
						default:
							return false;
					}
			}
		},
		options);
};
var $author$project$Option$setSelectedOptionInNewOptions = F2(
	function (oldOptions, newOptions) {
		var oldSelectedOption = $author$project$Option$selectedOptions(oldOptions);
		return A2(
			$elm$core$List$map,
			function (newOption_) {
				return A2($author$project$Option$optionListContainsOptionWithValue, newOption_, oldSelectedOption) ? $author$project$Option$selectOption(newOption_) : newOption_;
			},
			newOptions);
	});
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_community$list_extra$List$Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$Set$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$Set$insert, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$uniqueBy = F2(
	function (f, list) {
		return A4($elm_community$list_extra$List$Extra$uniqueHelp, f, $elm$core$Set$empty, list, _List_Nil);
	});
var $author$project$Option$mergeTwoListsOfOptionsPreservingSelectedOptions = F2(
	function (optionsA, optionsB) {
		var combineOptions = F2(
			function (optionA, optionB) {
				var optionBLabel = $author$project$Option$getOptionLabel(optionB);
				var optionBGroup = $author$project$Option$getOptionGroup(optionB);
				var optionBDescription = $author$project$Option$getOptionDescription(optionB);
				return A2(
					$author$project$Option$setGroup,
					optionBGroup,
					A2(
						$author$project$Option$setLabel,
						optionBLabel,
						A2($author$project$Option$setDescription, optionBDescription, optionA)));
			});
		var updatedOptionsA = A2(
			$elm$core$List$map,
			function (optionA) {
				var _v0 = A2(
					$author$project$Option$findOptionByOptionValue,
					$author$project$Option$getOptionValue(optionA),
					optionsB);
				if (!_v0.$) {
					var optionB = _v0.a;
					return A2(combineOptions, optionA, optionB);
				} else {
					return optionA;
				}
			},
			optionsA);
		var superList = _Utils_ap(updatedOptionsA, optionsB);
		var newOptions = A2($elm_community$list_extra$List$Extra$uniqueBy, $author$project$Option$getOptionValueAsString, superList);
		return A2($author$project$Option$setSelectedOptionInNewOptions, superList, newOptions);
	});
var $author$project$SortRank$NoSortRank = {$: 2};
var $author$project$OptionLabel$OptionLabel = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Option$newSelectedOption = F2(
	function (string, maybeCleanLabel) {
		return A6(
			$author$project$Option$Option,
			2,
			A3($author$project$OptionLabel$OptionLabel, string, maybeCleanLabel, $author$project$SortRank$NoSortRank),
			$author$project$Option$OptionValue(string),
			$author$project$Option$NoDescription,
			$author$project$Option$NoOptionGroup,
			$elm$core$Maybe$Nothing);
	});
var $author$project$Option$addAndSelectOptionsInOptionsListByString = F2(
	function (strings, options) {
		var newOptions = A2(
			$elm$core$List$map,
			function (str) {
				return A2($author$project$Option$newSelectedOption, str, $elm$core$Maybe$Nothing);
			},
			strings);
		return A2($author$project$Option$mergeTwoListsOfOptionsPreservingSelectedOptions, newOptions, options);
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Ports$errorMessage = _Platform_outgoingPort('errorMessage', $elm$json$Json$Encode$string);
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Option$getOptionDisplay = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			return display;
		case 1:
			var display = option.a;
			return display;
		default:
			var display = option.a;
			return display;
	}
};
var $author$project$Option$filterOptionsToShowInDropdown = $elm$core$List$filter(
	function (option) {
		var _v0 = $author$project$Option$getOptionDisplay(option);
		switch (_v0) {
			case 0:
				return true;
			case 1:
				return false;
			case 2:
				return true;
			case 3:
				return true;
			case 4:
				return true;
			default:
				return false;
		}
	});
var $elm_community$list_extra$List$Extra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(index);
				} else {
					var $temp$index = index + 1,
						$temp$predicate = predicate,
						$temp$list = xs;
					index = $temp$index;
					predicate = $temp$predicate;
					list = $temp$list;
					continue findIndexHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$findIndex = $elm_community$list_extra$List$Extra$findIndexHelp(0);
var $author$project$Option$isOptionHighlighted = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			switch (display) {
				case 0:
					return false;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return true;
				default:
					return false;
			}
		case 1:
			var display = option.a;
			switch (display) {
				case 0:
					return false;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return true;
				default:
					return false;
			}
		default:
			var display = option.a;
			switch (display) {
				case 0:
					return false;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return true;
				default:
					return false;
			}
	}
};
var $author$project$Option$findHighlightedOptionIndex = function (options) {
	return A2(
		$elm_community$list_extra$List$Extra$findIndex,
		function (option) {
			return $author$project$Option$isOptionHighlighted(option);
		},
		options);
};
var $author$project$Option$isOptionSelected = function (option) {
	var isOptionDisplaySelected = function (optionDisplay) {
		switch (optionDisplay) {
			case 0:
				return false;
			case 1:
				return false;
			case 2:
				return true;
			case 3:
				return true;
			case 4:
				return false;
			default:
				return false;
		}
	};
	switch (option.$) {
		case 0:
			var optionDisplay = option.a;
			return isOptionDisplaySelected(optionDisplay);
		case 1:
			var optionDisplay = option.a;
			return isOptionDisplaySelected(optionDisplay);
		default:
			var optionDisplay = option.a;
			return isOptionDisplaySelected(optionDisplay);
	}
};
var $author$project$Option$findSelectedOptionIndex = function (options) {
	return A2(
		$elm_community$list_extra$List$Extra$findIndex,
		function (option) {
			return $author$project$Option$isOptionSelected(option);
		},
		options);
};
var $author$project$Option$findHighlightedOrSelectedOptionIndex = function (options) {
	var _v0 = $author$project$Option$findHighlightedOptionIndex(options);
	if (!_v0.$) {
		var index = _v0.a;
		return $elm$core$Maybe$Just(index);
	} else {
		return $author$project$Option$findSelectedOptionIndex(options);
	}
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$PositiveInt$toInt = function (positiveInt) {
	var _int = positiveInt;
	return _int;
};
var $author$project$Main$figureOutWhichOptionsToShow = F2(
	function (maxDropdownItems, options) {
		var optionsThatCouldBeShown = $author$project$Option$filterOptionsToShowInDropdown(options);
		var maxNumberOfDropdownItems = $author$project$PositiveInt$toInt(maxDropdownItems);
		var lastIndexOfOptions = $elm$core$List$length(optionsThatCouldBeShown) - 1;
		if (_Utils_cmp(
			$elm$core$List$length(optionsThatCouldBeShown),
			maxNumberOfDropdownItems) < 1) {
			return optionsThatCouldBeShown;
		} else {
			var _v0 = $author$project$Option$findHighlightedOrSelectedOptionIndex(optionsThatCouldBeShown);
			if (!_v0.$) {
				var index = _v0.a;
				if (!index) {
					return A2($elm$core$List$take, maxNumberOfDropdownItems, optionsThatCouldBeShown);
				} else {
					if (_Utils_eq(
						index,
						$elm$core$List$length(optionsThatCouldBeShown) - 1)) {
						return A2(
							$elm$core$List$drop,
							$elm$core$List$length(options) - maxNumberOfDropdownItems,
							optionsThatCouldBeShown);
					} else {
						var isEven = !A2($elm$core$Basics$modBy, 2, maxNumberOfDropdownItems);
						var half = isEven ? ((maxNumberOfDropdownItems / 2) | 0) : (((maxNumberOfDropdownItems / 2) | 0) + 1);
						return (_Utils_cmp(index + half, lastIndexOfOptions) > 0) ? A2(
							$elm$core$List$drop,
							$elm$core$List$length(options) - maxNumberOfDropdownItems,
							optionsThatCouldBeShown) : (((index - half) < 0) ? A2($elm$core$List$take, maxNumberOfDropdownItems, optionsThatCouldBeShown) : A2(
							$elm$core$List$take,
							maxNumberOfDropdownItems,
							A2($elm$core$List$drop, (index + 1) - half, options)));
					}
				}
			} else {
				return A2($elm$core$List$take, maxNumberOfDropdownItems, options);
			}
		}
	});
var $author$project$Option$hasSelectedOption = function (options) {
	return !$elm$core$List$isEmpty(
		$author$project$Option$selectedOptions(options));
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Option$isEmptyOption = function (option) {
	switch (option.$) {
		case 0:
			return false;
		case 1:
			return false;
		default:
			return true;
	}
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $author$project$Option$optionValueToString = function (optionValue) {
	if (!optionValue.$) {
		var valueString = optionValue.a;
		return valueString;
	} else {
		return '';
	}
};
var $author$project$Option$isOptionInListOfOptionsByValue = F2(
	function (optionValue, options) {
		return A2(
			$elm$core$List$any,
			function (option) {
				return _Utils_eq(
					$author$project$Option$optionValueToString(
						$author$project$Option$getOptionValue(option)),
					$author$project$Option$optionValueToString(optionValue));
			},
			options);
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Ports$muchSelectIsReady = _Platform_outgoingPort(
	'muchSelectIsReady',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$PositiveInt$PositiveInt = $elm$core$Basics$identity;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$PositiveInt$new = function (_int) {
	return $elm$core$Basics$abs(_int);
};
var $author$project$Option$OptionShown = 0;
var $author$project$Option$newOption = F2(
	function (value, maybeCleanLabel) {
		if (value === '') {
			return A2(
				$author$project$Option$EmptyOption,
				0,
				A3($author$project$OptionLabel$OptionLabel, '', maybeCleanLabel, $author$project$SortRank$NoSortRank));
		} else {
			return A6(
				$author$project$Option$Option,
				0,
				A3($author$project$OptionLabel$OptionLabel, value, maybeCleanLabel, $author$project$SortRank$NoSortRank),
				$author$project$Option$OptionValue(value),
				$author$project$Option$NoDescription,
				$author$project$Option$NoOptionGroup,
				$elm$core$Maybe$Nothing);
		}
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Option$OptionDescription = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Option$descriptionDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A3(
			$elm$json$Json$Decode$map2,
			$author$project$Option$OptionDescription,
			A2($elm$json$Json$Decode$field, 'description', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$field,
				'descriptionClean',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string))),
			$elm$json$Json$Decode$succeed($author$project$Option$NoDescription)
		]));
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Option$displayDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$andThen,
			function (str) {
				if (str === 'true') {
					return $elm$json$Json$Decode$succeed(2);
				} else {
					return $elm$json$Json$Decode$fail('Option is not selected');
				}
			},
			A2($elm$json$Json$Decode$field, 'selected', $elm$json$Json$Decode$string)),
			A2(
			$elm$json$Json$Decode$andThen,
			function (isSelected) {
				return isSelected ? $elm$json$Json$Decode$succeed(2) : $elm$json$Json$Decode$succeed(0);
			},
			A2($elm$json$Json$Decode$field, 'selected', $elm$json$Json$Decode$bool)),
			A2(
			$elm$json$Json$Decode$andThen,
			function (isDisabled) {
				return isDisabled ? $elm$json$Json$Decode$succeed(5) : $elm$json$Json$Decode$fail('Option is not disabled');
			},
			A2($elm$json$Json$Decode$field, 'disabled', $elm$json$Json$Decode$bool)),
			$elm$json$Json$Decode$succeed(0)
		]));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$SortRank$Auto = function (a) {
	return {$: 0, a: a};
};
var $author$project$SortRank$Manual = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$PositiveInt$maybeNew = function (_int) {
	return (_int >= 0) ? $elm$core$Maybe$Just(_int) : $elm$core$Maybe$Nothing;
};
var $author$project$SortRank$sortRankDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$andThen,
			function (_int) {
				var _v0 = $author$project$PositiveInt$maybeNew(_int);
				if (!_v0.$) {
					var positiveInt = _v0.a;
					return $elm$json$Json$Decode$succeed(
						$author$project$SortRank$Auto(positiveInt));
				} else {
					return $elm$json$Json$Decode$fail('The index must be a positive number.');
				}
			},
			A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int)),
			A2(
			$elm$json$Json$Decode$andThen,
			function (_int) {
				var _v1 = $author$project$PositiveInt$maybeNew(_int);
				if (!_v1.$) {
					var positiveInt = _v1.a;
					return $elm$json$Json$Decode$succeed(
						$author$project$SortRank$Manual(positiveInt));
				} else {
					return $elm$json$Json$Decode$fail('The weight must be a positive number.');
				}
			},
			A2($elm$json$Json$Decode$field, 'weight', $elm$json$Json$Decode$int)),
			$elm$json$Json$Decode$succeed($author$project$SortRank$NoSortRank)
		]));
var $author$project$OptionLabel$labelDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$OptionLabel$OptionLabel,
	A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'labelClean',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	$author$project$SortRank$sortRankDecoder);
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Option$OptionGroup = function (a) {
	return {$: 0, a: a};
};
var $author$project$Option$optionGroupDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			$author$project$Option$OptionGroup,
			A2($elm$json$Json$Decode$field, 'group', $elm$json$Json$Decode$string)),
			$elm$json$Json$Decode$succeed($author$project$Option$NoOptionGroup)
		]));
var $elm$core$String$trim = _String_trim;
var $author$project$Option$valueDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (valueStr) {
		var _v0 = $elm$core$String$trim(valueStr);
		if (_v0 === '') {
			return $elm$json$Json$Decode$succeed($author$project$Option$EmptyOptionValue);
		} else {
			var str = _v0;
			return $elm$json$Json$Decode$succeed(
				$author$project$Option$OptionValue(str));
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Option$decodeOptionWithAValue = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Option$Option,
	$author$project$Option$displayDecoder,
	$author$project$OptionLabel$labelDecoder,
	A2($elm$json$Json$Decode$field, 'value', $author$project$Option$valueDecoder),
	$author$project$Option$descriptionDecoder,
	$author$project$Option$optionGroupDecoder,
	$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing));
var $author$project$Option$decodeOptionWithoutAValue = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		if (!value.$) {
			return $elm$json$Json$Decode$fail('It can not be an option without a value because it has a value.');
		} else {
			return A3($elm$json$Json$Decode$map2, $author$project$Option$EmptyOption, $author$project$Option$displayDecoder, $author$project$OptionLabel$labelDecoder);
		}
	},
	A2($elm$json$Json$Decode$field, 'value', $author$project$Option$valueDecoder));
var $author$project$Option$decoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[$author$project$Option$decodeOptionWithoutAValue, $author$project$Option$decodeOptionWithAValue]));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Option$optionsDecoder = $elm$json$Json$Decode$list($author$project$Option$decoder);
var $author$project$Option$OptionHighlighted = 4;
var $author$project$Option$deselectOption = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var description = option.d;
			var group = option.e;
			var search = option.f;
			switch (display) {
				case 0:
					return A6($author$project$Option$Option, 0, label, value, description, group, search);
				case 1:
					return A6($author$project$Option$Option, 1, label, value, description, group, search);
				case 2:
					return A6($author$project$Option$Option, 0, label, value, description, group, search);
				case 3:
					return A6($author$project$Option$Option, 0, label, value, description, group, search);
				case 4:
					return A6($author$project$Option$Option, 4, label, value, description, group, search);
				default:
					return A6($author$project$Option$Option, 5, label, value, description, group, search);
			}
		case 1:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var search = option.d;
			switch (display) {
				case 0:
					return A4($author$project$Option$CustomOption, 0, label, value, search);
				case 1:
					return A4($author$project$Option$CustomOption, 1, label, value, search);
				case 2:
					return A4($author$project$Option$CustomOption, 0, label, value, search);
				case 3:
					return A4($author$project$Option$CustomOption, 3, label, value, search);
				case 4:
					return A4($author$project$Option$CustomOption, 4, label, value, search);
				default:
					return A4($author$project$Option$CustomOption, 5, label, value, search);
			}
		default:
			var display = option.a;
			var label = option.b;
			switch (display) {
				case 0:
					return A2($author$project$Option$EmptyOption, 0, label);
				case 1:
					return A2($author$project$Option$EmptyOption, 1, label);
				case 2:
					return A2($author$project$Option$EmptyOption, 0, label);
				case 3:
					return A2($author$project$Option$EmptyOption, 0, label);
				case 4:
					return A2($author$project$Option$EmptyOption, 4, label);
				default:
					return A2($author$project$Option$EmptyOption, 5, label);
			}
	}
};
var $author$project$Option$isOptionValueInListOfStrings = F2(
	function (possibleValues, option) {
		return A2(
			$elm$core$List$any,
			function (possibleValue) {
				return _Utils_eq(
					$author$project$Option$getOptionValueAsString(option),
					possibleValue);
			},
			possibleValues);
	});
var $author$project$Option$selectOptionsInOptionsListByString = F2(
	function (strings, options) {
		return A2(
			$elm$core$List$map,
			function (option) {
				return A2($author$project$Option$isOptionValueInListOfStrings, strings, option) ? $author$project$Option$selectOption(option) : $author$project$Option$deselectOption(option);
			},
			options);
	});
var $author$project$Option$stringToOptionValue = function (string) {
	return $author$project$Option$OptionValue(string);
};
var $author$project$Main$init = function (flags) {
	var maxDropdownItems = $author$project$PositiveInt$new(flags.g);
	var initialValueStr = $elm$core$String$trim(flags.aT);
	var allowCustomOptions = flags.aA ? 0 : 1;
	var selectionMode = flags.aB ? $author$project$SelectionMode$MultiSelect(allowCustomOptions) : $author$project$SelectionMode$SingleSelect(allowCustomOptions);
	var initialValues = function () {
		if (initialValueStr === '') {
			return _List_Nil;
		} else {
			if (!selectionMode.$) {
				return _List_fromArray(
					[initialValueStr]);
			} else {
				return A2($elm$core$String$split, ',', initialValueStr);
			}
		}
	}();
	var _v0 = function () {
		var _v1 = A2($elm$json$Json$Decode$decodeString, $author$project$Option$optionsDecoder, flags.aN);
		if (!_v1.$) {
			var options = _v1.a;
			if (!selectionMode.$) {
				var _v3 = $elm$core$List$head(initialValues);
				if (!_v3.$) {
					var initialValueStr_ = _v3.a;
					if (A2(
						$author$project$Option$isOptionInListOfOptionsByValue,
						$author$project$Option$stringToOptionValue(initialValueStr_),
						options)) {
						var optionsWithUniqueValues = A2($elm_community$list_extra$List$Extra$uniqueBy, $author$project$Option$getOptionValueAsString, options);
						return _Utils_Tuple2(
							A2($author$project$Option$selectOptionsInOptionsListByString, initialValues, optionsWithUniqueValues),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								$author$project$Option$selectOption(
									A2($author$project$Option$newOption, initialValueStr_, $elm$core$Maybe$Nothing)),
								options),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					var optionsWithUniqueValues = A2($elm_community$list_extra$List$Extra$uniqueBy, $author$project$Option$getOptionValueAsString, options);
					return _Utils_Tuple2(optionsWithUniqueValues, $elm$core$Platform$Cmd$none);
				}
			} else {
				var optionsWithInitialValues = A2(
					$author$project$Option$addAndSelectOptionsInOptionsListByString,
					initialValues,
					A2(
						$elm$core$List$filter,
						A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$Option$isEmptyOption),
						options));
				return _Utils_Tuple2(optionsWithInitialValues, $elm$core$Platform$Cmd$none);
			}
		} else {
			var error = _v1.a;
			return _Utils_Tuple2(
				_List_Nil,
				$author$project$Ports$errorMessage(
					$elm$json$Json$Decode$errorToString(error)));
		}
	}();
	var optionsWithInitialValueSelected = _v0.a;
	var errorCmd = _v0.b;
	return _Utils_Tuple2(
		{
			a2: false,
			m: flags.m,
			q: false,
			bc: initialValues,
			g: maxDropdownItems,
			a: optionsWithInitialValueSelected,
			e: A2($author$project$Main$figureOutWhichOptionsToShow, maxDropdownItems, optionsWithInitialValueSelected),
			G: flags.G,
			I: function () {
				if (flags.aH) {
					return 1;
				} else {
					if (!selectionMode.$) {
						return 2;
					} else {
						return $author$project$Option$hasSelectedOption(optionsWithInitialValueSelected) ? 3 : 0;
					}
				}
			}(),
			E: '',
			l: selectionMode,
			af: false,
			ag: flags.ag,
			ay: 45,
			az: 100
		},
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					errorCmd,
					$author$project$Ports$muchSelectIsReady(0)
				])));
};
var $author$project$Main$AddOptions = function (a) {
	return {$: 11, a: a};
};
var $author$project$Main$AllowCustomOptionsChanged = function (a) {
	return {$: 18, a: a};
};
var $author$project$Main$DeselectOption = function (a) {
	return {$: 14, a: a};
};
var $author$project$Main$DisabledAttributeChanged = function (a) {
	return {$: 19, a: a};
};
var $author$project$Main$LoadingAttributeChanged = function (a) {
	return {$: 16, a: a};
};
var $author$project$Main$MaxDropdownItemsChanged = function (a) {
	return {$: 17, a: a};
};
var $author$project$Main$OptionsChanged = function (a) {
	return {$: 10, a: a};
};
var $author$project$Main$PlaceholderAttributeChanged = function (a) {
	return {$: 15, a: a};
};
var $author$project$Main$RemoveOptions = function (a) {
	return {$: 12, a: a};
};
var $author$project$Main$SelectOption = function (a) {
	return {$: 13, a: a};
};
var $author$project$Main$ValueCasingWidthUpdate = function (a) {
	return {$: 25, a: a};
};
var $author$project$Main$ValueChanged = function (a) {
	return {$: 9, a: a};
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Ports$addOptionsReceiver = _Platform_incomingPort('addOptionsReceiver', $elm$json$Json$Decode$value);
var $author$project$Ports$allowCustomOptionsReceiver = _Platform_incomingPort('allowCustomOptionsReceiver', $elm$json$Json$Decode$bool);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $author$project$Ports$deselectOptionReceiver = _Platform_incomingPort('deselectOptionReceiver', $elm$json$Json$Decode$value);
var $author$project$Ports$disableChangedReceiver = _Platform_incomingPort('disableChangedReceiver', $elm$json$Json$Decode$bool);
var $author$project$Ports$loadingChangedReceiver = _Platform_incomingPort('loadingChangedReceiver', $elm$json$Json$Decode$bool);
var $author$project$Ports$maxDropdownItemsChangedReceiver = _Platform_incomingPort('maxDropdownItemsChangedReceiver', $elm$json$Json$Decode$int);
var $author$project$Ports$optionsChangedReceiver = _Platform_incomingPort('optionsChangedReceiver', $elm$json$Json$Decode$value);
var $author$project$Ports$placeholderChangedReceiver = _Platform_incomingPort('placeholderChangedReceiver', $elm$json$Json$Decode$string);
var $author$project$Ports$removeOptionsReceiver = _Platform_incomingPort('removeOptionsReceiver', $elm$json$Json$Decode$value);
var $author$project$Ports$selectOptionReceiver = _Platform_incomingPort('selectOptionReceiver', $elm$json$Json$Decode$value);
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Ports$valueCasingDimensionsChangedReceiver = _Platform_incomingPort(
	'valueCasingDimensionsChangedReceiver',
	A2(
		$elm$json$Json$Decode$andThen,
		function (width) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (height) {
					return $elm$json$Json$Decode$succeed(
						{cc: height, cW: width});
				},
				A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float));
		},
		A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float)));
var $author$project$Ports$valueChangedReceiver = _Platform_incomingPort('valueChangedReceiver', $elm$json$Json$Decode$value);
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Ports$valueChangedReceiver($author$project$Main$ValueChanged),
				$author$project$Ports$addOptionsReceiver($author$project$Main$AddOptions),
				$author$project$Ports$removeOptionsReceiver($author$project$Main$RemoveOptions),
				$author$project$Ports$placeholderChangedReceiver($author$project$Main$PlaceholderAttributeChanged),
				$author$project$Ports$loadingChangedReceiver($author$project$Main$LoadingAttributeChanged),
				$author$project$Ports$disableChangedReceiver($author$project$Main$DisabledAttributeChanged),
				$author$project$Ports$optionsChangedReceiver($author$project$Main$OptionsChanged),
				$author$project$Ports$maxDropdownItemsChangedReceiver($author$project$Main$MaxDropdownItemsChanged),
				$author$project$Ports$allowCustomOptionsReceiver($author$project$Main$AllowCustomOptionsChanged),
				$author$project$Ports$valueCasingDimensionsChangedReceiver($author$project$Main$ValueCasingWidthUpdate),
				$author$project$Ports$selectOptionReceiver($author$project$Main$SelectOption),
				$author$project$Ports$deselectOptionReceiver($author$project$Main$DeselectOption)
			]));
};
var $author$project$Option$addAdditionalOptionsToOptionList = F2(
	function (currentOptions, newOptions) {
		return _Utils_ap(
			A2(
				$elm$core$List$filter,
				function (_new) {
					return !A2($author$project$Option$optionListContainsOptionWithValue, _new, currentOptions);
				},
				newOptions),
			currentOptions);
	});
var $author$project$Ports$blurInput = _Platform_outgoingPort(
	'blurInput',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Option$deselectAllOptionsInOptionsList = function (options) {
	return A2($elm$core$List$map, $author$project$Option$deselectOption, options);
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$Ports$customOptionSelected = _Platform_outgoingPort(
	'customOptionSelected',
	$elm$json$Json$Encode$list($elm$json$Json$Encode$string));
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Option$isCustomOption = function (option) {
	switch (option.$) {
		case 0:
			return false;
		case 1:
			return true;
		default:
			return false;
	}
};
var $author$project$Option$customOptions = function (options) {
	return A2($elm$core$List$filter, $author$project$Option$isCustomOption, options);
};
var $author$project$Option$customSelectedOptions = A2($elm$core$Basics$composeR, $author$project$Option$customOptions, $author$project$Option$selectedOptions);
var $author$project$Ports$optionSelected = _Platform_outgoingPort(
	'optionSelected',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(a),
					$elm$json$Json$Encode$string(b)
				]));
	});
var $author$project$Option$optionToValueLabelTuple = function (option) {
	return _Utils_Tuple2(
		$author$project$Option$getOptionValueAsString(option),
		$author$project$OptionLabel$optionLabelToString(
			$author$project$Option$getOptionLabel(option)));
};
var $author$project$Option$optionsValues = function (options) {
	return A2($elm$core$List$map, $author$project$Option$getOptionValueAsString, options);
};
var $author$project$Option$selectedOptionsToTuple = function (options) {
	return A2(
		$elm$core$List$map,
		$author$project$Option$optionToValueLabelTuple,
		$author$project$Option$selectedOptions(options));
};
var $author$project$Ports$valueChanged = _Platform_outgoingPort(
	'valueChanged',
	$elm$json$Json$Encode$list(
		function ($) {
			var a = $.a;
			var b = $.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(a),
						$elm$json$Json$Encode$string(b)
					]));
		}));
var $author$project$Ports$valueCleared = _Platform_outgoingPort(
	'valueCleared',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Main$makeCommandMessagesWhenValuesChanges = F2(
	function (selectedOptions, maybeSelectedValue) {
		var selectedCustomOptions = $author$project$Option$customSelectedOptions(selectedOptions);
		var optionSelectedCmd = function () {
			if (!maybeSelectedValue.$) {
				var selectedValue = maybeSelectedValue.a;
				var _v1 = A2($author$project$Option$findOptionByOptionValue, selectedValue, selectedOptions);
				if (!_v1.$) {
					var option = _v1.a;
					return $author$project$Ports$optionSelected(
						$author$project$Option$optionToValueLabelTuple(option));
				} else {
					return $elm$core$Platform$Cmd$none;
				}
			} else {
				return $elm$core$Platform$Cmd$none;
			}
		}();
		var customOptionCmd = $elm$core$List$isEmpty(selectedCustomOptions) ? $elm$core$Platform$Cmd$none : $author$project$Ports$customOptionSelected(
			$author$project$Option$optionsValues(selectedCustomOptions));
		var clearCmd = $elm$core$List$isEmpty(selectedOptions) ? $author$project$Ports$valueCleared(0) : $elm$core$Platform$Cmd$none;
		return $elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					$author$project$Ports$valueChanged(
					$author$project$Option$selectedOptionsToTuple(selectedOptions)),
					customOptionCmd,
					clearCmd,
					optionSelectedCmd
				]));
	});
var $author$project$Ports$optionDeselected = _Platform_outgoingPort(
	'optionDeselected',
	$elm$json$Json$Encode$list(
		function ($) {
			var a = $.a;
			var b = $.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(a),
						$elm$json$Json$Encode$string(b)
					]));
		}));
var $author$project$Main$clearAllSelectedOption = function (model) {
	var newOptions = $author$project$Option$deselectAllOptionsInOptionsList(model.a);
	var deselectedItems = $elm$core$List$isEmpty(
		$author$project$Option$selectedOptionsToTuple(model.a)) ? _List_Nil : $author$project$Option$selectedOptionsToTuple(model.a);
	var deselectEventMsg = $elm$core$List$isEmpty(deselectedItems) ? $elm$core$Platform$Cmd$none : $author$project$Ports$optionDeselected(deselectedItems);
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{
				a: $author$project$Option$deselectAllOptionsInOptionsList(newOptions),
				e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, newOptions),
				I: 0,
				E: ''
			}),
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2($author$project$Main$makeCommandMessagesWhenValuesChanges, _List_Nil, $elm$core$Maybe$Nothing),
					deselectEventMsg
				])));
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Option$setOptionDisplay = F2(
	function (optionDisplay, option) {
		switch (option.$) {
			case 0:
				var optionLabel = option.b;
				var optionValue = option.c;
				var optionDescription = option.d;
				var optionGroup = option.e;
				var search = option.f;
				return A6($author$project$Option$Option, optionDisplay, optionLabel, optionValue, optionDescription, optionGroup, search);
			case 1:
				var optionLabel = option.b;
				var optionValue = option.c;
				var search = option.d;
				return A4($author$project$Option$CustomOption, optionDisplay, optionLabel, optionValue, search);
			default:
				var optionLabel = option.b;
				return A2($author$project$Option$EmptyOption, optionDisplay, optionLabel);
		}
	});
var $author$project$Option$deselectAllSelectedHighlightedOptions = function (options) {
	return A2(
		$elm$core$List$map,
		function (option_) {
			switch (option_.$) {
				case 0:
					var optionDisplay = option_.a;
					switch (optionDisplay) {
						case 0:
							return option_;
						case 1:
							return option_;
						case 2:
							return option_;
						case 3:
							return A2($author$project$Option$setOptionDisplay, 0, option_);
						case 4:
							return option_;
						default:
							return option_;
					}
				case 1:
					var optionDisplay = option_.a;
					switch (optionDisplay) {
						case 0:
							return option_;
						case 1:
							return option_;
						case 2:
							return option_;
						case 3:
							return A2($author$project$Option$setOptionDisplay, 0, option_);
						case 4:
							return option_;
						default:
							return option_;
					}
				default:
					return option_;
			}
		},
		options);
};
var $author$project$Option$optionValuesEqual = F2(
	function (option, optionValue) {
		return _Utils_eq(
			$author$project$Option$getOptionValue(option),
			optionValue);
	});
var $author$project$Option$deselectOptionInListByOptionValue = F2(
	function (value, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				return A2($author$project$Option$optionValuesEqual, option_, value) ? $author$project$Option$deselectOption(option_) : option_;
			},
			options);
	});
var $author$project$Ports$focusInput = _Platform_outgoingPort(
	'focusInput',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Option$highlightOption = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var description = option.d;
			var group = option.e;
			var search = option.f;
			switch (display) {
				case 0:
					return A6($author$project$Option$Option, 4, label, value, description, group, search);
				case 1:
					return A6($author$project$Option$Option, 1, label, value, description, group, search);
				case 2:
					return A6($author$project$Option$Option, 3, label, value, description, group, search);
				case 3:
					return A6($author$project$Option$Option, 3, label, value, description, group, search);
				case 4:
					return A6($author$project$Option$Option, 4, label, value, description, group, search);
				default:
					return A6($author$project$Option$Option, 5, label, value, description, group, search);
			}
		case 1:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var search = option.d;
			switch (display) {
				case 0:
					return A4($author$project$Option$CustomOption, 4, label, value, search);
				case 1:
					return A4($author$project$Option$CustomOption, 1, label, value, search);
				case 2:
					return A4($author$project$Option$CustomOption, 3, label, value, search);
				case 3:
					return A4($author$project$Option$CustomOption, 3, label, value, search);
				case 4:
					return A4($author$project$Option$CustomOption, 4, label, value, search);
				default:
					return A4($author$project$Option$CustomOption, 5, label, value, search);
			}
		default:
			var display = option.a;
			var label = option.b;
			switch (display) {
				case 0:
					return A2($author$project$Option$EmptyOption, 4, label);
				case 1:
					return A2($author$project$Option$EmptyOption, 1, label);
				case 2:
					return A2($author$project$Option$EmptyOption, 3, label);
				case 3:
					return A2($author$project$Option$EmptyOption, 3, label);
				case 4:
					return A2($author$project$Option$EmptyOption, 4, label);
				default:
					return A2($author$project$Option$EmptyOption, 5, label);
			}
	}
};
var $author$project$Option$removeHighlightOption = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var description = option.d;
			var group = option.e;
			var search = option.f;
			switch (display) {
				case 0:
					return A6($author$project$Option$Option, 0, label, value, description, group, search);
				case 1:
					return A6($author$project$Option$Option, 1, label, value, description, group, search);
				case 2:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 3:
					return A6($author$project$Option$Option, 2, label, value, description, group, search);
				case 4:
					return A6($author$project$Option$Option, 0, label, value, description, group, search);
				default:
					return A6($author$project$Option$Option, 5, label, value, description, group, search);
			}
		case 1:
			var display = option.a;
			var label = option.b;
			var value = option.c;
			var search = option.d;
			switch (display) {
				case 0:
					return A4($author$project$Option$CustomOption, 0, label, value, search);
				case 1:
					return A4($author$project$Option$CustomOption, 1, label, value, search);
				case 2:
					return A4($author$project$Option$CustomOption, 2, label, value, search);
				case 3:
					return A4($author$project$Option$CustomOption, 2, label, value, search);
				case 4:
					return A4($author$project$Option$CustomOption, 0, label, value, search);
				default:
					return A4($author$project$Option$CustomOption, 5, label, value, search);
			}
		default:
			var display = option.a;
			var label = option.b;
			switch (display) {
				case 0:
					return A2($author$project$Option$EmptyOption, 0, label);
				case 1:
					return A2($author$project$Option$EmptyOption, 1, label);
				case 2:
					return A2($author$project$Option$EmptyOption, 2, label);
				case 4:
					return A2($author$project$Option$EmptyOption, 0, label);
				case 5:
					return A2($author$project$Option$EmptyOption, 5, label);
				default:
					return A2($author$project$Option$EmptyOption, 3, label);
			}
	}
};
var $author$project$Option$highlightOptionInListByValue = F2(
	function (value, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				return A2($author$project$Option$optionValuesEqual, option_, value) ? $author$project$Option$highlightOption(option_) : $author$project$Option$removeHighlightOption(option_);
			},
			options);
	});
var $author$project$Ports$inputKeyUp = _Platform_outgoingPort('inputKeyUp', $elm$json$Json$Encode$string);
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Option$optionIsHighlightable = function (option) {
	switch (option.$) {
		case 0:
			var display = option.a;
			switch (display) {
				case 0:
					return true;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return false;
				default:
					return false;
			}
		case 1:
			var display = option.a;
			switch (display) {
				case 0:
					return true;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return false;
				default:
					return false;
			}
		default:
			var display = option.a;
			switch (display) {
				case 0:
					return true;
				case 1:
					return false;
				case 2:
					return false;
				case 3:
					return false;
				case 4:
					return false;
				default:
					return false;
			}
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm_community$list_extra$List$Extra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $author$project$Option$findClosestHighlightableOptionGoingDown = F2(
	function (index, options) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			$author$project$Option$optionIsHighlightable,
			A2($elm_community$list_extra$List$Extra$splitAt, index, options).b);
	});
var $author$project$Option$highlightOptionInList = F2(
	function (option, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				return _Utils_eq(option, option_) ? $author$project$Option$highlightOption(option_) : $author$project$Option$removeHighlightOption(option_);
			},
			options);
	});
var $author$project$Option$moveHighlightedOptionDown = function (options) {
	var maybeLowerSibling = A2(
		$elm$core$Maybe$andThen,
		function (index) {
			return A2($author$project$Option$findClosestHighlightableOptionGoingDown, index, options);
		},
		$author$project$Option$findHighlightedOrSelectedOptionIndex(options));
	if (!maybeLowerSibling.$) {
		var option = maybeLowerSibling.a;
		return A2($author$project$Option$highlightOptionInList, option, options);
	} else {
		var _v1 = $elm$core$List$head(options);
		if (!_v1.$) {
			var firstOption = _v1.a;
			return A2($author$project$Option$highlightOptionInList, firstOption, options);
		} else {
			return options;
		}
	}
};
var $author$project$Option$findClosestHighlightableOptionGoingUp = F2(
	function (index, options) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			$author$project$Option$optionIsHighlightable,
			$elm$core$List$reverse(
				A2($elm_community$list_extra$List$Extra$splitAt, index, options).a));
	});
var $author$project$Option$moveHighlightedOptionUp = function (options) {
	var maybeHigherSibling = A2(
		$elm$core$Maybe$andThen,
		function (index) {
			return A2($author$project$Option$findClosestHighlightableOptionGoingUp, index, options);
		},
		$author$project$Option$findHighlightedOrSelectedOptionIndex(options));
	if (!maybeHigherSibling.$) {
		var option = maybeHigherSibling.a;
		return A2($author$project$Option$highlightOptionInList, option, options);
	} else {
		var _v1 = $elm$core$List$head(options);
		if (!_v1.$) {
			var firstOption = _v1.a;
			return A2($author$project$Option$highlightOptionInList, firstOption, options);
		} else {
			return options;
		}
	}
};
var $author$project$Option$removeHighlightOptionInList = F2(
	function (value, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				return A2($author$project$Option$optionValuesEqual, option_, value) ? $author$project$Option$removeHighlightOption(option_) : option_;
			},
			options);
	});
var $author$project$Option$removeOptionsFromOptionList = F2(
	function (options, optionsToRemove) {
		return A2(
			$elm$core$List$filter,
			function (option) {
				return !A2($author$project$Option$optionListContainsOptionWithValue, option, optionsToRemove);
			},
			options);
	});
var $author$project$Ports$scrollDropdownToElement = _Platform_outgoingPort('scrollDropdownToElement', $elm$json$Json$Encode$string);
var $author$project$Option$selectEmptyOption = function (options) {
	return A2(
		$elm$core$List$map,
		function (option_) {
			switch (option_.$) {
				case 0:
					return $author$project$Option$deselectOption(option_);
				case 1:
					return $author$project$Option$deselectOption(option_);
				default:
					return $author$project$Option$selectOption(option_);
			}
		},
		options);
};
var $author$project$Option$setLabelWithString = F3(
	function (string, maybeCleanString, option) {
		switch (option.$) {
			case 0:
				var optionDisplay = option.a;
				var optionValue = option.c;
				var description = option.d;
				var group = option.e;
				var search = option.f;
				return A6(
					$author$project$Option$Option,
					optionDisplay,
					A3($author$project$OptionLabel$OptionLabel, string, maybeCleanString, $author$project$SortRank$NoSortRank),
					optionValue,
					description,
					group,
					search);
			case 1:
				var optionDisplay = option.a;
				var search = option.d;
				return A4(
					$author$project$Option$CustomOption,
					optionDisplay,
					A3($author$project$OptionLabel$OptionLabel, string, maybeCleanString, $author$project$SortRank$NoSortRank),
					$author$project$Option$OptionValue(string),
					search);
			default:
				var optionDisplay = option.a;
				return A2(
					$author$project$Option$EmptyOption,
					optionDisplay,
					A3($author$project$OptionLabel$OptionLabel, string, maybeCleanString, $author$project$SortRank$NoSortRank));
		}
	});
var $author$project$Option$selectOptionInListByOptionValue = F2(
	function (value, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				if (A2($author$project$Option$optionValuesEqual, option_, value)) {
					switch (option_.$) {
						case 0:
							return $author$project$Option$selectOption(option_);
						case 1:
							if (!value.$) {
								var valueStr = value.a;
								return A3(
									$author$project$Option$setLabelWithString,
									valueStr,
									$elm$core$Maybe$Nothing,
									$author$project$Option$selectOption(option_));
							} else {
								return $author$project$Option$selectOption(option_);
							}
						default:
							return $author$project$Option$selectOption(option_);
					}
				} else {
					return option_;
				}
			},
			options);
	});
var $author$project$Option$selectSingleOptionInList = F2(
	function (value, options) {
		return A2(
			$elm$core$List$map,
			function (option_) {
				if (A2($author$project$Option$optionValuesEqual, option_, value)) {
					switch (option_.$) {
						case 0:
							return $author$project$Option$selectOption(option_);
						case 1:
							var optionValue = option_.c;
							if (!optionValue.$) {
								var valueStr = optionValue.a;
								return A3(
									$author$project$Option$setLabelWithString,
									valueStr,
									$elm$core$Maybe$Nothing,
									$author$project$Option$selectOption(option_));
							} else {
								return $author$project$Option$selectOption(option_);
							}
						default:
							return $author$project$Option$selectOption(option_);
					}
				} else {
					return $author$project$Option$deselectOption(option_);
				}
			},
			options);
	});
var $author$project$Option$selectHighlightedOption = F2(
	function (selectionMode, options) {
		return function (maybeOption) {
			if (!maybeOption.$) {
				var option = maybeOption.a;
				switch (option.$) {
					case 0:
						var value = option.c;
						if (selectionMode.$ === 1) {
							return A2($author$project$Option$selectOptionInListByOptionValue, value, options);
						} else {
							return A2($author$project$Option$selectSingleOptionInList, value, options);
						}
					case 1:
						var value = option.c;
						if (selectionMode.$ === 1) {
							return A2($author$project$Option$selectOptionInListByOptionValue, value, options);
						} else {
							return A2($author$project$Option$selectSingleOptionInList, value, options);
						}
					default:
						if (selectionMode.$ === 1) {
							return $author$project$Option$selectEmptyOption(options);
						} else {
							return $author$project$Option$selectEmptyOption(options);
						}
				}
			} else {
				return options;
			}
		}(
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (option) {
						return $author$project$Option$isOptionHighlighted(option);
					},
					options)));
	});
var $author$project$SelectionMode$setAllowCustomOptionsWithBool = F2(
	function (bool, mode) {
		if (!mode.$) {
			return bool ? $author$project$SelectionMode$SingleSelect(0) : $author$project$SelectionMode$SingleSelect(1);
		} else {
			return bool ? $author$project$SelectionMode$MultiSelect(0) : $author$project$SelectionMode$MultiSelect(1);
		}
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm_community$list_extra$List$Extra$gatherWith = F2(
	function (testFn, list) {
		var helper = F2(
			function (scattered, gathered) {
				if (!scattered.b) {
					return $elm$core$List$reverse(gathered);
				} else {
					var toGather = scattered.a;
					var population = scattered.b;
					var _v1 = A2(
						$elm$core$List$partition,
						testFn(toGather),
						population);
					var gathering = _v1.a;
					var remaining = _v1.b;
					return A2(
						helper,
						remaining,
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(toGather, gathering),
							gathered));
				}
			});
		return A2(helper, list, _List_Nil);
	});
var $author$project$SortRank$getAutoIndexForSorting = function (sortRank) {
	switch (sortRank.$) {
		case 0:
			var positiveInt = sortRank.a;
			return $author$project$PositiveInt$toInt(positiveInt);
		case 1:
			return 100000000;
		default:
			return 100000000;
	}
};
var $author$project$OptionLabel$getSortRank = function (optionLabel) {
	var sortRank = optionLabel.c;
	return sortRank;
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Option$sortOptionsByLabel = function (options) {
	return A2(
		$elm$core$List$sortBy,
		function (option) {
			return $author$project$SortRank$getAutoIndexForSorting(
				$author$project$OptionLabel$getSortRank(
					$author$project$Option$getOptionLabel(option)));
		},
		A2(
			$elm$core$List$sortBy,
			function (option) {
				return $author$project$OptionLabel$optionLabelToString(
					$author$project$Option$getOptionLabel(option));
			},
			options));
};
var $author$project$Option$sortOptionsByGroupAndLabel = function (options) {
	return $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			function (options_) {
				return $author$project$Option$sortOptionsByLabel(options_);
			},
			A2(
				$elm$core$List$map,
				function (_v0) {
					var option_ = _v0.a;
					var options_ = _v0.b;
					return A2(
						$elm$core$List$append,
						_List_fromArray(
							[option_]),
						options_);
				},
				A2(
					$elm_community$list_extra$List$Extra$gatherWith,
					F2(
						function (optionA, optionB) {
							return _Utils_eq(
								$author$project$Option$getOptionGroup(optionA),
								$author$project$Option$getOptionGroup(optionB));
						}),
					options))));
};
var $author$project$Option$getMaybeOptionSearchFilter = function (option) {
	switch (option.$) {
		case 0:
			var maybeOptionSearchFilter = option.f;
			return maybeOptionSearchFilter;
		case 1:
			var maybeOptionSearchFilter = option.d;
			return maybeOptionSearchFilter;
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Option$sortOptionsByTotalScore = function (options) {
	return A2(
		$elm$core$List$sortBy,
		function (option) {
			return A2(
				$elm$core$Maybe$withDefault,
				100000,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.cT;
					},
					$author$project$Option$getMaybeOptionSearchFilter(option)));
		},
		options);
};
var $author$project$OptionSearchFilter$new = F4(
	function (totalScore, searchResult, labelTokens, descriptionTokens) {
		return {b3: descriptionTokens, cl: labelTokens, bF: searchResult, cT: totalScore};
	});
var $author$project$Option$optionDescriptionToString = function (optionDescription) {
	if (!optionDescription.$) {
		var string = optionDescription.a;
		return string;
	} else {
		return '';
	}
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$Option$optionDescriptionToSearchString = function (optionDescription) {
	if (!optionDescription.$) {
		var description = optionDescription.a;
		var maybeCleanDescription = optionDescription.b;
		if (!maybeCleanDescription.$) {
			var cleanDescription = maybeCleanDescription.a;
			return cleanDescription;
		} else {
			return $elm$core$String$toLower(description);
		}
	} else {
		return '';
	}
};
var $author$project$OptionLabel$optionLabelToSearchString = function (optionLabel) {
	var string = optionLabel.a;
	var maybeCleanString = optionLabel.b;
	if (!maybeCleanString.$) {
		var cleanString = maybeCleanString.a;
		return cleanString;
	} else {
		return $elm$core$String$toLower(string);
	}
};
var $tripokey$elm_fuzzy$Fuzzy$Match = F4(
	function (score, offset, length, keys) {
		return {cj: keys, cm: length, cB: offset, bE: score};
	});
var $tripokey$elm_fuzzy$Fuzzy$Result = F2(
	function (score, matches) {
		return {co: matches, bE: score};
	});
var $tripokey$elm_fuzzy$Fuzzy$ConfigModel = F4(
	function (addPenalty, movePenalty, removePenalty, insertPenalty) {
		return {al: addPenalty, ar: insertPenalty, at: movePenalty, aw: removePenalty};
	});
var $tripokey$elm_fuzzy$Fuzzy$defaultConfig = A4($tripokey$elm_fuzzy$Fuzzy$ConfigModel, 10, 1000, 10000, 1);
var $tripokey$elm_fuzzy$Fuzzy$dissect = F2(
	function (separators, strings) {
		dissect:
		while (true) {
			if (!separators.b) {
				return strings;
			} else {
				var head = separators.a;
				var tail = separators.b;
				var dissectEntry = function (entry) {
					var separatorLength = $elm$core$String$length(head);
					var slice = F2(
						function (index, _v1) {
							var prevIndex = _v1.a;
							var sum = _v1.b;
							var separatorSlice = _List_fromArray(
								[
									A3($elm$core$String$slice, index, index + separatorLength, entry)
								]);
							var precedingSlice = _Utils_eq(prevIndex, index) ? _List_Nil : _List_fromArray(
								[
									A3($elm$core$String$slice, prevIndex, index, entry)
								]);
							return _Utils_Tuple2(
								index + separatorLength,
								_Utils_ap(
									sum,
									_Utils_ap(precedingSlice, separatorSlice)));
						});
					var indexes = A2($elm$core$String$indexes, head, entry);
					var result = A3(
						$elm$core$List$foldl,
						slice,
						_Utils_Tuple2(0, _List_Nil),
						indexes);
					var lastIndex = result.a;
					var first = result.b;
					var entryLength = $elm$core$String$length(entry);
					var last = _Utils_eq(lastIndex, entryLength) ? _List_Nil : _List_fromArray(
						[
							A3($elm$core$String$slice, lastIndex, entryLength, entry)
						]);
					return _Utils_ap(first, last);
				};
				var dissected = A3(
					$elm$core$List$foldl,
					F2(
						function (e, s) {
							return _Utils_ap(
								s,
								dissectEntry(e));
						}),
					_List_Nil,
					strings);
				var $temp$separators = tail,
					$temp$strings = dissected;
				separators = $temp$separators;
				strings = $temp$strings;
				continue dissect;
			}
		}
	});
var $elm$core$String$foldl = _String_foldl;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $tripokey$elm_fuzzy$Fuzzy$initialModel = _List_Nil;
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $tripokey$elm_fuzzy$Fuzzy$quickSort = function (entries) {
	if (!entries.b) {
		return _Utils_Tuple2(0, _List_Nil);
	} else {
		var head = entries.a;
		var tail = entries.b;
		var partition = A2(
			$elm$core$List$partition,
			function (e) {
				return _Utils_cmp(e, head) < 0;
			},
			tail);
		var smaller = $tripokey$elm_fuzzy$Fuzzy$quickSort(partition.a);
		var penalty = $elm$core$List$isEmpty(smaller.b) ? 0 : 1;
		var larger = $tripokey$elm_fuzzy$Fuzzy$quickSort(partition.b);
		return _Utils_Tuple2(
			(smaller.a + penalty) + larger.a,
			_Utils_ap(
				smaller.b,
				_Utils_ap(
					_List_fromArray(
						[head]),
					larger.b)));
	}
};
var $tripokey$elm_fuzzy$Fuzzy$distance = F3(
	function (config, needle, hay) {
		var accumulateInsertPenalty = F2(
			function (elem, result) {
				if (!result.a.$) {
					var prev = result.a.a;
					var score = result.b;
					return _Utils_Tuple2(
						$elm$core$Maybe$Just(elem),
						((elem - 1) - prev) + score);
				} else {
					var _v2 = result.a;
					var score = result.b;
					return _Utils_Tuple2(
						$elm$core$Maybe$Just(elem),
						score);
				}
			});
		var accumulate = F2(
			function (c, indexList) {
				var indexes = A2(
					$elm$core$String$indexes,
					$elm$core$String$fromChar(c),
					hay);
				var hayIndex = $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (e) {
							return !A2($elm$core$List$member, e, indexList);
						},
						indexes));
				if (!hayIndex.$) {
					var v = hayIndex.a;
					return _Utils_ap(
						indexList,
						_List_fromArray(
							[v]));
				} else {
					return indexList;
				}
			});
		var accumulated = A3($elm$core$String$foldl, accumulate, $tripokey$elm_fuzzy$Fuzzy$initialModel, needle);
		var hPenalty = ($elm$core$String$length(hay) - $elm$core$List$length(accumulated)) * config.al;
		var nPenalty = ($elm$core$String$length(needle) - $elm$core$List$length(accumulated)) * config.aw;
		var sorted = $tripokey$elm_fuzzy$Fuzzy$quickSort(accumulated);
		var iPenalty = A3(
			$elm$core$List$foldl,
			accumulateInsertPenalty,
			_Utils_Tuple2($elm$core$Maybe$Nothing, 0),
			sorted.b).b * config.ar;
		var mPenalty = sorted.a * config.at;
		return A4(
			$tripokey$elm_fuzzy$Fuzzy$Match,
			((mPenalty + hPenalty) + nPenalty) + iPenalty,
			0,
			$elm$core$String$length(hay),
			sorted.b);
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $tripokey$elm_fuzzy$Fuzzy$padHays = F2(
	function (ns, hs) {
		return _Utils_ap(
			hs,
			A2(
				$elm$core$List$repeat,
				ns - $elm$core$List$length(hs),
				''));
	});
var $tripokey$elm_fuzzy$Fuzzy$reduceLeft = F3(
	function (ns, c, hs) {
		return _Utils_Tuple2(
			A3(
				$elm$core$List$foldl,
				F2(
					function (e, sum) {
						return $elm$core$String$length(e) + sum;
					}),
				0,
				A2($elm$core$List$take, c, hs)),
			A2($elm$core$List$drop, c, hs));
	});
var $tripokey$elm_fuzzy$Fuzzy$reduceRight = F3(
	function (ns, c, hs) {
		return A2(
			$elm$core$List$take,
			$elm$core$List$length(hs) - ((ns - c) - 1),
			hs);
	});
var $tripokey$elm_fuzzy$Fuzzy$match = F4(
	function (configs, separators, needle, hay) {
		var reduceHays = F3(
			function (ns, c, hs) {
				return A3(
					$tripokey$elm_fuzzy$Fuzzy$reduceLeft,
					ns,
					c,
					A3(
						$tripokey$elm_fuzzy$Fuzzy$reduceRight,
						ns,
						c,
						A2($tripokey$elm_fuzzy$Fuzzy$padHays, ns, hs)));
			});
		var needles = A2(
			$tripokey$elm_fuzzy$Fuzzy$dissect,
			separators,
			_List_fromArray(
				[needle]));
		var initialResult = A2($tripokey$elm_fuzzy$Fuzzy$Result, 0, _List_Nil);
		var hays = A2(
			$tripokey$elm_fuzzy$Fuzzy$dissect,
			separators,
			_List_fromArray(
				[hay]));
		var accumulateConfig = F2(
			function (c, sum) {
				switch (c.$) {
					case 0:
						var val = c.a;
						return _Utils_update(
							sum,
							{al: val});
					case 1:
						var val = c.a;
						return _Utils_update(
							sum,
							{aw: val});
					case 2:
						var val = c.a;
						return _Utils_update(
							sum,
							{at: val});
					default:
						var val = c.a;
						return _Utils_update(
							sum,
							{ar: val});
				}
			});
		var config = A3($elm$core$List$foldl, accumulateConfig, $tripokey$elm_fuzzy$Fuzzy$defaultConfig, configs);
		var minScore = F2(
			function (n, _v2) {
				var offset = _v2.a;
				var hs = _v2.b;
				var initialPenalty = ((($elm$core$String$length(n) * config.aw) + ($elm$core$String$length(n) * config.at)) + ($elm$core$String$length(hay) * config.al)) + (($elm$core$String$length(hay) * $elm$core$String$length(n)) * config.ar);
				var initialMatch = A4($tripokey$elm_fuzzy$Fuzzy$Match, initialPenalty, offset, 0, _List_Nil);
				var accumulateMatch = F2(
					function (e, _v1) {
						var prev = _v1.a;
						var prevOffset = _v1.b;
						var newOffset = prevOffset + $elm$core$String$length(e);
						var eDistance = A3($tripokey$elm_fuzzy$Fuzzy$distance, config, n, e);
						var newMatch = (_Utils_cmp(eDistance.bE, prev.bE) < 0) ? _Utils_update(
							eDistance,
							{cB: prevOffset}) : prev;
						return _Utils_Tuple2(newMatch, newOffset);
					});
				return A3(
					$elm$core$List$foldl,
					accumulateMatch,
					_Utils_Tuple2(initialMatch, offset),
					hs).a;
			});
		var accumulateResult = F2(
			function (n, _v0) {
				var prev = _v0.a;
				var num = _v0.b;
				var matchResult = A2(
					minScore,
					n,
					A3(
						reduceHays,
						$elm$core$List$length(needles),
						num,
						hays));
				var newResult = _Utils_update(
					prev,
					{
						co: _Utils_ap(
							prev.co,
							_List_fromArray(
								[matchResult])),
						bE: matchResult.bE + prev.bE
					});
				return _Utils_Tuple2(newResult, num + 1);
			});
		return A3(
			$elm$core$List$foldl,
			accumulateResult,
			_Utils_Tuple2(initialResult, 0),
			needles).a;
	});
var $author$project$OptionSearcher$simpleMatch = F2(
	function (needle, hay) {
		return A4(
			$tripokey$elm_fuzzy$Fuzzy$match,
			_List_Nil,
			_List_fromArray(
				[' ']),
			needle,
			hay);
	});
var $author$project$OptionSearcher$search = F2(
	function (string, option) {
		return {
			b2: A2(
				$author$project$OptionSearcher$simpleMatch,
				$elm$core$String$toLower(string),
				$author$project$Option$optionDescriptionToSearchString(
					$author$project$Option$getOptionDescription(option))),
			ck: A2(
				$author$project$OptionSearcher$simpleMatch,
				$elm$core$String$toLower(string),
				$author$project$OptionLabel$optionLabelToSearchString(
					$author$project$Option$getOptionLabel(option)))
		};
	});
var $author$project$Option$setOptionSearchFilter = F2(
	function (maybeOptionSearchFilter, option) {
		switch (option.$) {
			case 0:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				var optionValue = option.c;
				var optionDescription = option.d;
				var optionGroup = option.e;
				return A6($author$project$Option$Option, optionDisplay, optionLabel, optionValue, optionDescription, optionGroup, maybeOptionSearchFilter);
			case 1:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				var optionValue = option.c;
				return A4($author$project$Option$CustomOption, optionDisplay, optionLabel, optionValue, maybeOptionSearchFilter);
			default:
				var optionDisplay = option.a;
				var optionLabel = option.b;
				return A2($author$project$Option$EmptyOption, optionDisplay, optionLabel);
		}
	});
var $elm_community$list_extra$List$Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _v0) {
				var i = _v0.a;
				var thisAcc = _v0.b;
				return _Utils_Tuple2(
					i + 1,
					A3(func, i, x, thisAcc));
			});
		return A3(
			$elm$core$List$foldl,
			step,
			_Utils_Tuple2(0, acc),
			list).b;
	});
var $mhoare$elm_stack$Stack$Stack = $elm$core$Basics$identity;
var $mhoare$elm_stack$Stack$initialise = _List_Nil;
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm$core$String$fromList = _String_fromList;
var $author$project$OptionPresentor$indexInsideMatch = F2(
	function (result, index) {
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (match) {
					var matchIndex = index - match.cB;
					return A2($elm$core$List$member, matchIndex, match.cj);
				},
				result.co));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR = 0;
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty = {$: 1};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty = $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty = {$: 1};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Error = function (a) {
	return {$: 0, a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$One = {$: 2};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two = function (a) {
	return {$: 3, a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Branch = F5(
	function (a, b, c, d, e) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e};
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height = function (rangeDict) {
	if (rangeDict.$ === 1) {
		return 0;
	} else {
		var height_ = rangeDict.a;
		return height_;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch = F4(
	function (range, value, lt, gt) {
		return A5(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Branch,
			A2(
				$elm$core$Basics$max,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(lt),
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(gt)) + 1,
			range,
			value,
			lt,
			gt);
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff = function (rangeDict) {
	if (rangeDict.$ === 1) {
		return 0;
	} else {
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(gt) - $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(lt);
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft = function (rangeDict) {
	if ((!rangeDict.$) && (!rangeDict.e.$)) {
		var head = rangeDict.b;
		var value = rangeDict.c;
		var lessThans = rangeDict.d;
		var _v1 = rangeDict.e;
		var subHead = _v1.b;
		var subValue = _v1.c;
		var betweens = _v1.d;
		var greaterThans = _v1.e;
		return A4(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
			subHead,
			subValue,
			A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, head, value, lessThans, betweens),
			greaterThans);
	} else {
		return rangeDict;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight = function (rangeDict) {
	if ((!rangeDict.$) && (!rangeDict.d.$)) {
		var head = rangeDict.b;
		var value = rangeDict.c;
		var _v1 = rangeDict.d;
		var subHead = _v1.b;
		var subValue = _v1.c;
		var lessThans = _v1.d;
		var betweens = _v1.e;
		var greaterThans = rangeDict.e;
		return A4(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
			subHead,
			subValue,
			lessThans,
			A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, head, value, betweens, greaterThans));
	} else {
		return rangeDict;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance = function (rangeDict) {
	if (rangeDict.$ === 1) {
		return rangeDict;
	} else {
		var here = rangeDict.b;
		var value = rangeDict.c;
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return (_Utils_eq(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict),
			-2) && ($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(lt) === 1)) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(
			A4(
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
				here,
				value,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(lt),
				gt)) : ((_Utils_cmp(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict),
			-1) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(rangeDict) : ((($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict) === 2) && _Utils_eq(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(gt),
			-1)) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(
			A4(
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
				here,
				value,
				lt,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(gt))) : (($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict) > 1) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(rangeDict) : rangeDict)));
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point = function (a) {
	return {$: 0, a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range = F2(
	function (a, b) {
		return _Utils_eq(a, b) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point(a) : A2(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
			A2($elm$core$Basics$min, a, b),
			A2($elm$core$Basics$max, a, b));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$combine = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if (!_v0.a.$) {
			if (!_v0.b.$) {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range, x, y);
			} else {
				var x = _v0.a.a;
				var _v1 = _v0.b;
				var low = _v1.a;
				var high = _v1.b;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, x, low),
					A2($elm$core$Basics$max, x, high));
			}
		} else {
			if (!_v0.b.$) {
				var _v2 = _v0.a;
				var low = _v2.a;
				var high = _v2.b;
				var x = _v0.b.a;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, x, low),
					A2($elm$core$Basics$max, x, high));
			} else {
				var _v3 = _v0.a;
				var low1 = _v3.a;
				var high1 = _v3.b;
				var _v4 = _v0.b;
				var low2 = _v4.a;
				var high2 = _v4.b;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, low1, low2),
					A2($elm$core$Basics$max, high1, high2));
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$EQ = 0;
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT = 1;
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT = 2;
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Overlapping = 3;
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if (!_v0.a.$) {
			if (!_v0.b.$) {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return (_Utils_cmp(x, y) > 0) ? 1 : ((_Utils_cmp(x, y) < 0) ? 2 : 0);
			} else {
				var x = _v0.a.a;
				var _v1 = _v0.b;
				var low = _v1.a;
				var high = _v1.b;
				return (_Utils_cmp(x, low) < 0) ? 2 : ((_Utils_cmp(x, high) > 0) ? 1 : 3);
			}
		} else {
			if (!_v0.b.$) {
				var _v2 = _v0.a;
				var low = _v2.a;
				var high = _v2.b;
				var x = _v0.b.a;
				return (_Utils_cmp(x, low) < 0) ? 1 : ((_Utils_cmp(x, high) > 0) ? 2 : 3);
			} else {
				var _v3 = _v0.a;
				var low1 = _v3.a;
				var high1 = _v3.b;
				var _v4 = _v0.b;
				var low2 = _v4.a;
				var high2 = _v4.b;
				return (_Utils_cmp(high1, low2) < 0) ? 2 : ((_Utils_cmp(low1, high2) > 0) ? 1 : ((_Utils_eq(low1, low2) && _Utils_eq(high1, high2)) ? 0 : 3));
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert = F3(
	function (range, value, set) {
		if (set.$ === 1) {
			return A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, range, value, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty);
		} else {
			var height_ = set.a;
			var here = set.b;
			var hereValue = set.c;
			var lt = set.d;
			var gt = set.e;
			var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare, here, range);
			switch (_v1) {
				case 2:
					return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance(
						A4(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
							here,
							hereValue,
							A3($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range, value, lt),
							gt));
				case 1:
					return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance(
						A4(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
							here,
							hereValue,
							lt,
							A3($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range, value, gt)));
				case 0:
					return set;
				default:
					var combined = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$combine, range, here);
					return A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, combined, value, lt, gt);
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point = $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$handleChar = F3(
	function (value, _char, _v0) {
		var parserState = _v0.a;
		var rangeDict = _v0.b;
		var _v1 = _Utils_Tuple2(parserState, _char);
		switch (_v1.a.$) {
			case 0:
				return _Utils_Tuple2(parserState, rangeDict);
			case 1:
				switch (_v1.b) {
					case '1':
						var _v2 = _v1.a;
						return _Utils_Tuple2($BrianHicks$elm_string_graphemes$String$Graphemes$Data$One, rangeDict);
					case '2':
						var _v3 = _v1.a;
						return _Utils_Tuple2(
							$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two($elm$core$Maybe$Nothing),
							rangeDict);
					default:
						var _v4 = _v1.a;
						return _Utils_Tuple2(
							$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Error('expected to see a parsing directive like \'1\' or \'2\''),
							rangeDict);
				}
			case 2:
				var _v5 = _v1.a;
				return _Utils_Tuple2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty,
					A3(
						$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert,
						$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point(_char),
						value,
						rangeDict));
			default:
				if (_v1.a.a.$ === 1) {
					var _v6 = _v1.a.a;
					return _Utils_Tuple2(
						$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two(
							$elm$core$Maybe$Just(_char)),
						rangeDict);
				} else {
					var low = _v1.a.a.a;
					return _Utils_Tuple2(
						$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty,
						A3(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert,
							A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range, low, _char),
							value,
							rangeDict));
				}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser = F2(
	function (value, source) {
		var _v0 = A3(
			$elm$core$String$foldl,
			$BrianHicks$elm_string_graphemes$String$Graphemes$Data$handleChar(value),
			_Utils_Tuple2($BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
			source);
		switch (_v0.a.$) {
			case 1:
				var _v1 = _v0.a;
				var out = _v0.b;
				return $elm$core$Result$Ok(out);
			case 0:
				var err = _v0.a.a;
				return $elm$core$Result$Err(err);
			case 2:
				var _v2 = _v0.a;
				return $elm$core$Result$Err('ended with an empty One');
			default:
				return $elm$core$Result$Err('ended with an empty Two');
		}
	});
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(0))('1\r');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control = 2;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(2))('2\u0000\u00092\u000B\u000C2\u000E\u001F2\u007F\u009F1\u00AD1\u061C1\u180E1\u200B2\u200E\u200F1\u20281\u20292\u202A\u202E2\u2060\u20641\u20652\u2066\u206F1\uFEFF2\uFFF0\uFFF82\uFFF9\uFFFB2\uD80D\uDC30\uD80D\uDC382\uD82F\uDCA0\uD82F\uDCA32\uD834\uDD73\uD834\uDD7A1\uDB40\uDC001\uDB40\uDC012\uDB40\uDC02\uDB40\uDC1F2\uDB40\uDC80\uDB40\uDCFF2\uDB40\uDDF0\uDB43\uDFFF');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend = 11;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(11))('2̀ͯ2҃҇2҈҉2ֽ֑1ֿ2ׁׂ2ׅׄ1ׇ2ؚؐ2ًٟ1ٰ2ۖۜ2۟ۤ2ۧۨ2۪ۭ1ܑ2ܰ݊2ަް2߫߳1߽2ࠖ࠙2ࠛࠣ2ࠥࠧ2ࠩ࠭2࡙࡛2࣓࣡2ࣣं1ऺ1़2ुै1्2॑ॗ2ॢॣ1ঁ1়1া2ুৄ1্1ৗ2ৢৣ1৾2ਁਂ1਼2ੁੂ2ੇੈ2ੋ੍1ੑ2ੰੱ1ੵ2ઁં1઼2ુૅ2ેૈ1્2ૢૣ2ૺ૿1ଁ1଼1ା1ି2ୁୄ1୍1ୖ1ୗ2ୢୣ1ஂ1ா1ீ1்1ௗ1ఀ1ఄ2ాీ2ెై2ొ్2ౕౖ2ౢౣ1ಁ1಼1ಿ1ೂ1ೆ2ೌ್2ೕೖ2ೢೣ2ഀഁ2഻഼1ാ2ുൄ1്1ൗ2ൢൣ1්1ා2ිු1ූ1ෟ1ั2ิฺ2็๎1ັ2ິຼ2່ໍ2༘༙1༵1༷1༹2ཱཾ2྄ྀ2྆྇2ྍྗ2ྙྼ1࿆2ိူ2ဲ့2္်2ွှ2ၘၙ2ၞၠ2ၱၴ1ႂ2ႅႆ1ႍ1ႝ2፝፟2ᜒ᜔2ᜲ᜴2ᝒᝓ2ᝲᝳ2឴឵2ិួ1ំ2៉៓1៝2᠋᠍2ᢅᢆ1ᢩ2ᤠᤢ2ᤧᤨ1ᤲ2᤻᤹2ᨘᨗ1ᨛ1ᩖ2ᩘᩞ1᩠1ᩢ2ᩥᩬ2ᩳ᩼1᩿2᪽᪰1᪾2ᬀᬃ1᬴1ᬵ2ᬶᬺ1ᬼ1ᭂ2᭫᭳2ᮀᮁ2ᮢᮥ2ᮨᮩ2᮫ᮭ1᯦2ᯨᯩ1ᯭ2ᯯᯱ2ᰬᰳ2ᰶ᰷2᳐᳒2᳔᳠2᳢᳨1᳭1᳴2᳸᳹2᷹᷀2᷿᷻1‌2⃐⃜2⃝⃠1⃡2⃢⃤2⃥⃰2⳯⳱1⵿2ⷠⷿ2〪〭2〮〯2゙゚1꙯2꙰꙲2ꙴ꙽2ꚞꚟ2꛰꛱1ꠂ1꠆1ꠋ2ꠥꠦ2꣄ꣅ2꣠꣱1ꣿ2ꤦ꤭2ꥇꥑ2ꦀꦂ1꦳2ꦶꦹ2ꦼꦽ1ꧥ2ꨩꨮ2ꨱꨲ2ꨵꨶ1ꩃ1ꩌ1ꩼ1ꪰ2ꪴꪲ2ꪷꪸ2ꪾ꪿1꫁2ꫬꫭ1꫶1ꯥ1ꯨ1꯭1ﬞ2︀️2︠︯2ﾞﾟ1𐇽1𐋠2𐍶𐍺2𐨁𐨃2𐨅𐨆2𐨌𐨏2𐨺𐨸1𐨿2𐫦𐫥2𐴤𐴧2𐽆𐽐1𑀁2𑀸𑁆2𑁿𑂁2𑂳𑂶2𑂺𑂹2𑄀𑄂2𑄧𑄫2𑄭𑄴1𑅳2𑆀𑆁2𑆶𑆾2𑇉𑇌2𑈯𑈱1𑈴2𑈶𑈷1𑈾1𑋟2𑋣𑋪2𑌀𑌁2𑌻𑌼1𑌾1𑍀1𑍗2𑍦𑍬2𑍰𑍴2𑐸𑐿2𑑂𑑄1𑑆1𑑞1𑒰2𑒳𑒸1𑒺1𑒽2𑒿𑓀2𑓃𑓂1𑖯2𑖲𑖵2𑖼𑖽2𑗀𑖿2𑗜𑗝2𑘳𑘺1𑘽2𑘿𑙀1𑚫1𑚭2𑚰𑚵1𑚷2𑜝𑜟2𑜢𑜥2𑜧𑜫2𑠯𑠷2𑠺𑠹2𑧔𑧗2𑧚𑧛1𑧠2𑨁𑨊2𑨳𑨸2𑨻𑨾1𑩇2𑩑𑩖2𑩙𑩛2𑪊𑪖2𑪘𑪙2𑰰𑰶2𑰸𑰽1𑰿2𑲒𑲧2𑲪𑲰2𑲲𑲳2𑲵𑲶2𑴱𑴶1𑴺2𑴼𑴽2𑴿𑵅1𑵇2𑶐𑶑1𑶕1𑶗2𑻳𑻴2𖫰𖫴2𖬰𖬶1𖽏2𖾏𖾒2𛲝𛲞1𝅥2𝅧𝅩2𝅮𝅲2𝅻𝆂2𝆋𝆅2𝆪𝆭2𝉂𝉄2𝨀𝨶2𝨻𝩬1𝩵1𝪄2𝪛𝪟2𝪡𝪯2𞀀𞀆2𞀈𞀘2𞀛𞀡2𞀣𞀤2𞀦𞀪2𞄰𞄶2𞋬𞋯2𞣐𞣖2𞥊𞥄2🏻🏿2󠀠󠁿2󠄀󠇯');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ExtendedPictographic = 10;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extended_Pictographic$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(10))('1©1®1‼1⁉1™1ℹ2↔↙2↩↪2⌚⌛1⌨1⎈1⏏2⏩⏳2⏸⏺1Ⓜ2▪▫1▶1◀2◻◾2☀☄1★2☇☍1☎2☏☐1☑1☒2☔☕2☖☗1☘2☙☜1☝2☞☟1☠1☡2☢☣2☤☥1☦2☧☩1☪2☫☭2☮☯2☰☷2☸☺2☻☿1♀1♁1♂2♃♇2♈♓2♔♞1♟1♠2♡♢1♣1♤2♥♦1♧1♨2♩♺1♻2♼♽1♾1♿2⚀⚅2⚐⚑2⚒⚔1⚕2⚖⚗1⚘1⚙1⚚2⚛⚜2⚝⚟2⚠⚡2⚢⚩2⚪⚫2⚬⚯2⚰⚱2⚲⚼2⚽⚾2⚿⛃2⛄⛅2⛆⛇1⛈2⛉⛍2⛎⛏1⛐1⛑1⛒2⛓⛔2⛕⛨2⛩⛪2⛫⛯2⛰⛵1⛶2⛷⛺2⛻⛼1⛽2⛾✁1✂2✃✄1✅2✈✍1✎1✏2✐✑1✒1✔1✖1✝1✡1✨2✳✴1❄1❇1❌1❎2❓❕1❗2❣❤2❥❧2➕➗1➡1➰1➿2⤴⤵2⬅⬇2⬛⬜1⭐1⭕1〰1〽1㊗1㊙2🀀🀃1🀄2🀅🃎1🃏2🃐🃿2🄍🄏1🄯2🅬🅯2🅰🅱2🅾🅿1🆎2🆑🆚2🆭🇥2🈁🈂2🈃🈏1🈚1🈯2🈲🈺2🈼🈿2🉉🉏2🉐🉑2🉒🋿2🌀🌡2🌢🌣2🌤🎓2🎔🎕2🎖🎗1🎘2🎙🎛2🎜🎝2🎞🏰2🏱🏲2🏳🏵1🏶2🏷🏺2🐀📽1📾2📿🔽2🕆🕈2🕉🕎1🕏2🕐🕧2🕨🕮2🕯🕰2🕱🕲2🕳🕹1🕺2🕻🖆1🖇2🖈🖉2🖊🖍2🖎🖏1🖐2🖑🖔2🖕🖖2🖗🖣1🖤1🖥2🖦🖧1🖨2🖩🖰2🖱🖲2🖳🖻1🖼2🖽🗁2🗂🗄2🗅🗐2🗑🗓2🗔🗛2🗜🗞2🗟🗠1🗡1🗢1🗣2🗤🗧1🗨2🗩🗮1🗯2🗰🗲1🗳2🗴🗹2🗺🙏2🚀🛅2🛆🛊2🛋🛐2🛑🛒2🛓🛔1🛕2🛖🛟2🛠🛥2🛦🛨1🛩1🛪2🛫🛬2🛭🛯1🛰2🛱🛲1🛳2🛴🛶2🛷🛸1🛹1🛺2🛻🛿2🝴🝿2🟕🟟2🟠🟫2🟬🟿2🠌🠏2🡈🡏2🡚🡟2🢈🢏2🢮🣿1🤌2🤍🤏2🤐🤘2🤙🤞1🤟2🤠🤧2🤨🤯1🤰2🤱🤲2🤳🤺2🤼🤾1🤿2🥀🥅2🥇🥋1🥌2🥍🥏2🥐🥞2🥟🥫2🥬🥰1🥱1🥲2🥳🥶2🥷🥹1🥺1🥻2🥼🥿2🦀🦄2🦅🦑2🦒🦗2🦘🦢2🦣🦤2🦥🦪2🦫🦭2🦮🦯2🦰🦹2🦺🦿1🧀2🧁🧂2🧃🧊2🧋🧌2🧍🧏2🧐🧦2🧧🧿2🨀🩯2🩰🩳2🩴🩷2🩸🩺2🩻🩿2🪀🪂2🪃🪏2🪐🪕2🪖🿽');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L = 5;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(5))('2ᄀᅟ2ꥠꥼ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF = 1;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(1))('1\n');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV = 8;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(8))('1가1개1갸1걔1거1게1겨1계1고1과1괘1괴1교1구1궈1궤1귀1규1그1긔1기1까1깨1꺄1꺠1꺼1께1껴1꼐1꼬1꽈1꽤1꾀1꾜1꾸1꿔1꿰1뀌1뀨1끄1끠1끼1나1내1냐1냬1너1네1녀1녜1노1놔1놰1뇌1뇨1누1눠1눼1뉘1뉴1느1늬1니1다1대1댜1댸1더1데1뎌1뎨1도1돠1돼1되1됴1두1둬1뒈1뒤1듀1드1듸1디1따1때1땨1떄1떠1떼1뗘1뗴1또1똬1뙈1뙤1뚀1뚜1뚸1뛔1뛰1뜌1뜨1띄1띠1라1래1랴1럐1러1레1려1례1로1롸1뢔1뢰1료1루1뤄1뤠1뤼1류1르1릐1리1마1매1먀1먜1머1메1며1몌1모1뫄1뫠1뫼1묘1무1뭐1뭬1뮈1뮤1므1믜1미1바1배1뱌1뱨1버1베1벼1볘1보1봐1봬1뵈1뵤1부1붜1붸1뷔1뷰1브1븨1비1빠1빼1뺘1뺴1뻐1뻬1뼈1뼤1뽀1뽜1뽸1뾔1뾰1뿌1뿨1쀄1쀠1쀼1쁘1쁴1삐1사1새1샤1섀1서1세1셔1셰1소1솨1쇄1쇠1쇼1수1숴1쉐1쉬1슈1스1싀1시1싸1쌔1쌰1썌1써1쎄1쎠1쎼1쏘1쏴1쐐1쐬1쑈1쑤1쒀1쒜1쒸1쓔1쓰1씌1씨1아1애1야1얘1어1에1여1예1오1와1왜1외1요1우1워1웨1위1유1으1의1이1자1재1쟈1쟤1저1제1져1졔1조1좌1좨1죄1죠1주1줘1줴1쥐1쥬1즈1즤1지1짜1째1쨔1쨰1쩌1쩨1쪄1쪠1쪼1쫘1쫴1쬐1쬬1쭈1쭤1쮀1쮜1쮸1쯔1쯰1찌1차1채1챠1챼1처1체1쳐1쳬1초1촤1쵀1최1쵸1추1춰1췌1취1츄1츠1츼1치1카1캐1캬1컈1커1케1켜1켸1코1콰1쾌1쾨1쿄1쿠1쿼1퀘1퀴1큐1크1킈1키1타1태1탸1턔1터1테1텨1톄1토1톼1퇘1퇴1툐1투1퉈1퉤1튀1튜1트1틔1티1파1패1퍄1퍠1퍼1페1펴1폐1포1퐈1퐤1푀1표1푸1풔1풰1퓌1퓨1프1픠1피1하1해1햐1햬1허1헤1혀1혜1호1화1홰1회1효1후1훠1훼1휘1휴1흐1희1히');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT = 9;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(9))('2각갛2객갷2갹걓2걕걯2걱겋2겍겧2격곃2곅곟2곡곻2곽괗2괙괳2괵굏2굑굫2국궇2궉궣2궥궿2귁귛2귝귷2극긓2긕긯2긱깋2깍깧2깩꺃2꺅꺟2꺡꺻2꺽껗2껙껳2껵꼏2꼑꼫2꼭꽇2꽉꽣2꽥꽿2꾁꾛2꾝꾷2꾹꿓2꿕꿯2꿱뀋2뀍뀧2뀩끃2끅끟2끡끻2끽낗2낙낳2낵냏2냑냫2냭넇2넉넣2넥넿2녁녛2녝녷2녹놓2놕놯2놱뇋2뇍뇧2뇩눃2눅눟2눡눻2눽뉗2뉙뉳2뉵늏2늑늫2늭닇2닉닣2닥닿2댁댛2댝댷2댹덓2덕덯2덱뎋2뎍뎧2뎩돃2독돟2돡돻2돽됗2됙됳2됵둏2둑둫2둭뒇2뒉뒣2뒥뒿2듁듛2득듷2듹딓2딕딯2딱땋2땍땧2땩떃2떅떟2떡떻2떽뗗2뗙뗳2뗵똏2똑똫2똭뙇2뙉뙣2뙥뙿2뚁뚛2뚝뚷2뚹뛓2뛕뛯2뛱뜋2뜍뜧2뜩띃2띅띟2띡띻2락랗2랙랳2략럏2럑럫2럭렇2렉렣2력렿2롁롛2록롷2롹뢓2뢕뢯2뢱룋2룍룧2룩뤃2뤅뤟2뤡뤻2뤽륗2륙륳2륵릏2릑릫2릭맇2막맣2맥맿2먁먛2먝먷2먹멓2멕멯2멱몋2몍몧2목뫃2뫅뫟2뫡뫻2뫽묗2묙묳2묵뭏2뭑뭫2뭭뮇2뮉뮣2뮥뮿2믁믛2믝믷2믹밓2박밯2백뱋2뱍뱧2뱩벃2벅벟2벡벻2벽볗2볙볳2복봏2봑봫2봭뵇2뵉뵣2뵥뵿2북붛2붝붷2붹뷓2뷕뷯2뷱븋2븍븧2븩빃2빅빟2빡빻2빽뺗2뺙뺳2뺵뻏2뻑뻫2뻭뼇2뼉뼣2뼥뼿2뽁뽛2뽝뽷2뽹뾓2뾕뾯2뾱뿋2뿍뿧2뿩쀃2쀅쀟2쀡쀻2쀽쁗2쁙쁳2쁵삏2삑삫2삭샇2색샣2샥샿2섁섛2석섷2섹셓2셕셯2셱솋2속솧2솩쇃2쇅쇟2쇡쇻2쇽숗2숙숳2숵쉏2쉑쉫2쉭슇2슉슣2슥슿2싁싛2식싷2싹쌓2쌕쌯2쌱썋2썍썧2썩쎃2쎅쎟2쎡쎻2쎽쏗2쏙쏳2쏵쐏2쐑쐫2쐭쑇2쑉쑣2쑥쑿2쒁쒛2쒝쒷2쒹쓓2쓕쓯2쓱씋2씍씧2씩앃2악앟2액앻2약얗2얙얳2억엏2엑엫2역옇2옉옣2옥옿2왁왛2왝왷2왹욓2욕욯2욱웋2웍웧2웩윃2윅윟2육윻2윽읗2읙읳2익잏2작잫2잭쟇2쟉쟣2쟥쟿2적젛2젝젷2젹졓2졕졯2족좋2좍좧2좩죃2죅죟2죡죻2죽줗2줙줳2줵쥏2쥑쥫2쥭즇2즉즣2즥즿2직짛2짝짷2짹쨓2쨕쨯2쨱쩋2쩍쩧2쩩쪃2쪅쪟2쪡쪻2쪽쫗2쫙쫳2쫵쬏2쬑쬫2쬭쭇2쭉쭣2쭥쭿2쮁쮛2쮝쮷2쮹쯓2쯕쯯2쯱찋2찍찧2착챃2책챟2챡챻2챽첗2척첳2첵쳏2쳑쳫2쳭촇2촉촣2촥촿2쵁쵛2쵝쵷2쵹춓2축춯2춱췋2췍췧2췩츃2츅츟2측츻2츽칗2칙칳2칵캏2캑캫2캭컇2컉컣2컥컿2켁켛2켝켷2켹콓2콕콯2콱쾋2쾍쾧2쾩쿃2쿅쿟2쿡쿻2쿽퀗2퀙퀳2퀵큏2큑큫2큭킇2킉킣2킥킿2탁탛2택탷2탹턓2턕턯2턱텋2텍텧2텩톃2톅톟2톡톻2톽퇗2퇙퇳2퇵툏2툑툫2툭퉇2퉉퉣2퉥퉿2튁튛2튝튷2특틓2틕틯2틱팋2팍팧2팩퍃2퍅퍟2퍡퍻2퍽펗2펙펳2펵폏2폑폫2폭퐇2퐉퐣2퐥퐿2푁푛2푝푷2푹풓2풕풯2풱퓋2퓍퓧2퓩픃2픅픟2픡픻2픽핗2학핳2핵햏2햑햫2햭헇2헉헣2헥헿2혁혛2혝혷2혹홓2확홯2홱횋2획횧2횩훃2훅훟2훡훻2훽휗2휙휳2휵흏2흑흫2흭힇2힉힣');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend = 3;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(3))('2؀؅1۝1܏1࣢1ൎ1𑂽1𑃍2𑇂𑇃1𑨺2𑪄𑪉1𑵆');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$RegionalIndicator = 4;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Regional_Indicator$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(4))('2🇦🇿');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark = 12;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(12))('1ः1ऻ2ाी2ॉौ2ॎॏ2ংঃ2িী2েৈ2োৌ1ਃ2ਾੀ1ઃ2ાી1ૉ2ોૌ2ଂଃ1ୀ2େୈ2ୋୌ1ி2ுூ2ெை2ொௌ2ఁః2ుౄ2ಂಃ1ಾ2ೀು2ೃೄ2ೇೈ2ೊೋ2ംഃ2ിീ2െൈ2ൊൌ2ංඃ2ැෑ2ෘෞ2ෲෳ1ำ1ຳ2༾༿1ཿ1ေ2ျြ2ၖၗ1ႄ1ា2ើៅ2ះៈ2ᤣᤦ2ᤩᤫ2ᤰᤱ2ᤳᤸ2ᨙᨚ1ᩕ1ᩗ2ᩭᩲ1ᬄ1ᬻ2ᬽᭁ2ᭃ᭄1ᮂ1ᮡ2ᮦᮧ1᮪1ᯧ2ᯪᯬ1ᯮ2᯲᯳2ᰤᰫ2ᰴᰵ1᳡1᳷2ꠣꠤ1ꠧ2ꢀꢁ2ꢴꣃ2ꥒ꥓1ꦃ2ꦴꦵ2ꦺꦻ2ꦾ꧀2ꨯꨰ2ꨳꨴ1ꩍ1ꫫ2ꫮꫯ1ꫵ2ꯣꯤ2ꯦꯧ2ꯩꯪ1꯬1𑀀1𑀂1𑂂2𑂰𑂲2𑂷𑂸1𑄬2𑅅𑅆1𑆂2𑆳𑆵2𑆿𑇀2𑈬𑈮2𑈲𑈳1𑈵2𑋠𑋢2𑌂𑌃1𑌿2𑍁𑍄2𑍇𑍈2𑍋𑍍2𑍢𑍣2𑐵𑐷2𑑀𑑁1𑑅2𑒱𑒲1𑒹2𑒻𑒼1𑒾1𑓁2𑖰𑖱2𑖸𑖻1𑖾2𑘰𑘲2𑘻𑘼1𑘾1𑚬2𑚮𑚯1𑚶2𑜠𑜡1𑜦2𑠬𑠮1𑠸2𑧑𑧓2𑧜𑧟1𑧤1𑨹2𑩗𑩘1𑪗1𑰯1𑰾1𑲩1𑲱1𑲴2𑶊𑶎2𑶓𑶔1𑶖2𑻵𑻶2𖽑𖾇1𝅦1𝅭');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T = 7;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(7))('2ᆨᇿ2ퟋퟻ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V = 6;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(6))('2ᅠᆧ2ힰퟆ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ = 13;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser(13))('1‍');
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList = function (rangeDict) {
	if (rangeDict.$ === 1) {
		return _List_Nil;
	} else {
		var here = rangeDict.b;
		var value = rangeDict.c;
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return _Utils_ap(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(lt),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(here, value),
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(gt)));
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$union = F2(
	function (a, b) {
		return A3(
			$elm$core$List$foldl,
			function (_v0) {
				var range_ = _v0.a;
				var value = _v0.b;
				return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range_, value);
			},
			b,
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(a));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes = A3(
	$elm$core$List$foldl,
	$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$union,
	$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty,
	_List_fromArray(
		[$BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Regional_Indicator$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extended_Pictographic$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ$chars]));
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$getHelp = F2(
	function (range, rangeDict) {
		getHelp:
		while (true) {
			if (rangeDict.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var height_ = rangeDict.a;
				var here = rangeDict.b;
				var value = rangeDict.c;
				var lt = rangeDict.d;
				var gt = rangeDict.e;
				var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare, range, here);
				switch (_v1) {
					case 2:
						var $temp$range = range,
							$temp$rangeDict = gt;
						range = $temp$range;
						rangeDict = $temp$rangeDict;
						continue getHelp;
					case 1:
						var $temp$range = range,
							$temp$rangeDict = lt;
						range = $temp$range;
						rangeDict = $temp$rangeDict;
						continue getHelp;
					case 0:
						return $elm$core$Maybe$Just(value);
					default:
						return $elm$core$Maybe$Just(value);
				}
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get = F2(
	function (what, rangeDict) {
		return A2(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$getHelp,
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point(what),
			rangeDict);
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakForRule11 = function (classes_) {
	shouldBreakForRule11:
	while (true) {
		_v0$2:
		while (true) {
			if (classes_.b && (!classes_.a.$)) {
				switch (classes_.a.a) {
					case 11:
						var _v1 = classes_.a.a;
						var rest = classes_.b;
						var $temp$classes_ = rest;
						classes_ = $temp$classes_;
						continue shouldBreakForRule11;
					case 10:
						if (!classes_.b.b) {
							var _v2 = classes_.a.a;
							return false;
						} else {
							break _v0$2;
						}
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return true;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore = F3(
	function (lastChar, restChars, nextChar) {
		var _v0 = _Utils_Tuple2(
			A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, lastChar, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes),
			A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, nextChar, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes));
		_v0$1:
		while (true) {
			_v0$8:
			while (true) {
				_v0$20:
				while (true) {
					_v0$21:
					while (true) {
						_v0$22:
						while (true) {
							_v0$23:
							while (true) {
								if (!_v0.a.$) {
									switch (_v0.a.a) {
										case 0:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 1:
														var _v1 = _v0.a.a;
														var _v2 = _v0.b.a;
														return false;
													case 13:
														break _v0$1;
													case 12:
														break _v0$1;
													case 11:
														break _v0$1;
													default:
														break _v0$1;
												}
											} else {
												break _v0$1;
											}
										case 1:
											var _v4 = _v0.a.a;
											return true;
										case 2:
											var _v5 = _v0.a.a;
											return true;
										case 4:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 4:
														var _v6 = _v0.a.a;
														var _v7 = _v0.b.a;
														return !$elm$core$List$isEmpty(restChars);
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 3:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 0:
														var _v8 = _v0.a.a;
														var _v9 = _v0.b.a;
														return true;
													case 1:
														var _v10 = _v0.a.a;
														var _v11 = _v0.b.a;
														return true;
													case 2:
														var _v12 = _v0.a.a;
														var _v13 = _v0.b.a;
														return true;
													case 13:
														break _v0$8;
													case 12:
														break _v0$8;
													case 11:
														break _v0$8;
													default:
														break _v0$8;
												}
											} else {
												break _v0$8;
											}
										case 5:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 5:
														var _v15 = _v0.a.a;
														var _v16 = _v0.b.a;
														return false;
													case 6:
														var _v17 = _v0.a.a;
														var _v18 = _v0.b.a;
														return false;
													case 8:
														var _v19 = _v0.a.a;
														var _v20 = _v0.b.a;
														return false;
													case 9:
														var _v21 = _v0.a.a;
														var _v22 = _v0.b.a;
														return false;
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 6:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 6:
														var _v23 = _v0.a.a;
														var _v24 = _v0.b.a;
														return false;
													case 7:
														var _v25 = _v0.a.a;
														var _v26 = _v0.b.a;
														return false;
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 7:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 7:
														var _v27 = _v0.a.a;
														var _v28 = _v0.b.a;
														return false;
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 8:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 6:
														var _v29 = _v0.a.a;
														var _v30 = _v0.b.a;
														return false;
													case 7:
														var _v31 = _v0.a.a;
														var _v32 = _v0.b.a;
														return false;
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 9:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 7:
														var _v33 = _v0.a.a;
														var _v34 = _v0.b.a;
														return false;
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 13:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 10:
														var _v35 = _v0.a.a;
														var _v36 = _v0.b.a;
														return $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakForRule11(
															A2(
																$elm$core$List$map,
																function (c) {
																	return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, c, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes);
																},
																restChars));
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										default:
											if (!_v0.b.$) {
												switch (_v0.b.a) {
													case 13:
														break _v0$20;
													case 12:
														break _v0$21;
													case 11:
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
									}
								} else {
									if (!_v0.b.$) {
										switch (_v0.b.a) {
											case 13:
												break _v0$20;
											case 12:
												break _v0$21;
											case 11:
												break _v0$22;
											default:
												break _v0$23;
										}
									} else {
										break _v0$23;
									}
								}
							}
							return true;
						}
						var _v39 = _v0.b.a;
						return false;
					}
					var _v38 = _v0.b.a;
					return false;
				}
				var _v37 = _v0.b.a;
				return false;
			}
			var _v14 = _v0.a.a;
			return false;
		}
		var _v3 = _v0.a.a;
		return true;
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$unconsHelp = F2(
	function (str, chars) {
		unconsHelp:
		while (true) {
			var _v0 = _Utils_Tuple2(
				$elm$core$String$uncons(str),
				chars);
			if (_v0.a.$ === 1) {
				if (!_v0.b.b) {
					var _v1 = _v0.a;
					if (A2($elm$core$String$left, 1, str) === '\u0000') {
						var $temp$str = A2($elm$core$String$dropLeft, 1, str),
							$temp$chars = _List_fromArray(
							['\u0000']);
						str = $temp$str;
						chars = $temp$chars;
						continue unconsHelp;
					} else {
						return _Utils_Tuple2(chars, '');
					}
				} else {
					var _v2 = _v0.a;
					var _v3 = _v0.b;
					var last = _v3.a;
					var rest = _v3.b;
					if (A2($elm$core$String$left, 1, str) === '\u0000') {
						if (A3($BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore, last, rest, '\u0000')) {
							return _Utils_Tuple2(chars, str);
						} else {
							var $temp$str = A2($elm$core$String$dropLeft, 1, str),
								$temp$chars = A2($elm$core$List$cons, '\u0000', chars);
							str = $temp$str;
							chars = $temp$chars;
							continue unconsHelp;
						}
					} else {
						return _Utils_Tuple2(chars, '');
					}
				}
			} else {
				if (!_v0.b.b) {
					var _v4 = _v0.a.a;
					var _char = _v4.a;
					var strTail = _v4.b;
					var $temp$str = strTail,
						$temp$chars = _List_fromArray(
						[_char]);
					str = $temp$str;
					chars = $temp$chars;
					continue unconsHelp;
				} else {
					var _v5 = _v0.a.a;
					var _char = _v5.a;
					var strTail = _v5.b;
					var _v6 = _v0.b;
					var last = _v6.a;
					var rest = _v6.b;
					if (A3($BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore, last, rest, _char)) {
						return _Utils_Tuple2(chars, str);
					} else {
						var $temp$str = strTail,
							$temp$chars = A2($elm$core$List$cons, _char, chars);
						str = $temp$str;
						chars = $temp$chars;
						continue unconsHelp;
					}
				}
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$foldl = F3(
	function (fn, initial, string) {
		foldl:
		while (true) {
			if (string === '') {
				return initial;
			} else {
				var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$Parser$unconsHelp, string, _List_Nil);
				var chars = _v1.a;
				var remaining = _v1.b;
				var $temp$fn = fn,
					$temp$initial = A2(
					fn,
					$elm$core$String$fromList(
						$elm$core$List$reverse(chars)),
					initial),
					$temp$string = remaining;
				fn = $temp$fn;
				initial = $temp$initial;
				string = $temp$string;
				continue foldl;
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$foldl = $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$foldl;
var $BrianHicks$elm_string_graphemes$String$Graphemes$length = A2(
	$BrianHicks$elm_string_graphemes$String$Graphemes$foldl,
	F2(
		function (_v0, len) {
			return len + 1;
		}),
	0);
var $mhoare$elm_stack$Stack$push = F2(
	function (item, _v0) {
		var stack = _v0;
		return A2($elm$core$List$cons, item, stack);
	});
var $mhoare$elm_stack$Stack$toList = function (_v0) {
	var stack = _v0;
	return stack;
};
var $mhoare$elm_stack$Stack$top = function (_v0) {
	var stack = _v0;
	return $elm$core$List$head(stack);
};
var $author$project$OptionPresentor$tokenizeHelper = F3(
	function (index, _char, highlightResult) {
		var theEnd = _Utils_eq(
			index,
			$BrianHicks$elm_string_graphemes$String$Graphemes$length(highlightResult.aF) - 1);
		if (A2($author$project$OptionPresentor$indexInsideMatch, highlightResult.aO, index)) {
			var _v0 = $mhoare$elm_stack$Stack$top(highlightResult.x);
			if (!_v0.$) {
				if (theEnd) {
					var prevText = _Utils_Tuple2(
						false,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								$mhoare$elm_stack$Stack$toList(highlightResult.x))));
					return _Utils_update(
						highlightResult,
						{
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[
										prevText,
										_Utils_Tuple2(
										true,
										$elm$core$String$fromChar(_char))
									]))
						});
				} else {
					var prevText = _Utils_Tuple2(
						false,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								$mhoare$elm_stack$Stack$toList(highlightResult.x))));
					return _Utils_update(
						highlightResult,
						{
							s: A2($mhoare$elm_stack$Stack$push, _char, highlightResult.s),
							x: $mhoare$elm_stack$Stack$initialise,
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[prevText]))
						});
				}
			} else {
				if (theEnd) {
					var currentHighlight = _Utils_Tuple2(
						true,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								A2(
									$elm$core$List$append,
									_List_fromArray(
										[_char]),
									$mhoare$elm_stack$Stack$toList(highlightResult.s)))));
					return _Utils_update(
						highlightResult,
						{
							s: $mhoare$elm_stack$Stack$initialise,
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[currentHighlight]))
						});
				} else {
					return _Utils_update(
						highlightResult,
						{
							s: A2($mhoare$elm_stack$Stack$push, _char, highlightResult.s)
						});
				}
			}
		} else {
			var _v1 = $mhoare$elm_stack$Stack$top(highlightResult.s);
			if (!_v1.$) {
				if (theEnd) {
					var prevHighlight = _Utils_Tuple2(
						true,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								$mhoare$elm_stack$Stack$toList(highlightResult.s))));
					return _Utils_update(
						highlightResult,
						{
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[
										prevHighlight,
										_Utils_Tuple2(
										false,
										$elm$core$String$fromChar(_char))
									]))
						});
				} else {
					var prevHighlight = _Utils_Tuple2(
						true,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								$mhoare$elm_stack$Stack$toList(highlightResult.s))));
					return _Utils_update(
						highlightResult,
						{
							s: $mhoare$elm_stack$Stack$initialise,
							x: A2($mhoare$elm_stack$Stack$push, _char, highlightResult.x),
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[prevHighlight]))
						});
				}
			} else {
				if (theEnd) {
					var prevText = _Utils_Tuple2(
						false,
						$elm$core$String$fromList(
							$elm$core$List$reverse(
								A2(
									$elm$core$List$append,
									_List_fromArray(
										[_char]),
									$mhoare$elm_stack$Stack$toList(highlightResult.x)))));
					return _Utils_update(
						highlightResult,
						{
							x: $mhoare$elm_stack$Stack$initialise,
							p: A2(
								$elm$core$List$append,
								highlightResult.p,
								_List_fromArray(
									[prevText]))
						});
				} else {
					return _Utils_update(
						highlightResult,
						{
							s: $mhoare$elm_stack$Stack$initialise,
							x: A2($mhoare$elm_stack$Stack$push, _char, highlightResult.x)
						});
				}
			}
		}
	});
var $author$project$OptionPresentor$tokenize = F2(
	function (hay, result) {
		return A3(
			$elm_community$list_extra$List$Extra$indexedFoldl,
			$author$project$OptionPresentor$tokenizeHelper,
			{aF: hay, s: $mhoare$elm_stack$Stack$initialise, x: $mhoare$elm_stack$Stack$initialise, aO: result, p: _List_Nil},
			$elm$core$String$toList(hay)).p;
	});
var $author$project$OptionSearcher$updateOptionsWithSearchString = F2(
	function (searchString, options) {
		if (searchString === '') {
			return A2(
				$elm$core$List$map,
				function (option) {
					return A2($author$project$Option$setOptionSearchFilter, $elm$core$Maybe$Nothing, option);
				},
				options);
		} else {
			return A2(
				$elm$core$List$map,
				function (option) {
					var searchResult = A2($author$project$OptionSearcher$search, searchString, option);
					var labelTokens = A2(
						$author$project$OptionPresentor$tokenize,
						$author$project$OptionLabel$optionLabelToString(
							$author$project$Option$getOptionLabel(option)),
						searchResult.ck);
					var descriptionTokens = A2(
						$author$project$OptionPresentor$tokenize,
						$author$project$Option$optionDescriptionToString(
							$author$project$Option$getOptionDescription(option)),
						searchResult.b2);
					return A2(
						$author$project$Option$setOptionSearchFilter,
						$elm$core$Maybe$Just(
							A4($author$project$OptionSearchFilter$new, searchResult.ck.bE + searchResult.b2.bE, searchResult, labelTokens, descriptionTokens)),
						option);
				},
				options);
		}
	});
var $author$project$SelectionMode$getCustomOptions = function (selectionMode) {
	if (!selectionMode.$) {
		var customOptions = selectionMode.a;
		return customOptions;
	} else {
		var customOptions = selectionMode.a;
		return customOptions;
	}
};
var $elm_community$list_extra$List$Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			if (!list.b) {
				return _List_Nil;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					var $temp$predicate = predicate,
						$temp$list = xs;
					predicate = $temp$predicate;
					list = $temp$list;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var $author$project$Option$updateOrAddCustomOption = F2(
	function (searchString, options) {
		var showAddOption = !A2(
			$elm$core$List$any,
			function (option_) {
				return _Utils_eq(
					$author$project$OptionLabel$optionLabelToSearchString(
						$author$project$Option$getOptionLabel(option_)),
					$elm$core$String$toLower(searchString));
			},
			options);
		var options_ = A2(
			$elm_community$list_extra$List$Extra$dropWhile,
			function (option_) {
				switch (option_.$) {
					case 1:
						return true;
					case 0:
						return false;
					default:
						return false;
				}
			},
			options);
		return showAddOption ? _Utils_ap(
			_List_fromArray(
				[
					A4(
					$author$project$Option$CustomOption,
					0,
					A3($author$project$OptionLabel$OptionLabel, 'Add ' + (searchString + '…'), $elm$core$Maybe$Nothing, $author$project$SortRank$NoSortRank),
					$author$project$Option$OptionValue(searchString),
					$elm$core$Maybe$Nothing)
				]),
			options_) : options_;
	});
var $author$project$OptionSearcher$updateOrAddCustomOption = F3(
	function (searchString, selectionMode, options) {
		if (searchString === '') {
			return options;
		} else {
			var _v1 = $author$project$SelectionMode$getCustomOptions(selectionMode);
			if (!_v1) {
				return A2($author$project$Option$updateOrAddCustomOption, searchString, options);
			} else {
				return options;
			}
		}
	});
var $author$project$OptionSearcher$updateOptions = F3(
	function (selectionMode, searchString, options) {
		return A2(
			$author$project$OptionSearcher$updateOptionsWithSearchString,
			searchString,
			A3($author$project$OptionSearcher$updateOrAddCustomOption, searchString, selectionMode, options));
	});
var $author$project$Main$updateModelWithSearchStringChanges = F4(
	function (maxNumberOfDropdownItems, searchString, options, model) {
		var optionsUpdatedWithSearchString = A3($author$project$OptionSearcher$updateOptions, model.l, searchString, options);
		if (searchString === '') {
			var updatedOptions = $author$project$Option$sortOptionsByGroupAndLabel(
				A3($author$project$OptionSearcher$updateOptions, model.l, searchString, options));
			return _Utils_update(
				model,
				{
					a: updatedOptions,
					e: A2($author$project$Main$figureOutWhichOptionsToShow, maxNumberOfDropdownItems, updatedOptions),
					E: searchString
				});
		} else {
			var optionsSortedByTotalScore = $author$project$Option$sortOptionsByTotalScore(optionsUpdatedWithSearchString);
			var maybeFirstOption = $elm$core$List$head(optionsSortedByTotalScore);
			var optionsSortedByTotalScoreWithTheFirstOptionHighlighted = function () {
				if (!maybeFirstOption.$) {
					var firstOption = maybeFirstOption.a;
					return A2($author$project$Option$highlightOptionInList, firstOption, optionsSortedByTotalScore);
				} else {
					return optionsSortedByTotalScore;
				}
			}();
			return _Utils_update(
				model,
				{
					a: optionsSortedByTotalScoreWithTheFirstOptionHighlighted,
					e: A2($author$project$Main$figureOutWhichOptionsToShow, maxNumberOfDropdownItems, optionsSortedByTotalScoreWithTheFirstOptionHighlighted),
					E: searchString
				});
		}
	});
var $author$project$Main$updateRightSlot = F3(
	function (current, selectionMode, hasSelectedOption) {
		switch (current) {
			case 0:
				if (!selectionMode.$) {
					return 2;
				} else {
					return hasSelectedOption ? 3 : 0;
				}
			case 1:
				return 1;
			case 2:
				return 2;
			default:
				return hasSelectedOption ? 3 : 0;
		}
	});
var $author$project$Main$updateRightSlotLoading = F3(
	function (isLoading, selectionMode, hasSelectedOption) {
		if (isLoading) {
			return 1;
		} else {
			if (!selectionMode.$) {
				return 2;
			} else {
				return hasSelectedOption ? 3 : 0;
			}
		}
	});
var $author$project$Ports$valuesDecoder = $elm$json$Json$Decode$list($elm$json$Json$Decode$string);
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 1:
				return model.q ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{q: true}),
					$author$project$Ports$focusInput(0));
			case 2:
				return model.q ? _Utils_Tuple2(
					_Utils_update(
						model,
						{q: false}),
					$author$project$Ports$blurInput(0)) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 3:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{q: false, af: false}),
					$elm$core$Platform$Cmd$none);
			case 4:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{af: true}),
					$elm$core$Platform$Cmd$none);
			case 5:
				var optionValue = msg.a;
				var updateOptions = A2($author$project$Option$highlightOptionInListByValue, optionValue, model.a);
				var optionsForTheDropdown = A2($author$project$Option$highlightOptionInListByValue, optionValue, model.e);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{a: updateOptions, e: optionsForTheDropdown}),
					$elm$core$Platform$Cmd$none);
			case 6:
				var optionValue = msg.a;
				var updatedOptions = A2($author$project$Option$removeHighlightOptionInList, optionValue, model.a);
				var optionsForTheDropdown = A2($author$project$Option$removeHighlightOptionInList, optionValue, model.e);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{a: updatedOptions, e: optionsForTheDropdown}),
					$elm$core$Platform$Cmd$none);
			case 7:
				var optionValue = msg.a;
				var options = function () {
					var _v1 = model.l;
					if (_v1.$ === 1) {
						return A2($author$project$Option$selectOptionInListByOptionValue, optionValue, model.a);
					} else {
						return A2($author$project$Option$selectSingleOptionInList, optionValue, model.a);
					}
				}();
				return _Utils_Tuple2(
					A4($author$project$Main$updateModelWithSearchStringChanges, model.g, '', options, model),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2(
								$author$project$Main$makeCommandMessagesWhenValuesChanges,
								options,
								$elm$core$Maybe$Just(optionValue)),
								$author$project$Ports$blurInput(0)
							])));
			case 8:
				var searchString = msg.a;
				return _Utils_Tuple2(
					A4($author$project$Main$updateModelWithSearchStringChanges, model.g, searchString, model.a, model),
					$author$project$Ports$inputKeyUp(searchString));
			case 9:
				var valuesJson = msg.a;
				var valuesResult = A2($elm$json$Json$Decode$decodeValue, $author$project$Ports$valuesDecoder, valuesJson);
				if (!valuesResult.$) {
					var values = valuesResult.a;
					var newOptions = A2($author$project$Option$addAndSelectOptionsInOptionsListByString, values, model.a);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a: newOptions,
								e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, newOptions),
								I: A3(
									$author$project$Main$updateRightSlot,
									model.I,
									model.l,
									$author$project$Option$hasSelectedOption(model.a))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = valuesResult.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 10:
				var optionsJson = msg.a;
				var _v3 = A2($elm$json$Json$Decode$decodeValue, $author$project$Option$optionsDecoder, optionsJson);
				if (!_v3.$) {
					var newOptions = _v3.a;
					var newOptionWithOldSelectedOption = function () {
						var _v4 = model.l;
						if (!_v4.$) {
							return A2($author$project$Option$mergeTwoListsOfOptionsPreservingSelectedOptions, model.a, newOptions);
						} else {
							return A2(
								$author$project$Option$mergeTwoListsOfOptionsPreservingSelectedOptions,
								model.a,
								A2(
									$elm$core$List$filter,
									A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$Option$isEmptyOption),
									newOptions));
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a: newOptionWithOldSelectedOption,
								e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, newOptionWithOldSelectedOption)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = _v3.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 11:
				var optionsJson = msg.a;
				var _v5 = A2($elm$json$Json$Decode$decodeValue, $author$project$Option$optionsDecoder, optionsJson);
				if (!_v5.$) {
					var newOptions = _v5.a;
					var updatedOptions = A2($author$project$Option$addAdditionalOptionsToOptionList, model.a, newOptions);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a: updatedOptions,
								e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, updatedOptions)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = _v5.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 12:
				var optionsJson = msg.a;
				var _v6 = A2($elm$json$Json$Decode$decodeValue, $author$project$Option$optionsDecoder, optionsJson);
				if (!_v6.$) {
					var optionsToRemove = _v6.a;
					var updatedOptions = A2($author$project$Option$removeOptionsFromOptionList, model.a, optionsToRemove);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a: updatedOptions,
								e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, updatedOptions)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = _v6.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 13:
				var optionJson = msg.a;
				var _v7 = A2($elm$json$Json$Decode$decodeValue, $author$project$Option$decoder, optionJson);
				if (!_v7.$) {
					var option = _v7.a;
					var optionValue = $author$project$Option$getOptionValue(option);
					var options = function () {
						var _v8 = model.l;
						if (_v8.$ === 1) {
							return A2($author$project$Option$selectOptionInListByOptionValue, optionValue, model.a);
						} else {
							return A2($author$project$Option$selectSingleOptionInList, optionValue, model.a);
						}
					}();
					return _Utils_Tuple2(
						A4($author$project$Main$updateModelWithSearchStringChanges, model.g, '', options, model),
						A2(
							$author$project$Main$makeCommandMessagesWhenValuesChanges,
							options,
							$elm$core$Maybe$Just(optionValue)));
				} else {
					var error = _v7.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 14:
				var optionJson = msg.a;
				var _v9 = A2($elm$json$Json$Decode$decodeValue, $author$project$Option$decoder, optionJson);
				if (!_v9.$) {
					var option = _v9.a;
					var optionValue = $author$project$Option$getOptionValue(option);
					var options = A2($author$project$Option$deselectOptionInListByOptionValue, optionValue, model.a);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a: options,
								e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, options)
							}),
						A2($author$project$Main$makeCommandMessagesWhenValuesChanges, options, $elm$core$Maybe$Nothing));
				} else {
					var error = _v9.a;
					return _Utils_Tuple2(
						model,
						$author$project$Ports$errorMessage(
							$elm$json$Json$Decode$errorToString(error)));
				}
			case 15:
				var newPlaceholder = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{G: newPlaceholder}),
					$elm$core$Platform$Cmd$none);
			case 16:
				var bool = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							I: A3(
								$author$project$Main$updateRightSlotLoading,
								bool,
								model.l,
								$author$project$Option$hasSelectedOption(model.a))
						}),
					$elm$core$Platform$Cmd$none);
			case 17:
				var _int = msg.a;
				var maxDropdownItems = $author$project$PositiveInt$new(_int);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							g: maxDropdownItems,
							e: A2($author$project$Main$figureOutWhichOptionsToShow, maxDropdownItems, model.a)
						}),
					$elm$core$Platform$Cmd$none);
			case 18:
				var canAddCustomOptions = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							l: A2($author$project$SelectionMode$setAllowCustomOptionsWithBool, canAddCustomOptions, model.l)
						}),
					$elm$core$Platform$Cmd$none);
			case 19:
				var bool = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{m: bool}),
					$elm$core$Platform$Cmd$none);
			case 20:
				var options = A2($author$project$Option$selectHighlightedOption, model.l, model.a);
				var _v10 = model.l;
				if (!_v10.$) {
					return _Utils_Tuple2(
						A4($author$project$Main$updateModelWithSearchStringChanges, model.g, '', options, model),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									A2($author$project$Main$makeCommandMessagesWhenValuesChanges, options, $elm$core$Maybe$Nothing),
									$author$project$Ports$blurInput(0)
								])));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{a: options, E: ''}),
						A2($author$project$Main$makeCommandMessagesWhenValuesChanges, options, $elm$core$Maybe$Nothing));
				}
			case 21:
				var _v11 = model.l;
				if (!_v11.$) {
					return $author$project$Option$hasSelectedOption(model.a) ? $author$project$Main$clearAllSelectedOption(model) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 22:
				return _Utils_Tuple2(
					A4($author$project$Main$updateModelWithSearchStringChanges, model.g, '', model.a, model),
					$author$project$Ports$blurInput(0));
			case 23:
				var updatedOptions = $author$project$Option$moveHighlightedOptionUp(model.a);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a: $author$project$Option$moveHighlightedOptionUp(model.a),
							e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, updatedOptions)
						}),
					$author$project$Ports$scrollDropdownToElement('something'));
			case 24:
				var updatedOptions = $author$project$Option$moveHighlightedOptionDown(model.a);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a: updatedOptions,
							e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, updatedOptions)
						}),
					$author$project$Ports$scrollDropdownToElement('something'));
			case 25:
				var dims = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ay: dims.cc, az: dims.cW}),
					$elm$core$Platform$Cmd$none);
			case 26:
				return $author$project$Main$clearAllSelectedOption(model);
			case 27:
				var updatedOptions = $author$project$Option$moveHighlightedOptionDown(model.a);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a: updatedOptions,
							e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, updatedOptions)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var newOptions = $author$project$Option$deselectAllSelectedHighlightedOptions(model.a);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a: newOptions,
							e: A2($author$project$Main$figureOutWhichOptionsToShow, model.g, newOptions)
						}),
					$author$project$Ports$valueChanged(
						$author$project$Option$selectedOptionsToTuple(newOptions)));
		}
	});
var $ohanhi$keyboard$Keyboard$ArrowDown = {$: 18};
var $ohanhi$keyboard$Keyboard$ArrowUp = {$: 21};
var $ohanhi$keyboard$Keyboard$Backspace = {$: 26};
var $author$project$Main$BringInputInFocus = {$: 1};
var $ohanhi$keyboard$Keyboard$Delete = {$: 31};
var $author$project$Main$DeleteSelectedAndHighlightedValues = {$: 28};
var $ohanhi$keyboard$Keyboard$Enter = {$: 15};
var $ohanhi$keyboard$Keyboard$Escape = {$: 62};
var $author$project$Main$EscapeKeyInInputFilter = {$: 22};
var $author$project$Main$InputBlur = {$: 3};
var $author$project$Main$InputFocus = {$: 4};
var $robinheghan$keyboard_events$Keyboard$Events$Keydown = 0;
var $author$project$Main$MoveHighlightedOptionDown = {$: 24};
var $author$project$Main$MoveHighlightedOptionUp = {$: 23};
var $author$project$Main$SearchInputOnInput = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$SelectHighlightedOption = {$: 20};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$defaultLoadingIndicator = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('default-loading-indicator')
		]),
	_List_Nil);
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$Main$DropdownMouseClickOption = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$DropdownMouseOutOption = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$DropdownMouseOverOption = function (a) {
	return {$: 5, a: a};
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Option$emptyOptionGroup = $author$project$Option$NoOptionGroup;
var $elm_community$list_extra$List$Extra$mapAccuml = F3(
	function (f, acc0, list) {
		var _v0 = A3(
			$elm$core$List$foldl,
			F2(
				function (x, _v1) {
					var acc1 = _v1.a;
					var ys = _v1.b;
					var _v2 = A2(f, acc1, x);
					var acc2 = _v2.a;
					var y = _v2.b;
					return _Utils_Tuple2(
						acc2,
						A2($elm$core$List$cons, y, ys));
				}),
			_Utils_Tuple2(acc0, _List_Nil),
			list);
		var accFinal = _v0.a;
		var generatedList = _v0.b;
		return _Utils_Tuple2(
			accFinal,
			$elm$core$List$reverse(generatedList));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 3, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $author$project$Main$mousedownPreventDefaultAndStopPropagation = function (message) {
	return A2(
		$elm$html$Html$Events$custom,
		'mousedown',
		$elm$json$Json$Decode$succeed(
			{M: message, P: true, R: true}));
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseenter',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Option$optionDescriptionToBool = function (optionDescription) {
	if (!optionDescription.$) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Option$optionGroupToString = function (optionGroup) {
	if (!optionGroup.$) {
		var string = optionGroup.a;
		return string;
	} else {
		return '';
	}
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$OptionPresentor$tokensToHtml = function (list) {
	return A2(
		$elm$core$List$map,
		function (_v0) {
			var highlighted = _v0.a;
			var string = _v0.b;
			return highlighted ? A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('highlight')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(string)
					])) : $elm$html$Html$text(string);
		},
		list);
};
var $author$project$Main$optionToDropdownOption = F6(
	function (mouseOverMsgConstructor, mouseOutMsgConstructor, clickMsgConstructor, selectionMode, prependOptionGroup, option) {
		var valueDataAttribute = A2(
			$elm$html$Html$Attributes$attribute,
			'data-value',
			$author$project$Option$getOptionValueAsString(option));
		var optionGroupHtml = prependOptionGroup ? A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('optgroup')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('optgroup-header')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Option$optionGroupToString(
								$author$project$Option$getOptionGroup(option)))
						]))
				])) : $elm$html$Html$text('');
		var labelHtml = function () {
			var _v4 = $author$project$Option$getMaybeOptionSearchFilter(option);
			if (!_v4.$) {
				var optionSearchFilter = _v4.a;
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					$author$project$OptionPresentor$tokensToHtml(optionSearchFilter.cl));
			} else {
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$OptionLabel$optionLabelToString(
								$author$project$Option$getOptionLabel(option)))
						]));
			}
		}();
		var descriptionHtml = function () {
			if ($author$project$Option$optionDescriptionToBool(
				$author$project$Option$getOptionDescription(option))) {
				var _v3 = $author$project$Option$getMaybeOptionSearchFilter(option);
				if (!_v3.$) {
					var optionSearchFilter = _v3.a;
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('description')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								$author$project$OptionPresentor$tokensToHtml(optionSearchFilter.b3))
							]));
				} else {
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('description')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Option$optionDescriptionToString(
											$author$project$Option$getOptionDescription(option)))
									]))
							]));
				}
			} else {
				return $elm$html$Html$text('');
			}
		}();
		var _v0 = $author$project$Option$getOptionDisplay(option);
		switch (_v0) {
			case 0:
				return _List_fromArray(
					[
						optionGroupHtml,
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseEnter(
								mouseOverMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$elm$html$Html$Events$onMouseLeave(
								mouseOutMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$author$project$Main$mousedownPreventDefaultAndStopPropagation(
								clickMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$elm$html$Html$Attributes$class('option'),
								valueDataAttribute
							]),
						_List_fromArray(
							[labelHtml, descriptionHtml]))
					]);
			case 1:
				return _List_fromArray(
					[
						optionGroupHtml,
						$elm$html$Html$text('')
					]);
			case 2:
				if (!selectionMode.$) {
					return _List_fromArray(
						[
							optionGroupHtml,
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Events$onMouseEnter(
									mouseOverMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$elm$html$Html$Events$onMouseLeave(
									mouseOutMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$author$project$Main$mousedownPreventDefaultAndStopPropagation(
									clickMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$elm$html$Html$Attributes$class('selected'),
									$elm$html$Html$Attributes$class('option'),
									valueDataAttribute
								]),
							_List_fromArray(
								[labelHtml, descriptionHtml]))
						]);
				} else {
					return _List_fromArray(
						[
							optionGroupHtml,
							$elm$html$Html$text('')
						]);
				}
			case 3:
				if (!selectionMode.$) {
					return _List_fromArray(
						[
							optionGroupHtml,
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Events$onMouseEnter(
									mouseOverMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$elm$html$Html$Events$onMouseLeave(
									mouseOutMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$author$project$Main$mousedownPreventDefaultAndStopPropagation(
									clickMsgConstructor(
										$author$project$Option$getOptionValue(option))),
									$elm$html$Html$Attributes$class('selected'),
									$elm$html$Html$Attributes$class('highlighted'),
									$elm$html$Html$Attributes$class('option'),
									valueDataAttribute
								]),
							_List_fromArray(
								[labelHtml, descriptionHtml]))
						]);
				} else {
					return _List_fromArray(
						[
							optionGroupHtml,
							$elm$html$Html$text('')
						]);
				}
			case 4:
				return _List_fromArray(
					[
						optionGroupHtml,
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseEnter(
								mouseOverMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$elm$html$Html$Events$onMouseLeave(
								mouseOutMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$author$project$Main$mousedownPreventDefaultAndStopPropagation(
								clickMsgConstructor(
									$author$project$Option$getOptionValue(option))),
								$elm$html$Html$Attributes$class('highlighted'),
								$elm$html$Html$Attributes$class('option'),
								valueDataAttribute
							]),
						_List_fromArray(
							[labelHtml, descriptionHtml]))
					]);
			default:
				return _List_fromArray(
					[
						optionGroupHtml,
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('disabled'),
								$elm$html$Html$Attributes$class('option'),
								valueDataAttribute
							]),
						_List_fromArray(
							[labelHtml, descriptionHtml]))
					]);
		}
	});
var $author$project$Main$optionsToDropdownOptions = F5(
	function (mouseOverMsgConstructor, mouseOutMsgConstructor, clickMsgConstructor, selectionMode, options) {
		var partialWithSelectionMode = A4($author$project$Main$optionToDropdownOption, mouseOverMsgConstructor, mouseOutMsgConstructor, clickMsgConstructor, selectionMode);
		var helper = F2(
			function (previousGroup, option_) {
				return _Utils_Tuple2(
					$author$project$Option$getOptionGroup(option_),
					A2(
						partialWithSelectionMode,
						!_Utils_eq(
							previousGroup,
							$author$project$Option$getOptionGroup(option_)),
						option_));
			});
		return A2(
			$elm$core$List$filter,
			function (htmlTag) {
				return !_Utils_eq(
					htmlTag,
					$elm$html$Html$text(''));
			},
			$elm$core$List$concat(
				A3($elm_community$list_extra$List$Extra$mapAccuml, helper, $author$project$Option$emptyOptionGroup, options).b));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$dropdown = function (model) {
	var optionsHtml = A5($author$project$Main$optionsToDropdownOptions, $author$project$Main$DropdownMouseOverOption, $author$project$Main$DropdownMouseOutOption, $author$project$Main$DropdownMouseClickOption, model.l, model.e);
	var dropdownFooterHtml = (_Utils_cmp(
		$elm$core$List$length(model.e),
		$elm$core$List$length(model.a)) < 0) ? A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('dropdown-footer')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				'showing ' + ($elm$core$String$fromInt(
					$elm$core$List$length(model.e)) + (' of ' + ($elm$core$String$fromInt(
					$elm$core$List$length(model.a)) + ' options'))))
			])) : $elm$html$Html$text('');
	var dropdownCss = _List_fromArray(
		[
			A2(
			$elm$html$Html$Attributes$style,
			'top',
			$elm$core$String$fromFloat(model.ay) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'width',
			$elm$core$String$fromFloat(model.az) + 'px')
		]);
	return model.m ? $elm$html$Html$text('') : ((model.af && ((!$elm$core$List$isEmpty(model.e)) && (!$elm$core$List$isEmpty(optionsHtml)))) ? A2(
		$elm$html$Html$div,
		_Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('dropdown'),
					$elm$html$Html$Attributes$class('showing')
				]),
			dropdownCss),
		_Utils_ap(
			optionsHtml,
			_List_fromArray(
				[dropdownFooterHtml]))) : A2(
		$elm$html$Html$div,
		_Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('dropdown'),
					$elm$html$Html$Attributes$class('hiding')
				]),
			dropdownCss),
		_Utils_ap(
			optionsHtml,
			_List_fromArray(
				[dropdownFooterHtml]))));
};
var $author$project$Main$dropdownIndicator = F3(
	function (focused, disabled, hasOptions) {
		return (disabled || (!hasOptions)) ? $elm$html$Html$text('') : A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('dropdown-indicator'),
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('down', focused),
							_Utils_Tuple2('up', !focused)
						]))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('▾')
				]));
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $robinheghan$keyboard_events$Keyboard$Events$eventToString = function (event) {
	switch (event) {
		case 0:
			return 'keydown';
		case 1:
			return 'keyup';
		default:
			return 'keypress';
	}
};
var $ohanhi$keyboard$Keyboard$Clear = {$: 27};
var $ohanhi$keyboard$Keyboard$Copy = {$: 28};
var $ohanhi$keyboard$Keyboard$CrSel = {$: 29};
var $ohanhi$keyboard$Keyboard$Cut = {$: 30};
var $ohanhi$keyboard$Keyboard$EraseEof = {$: 32};
var $ohanhi$keyboard$Keyboard$ExSel = {$: 33};
var $ohanhi$keyboard$Keyboard$Insert = {$: 34};
var $ohanhi$keyboard$Keyboard$Paste = {$: 35};
var $ohanhi$keyboard$Keyboard$Redo = {$: 36};
var $ohanhi$keyboard$Keyboard$Undo = {$: 37};
var $ohanhi$keyboard$Keyboard$editingKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'Backspace':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Backspace);
		case 'Clear':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Clear);
		case 'Copy':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Copy);
		case 'CrSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CrSel);
		case 'Cut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cut);
		case 'Delete':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Delete);
		case 'EraseEof':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EraseEof);
		case 'ExSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ExSel);
		case 'Insert':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Insert);
		case 'Paste':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Paste);
		case 'Redo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Redo);
		case 'Undo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Undo);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$F1 = {$: 38};
var $ohanhi$keyboard$Keyboard$F10 = {$: 47};
var $ohanhi$keyboard$Keyboard$F11 = {$: 48};
var $ohanhi$keyboard$Keyboard$F12 = {$: 49};
var $ohanhi$keyboard$Keyboard$F13 = {$: 50};
var $ohanhi$keyboard$Keyboard$F14 = {$: 51};
var $ohanhi$keyboard$Keyboard$F15 = {$: 52};
var $ohanhi$keyboard$Keyboard$F16 = {$: 53};
var $ohanhi$keyboard$Keyboard$F17 = {$: 54};
var $ohanhi$keyboard$Keyboard$F18 = {$: 55};
var $ohanhi$keyboard$Keyboard$F19 = {$: 56};
var $ohanhi$keyboard$Keyboard$F2 = {$: 39};
var $ohanhi$keyboard$Keyboard$F20 = {$: 57};
var $ohanhi$keyboard$Keyboard$F3 = {$: 40};
var $ohanhi$keyboard$Keyboard$F4 = {$: 41};
var $ohanhi$keyboard$Keyboard$F5 = {$: 42};
var $ohanhi$keyboard$Keyboard$F6 = {$: 43};
var $ohanhi$keyboard$Keyboard$F7 = {$: 44};
var $ohanhi$keyboard$Keyboard$F8 = {$: 45};
var $ohanhi$keyboard$Keyboard$F9 = {$: 46};
var $ohanhi$keyboard$Keyboard$functionKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'F1':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F1);
		case 'F2':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F2);
		case 'F3':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F3);
		case 'F4':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F4);
		case 'F5':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F5);
		case 'F6':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F6);
		case 'F7':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F7);
		case 'F8':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F8);
		case 'F9':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F9);
		case 'F10':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F10);
		case 'F11':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F11);
		case 'F12':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F12);
		case 'F13':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F13);
		case 'F14':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F14);
		case 'F15':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F15);
		case 'F16':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F16);
		case 'F17':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F17);
		case 'F18':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F18);
		case 'F19':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F19);
		case 'F20':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F20);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$ChannelDown = {$: 85};
var $ohanhi$keyboard$Keyboard$ChannelUp = {$: 86};
var $ohanhi$keyboard$Keyboard$MediaFastForward = {$: 87};
var $ohanhi$keyboard$Keyboard$MediaPause = {$: 88};
var $ohanhi$keyboard$Keyboard$MediaPlay = {$: 89};
var $ohanhi$keyboard$Keyboard$MediaPlayPause = {$: 90};
var $ohanhi$keyboard$Keyboard$MediaRecord = {$: 91};
var $ohanhi$keyboard$Keyboard$MediaRewind = {$: 92};
var $ohanhi$keyboard$Keyboard$MediaStop = {$: 93};
var $ohanhi$keyboard$Keyboard$MediaTrackNext = {$: 94};
var $ohanhi$keyboard$Keyboard$MediaTrackPrevious = {$: 95};
var $ohanhi$keyboard$Keyboard$mediaKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'ChannelDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelDown);
		case 'ChannelUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelUp);
		case 'MediaFastForward':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaFastForward);
		case 'MediaPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPause);
		case 'MediaPlay':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlay);
		case 'MediaPlayPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlayPause);
		case 'MediaRecord':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRecord);
		case 'MediaRewind':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRewind);
		case 'MediaStop':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaStop);
		case 'MediaTrackNext':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackNext);
		case 'MediaTrackPrevious':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackPrevious);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Alt = {$: 1};
var $ohanhi$keyboard$Keyboard$AltGraph = {$: 2};
var $ohanhi$keyboard$Keyboard$CapsLock = {$: 3};
var $ohanhi$keyboard$Keyboard$Control = {$: 4};
var $ohanhi$keyboard$Keyboard$Fn = {$: 5};
var $ohanhi$keyboard$Keyboard$FnLock = {$: 6};
var $ohanhi$keyboard$Keyboard$Hyper = {$: 7};
var $ohanhi$keyboard$Keyboard$Meta = {$: 8};
var $ohanhi$keyboard$Keyboard$NumLock = {$: 9};
var $ohanhi$keyboard$Keyboard$ScrollLock = {$: 10};
var $ohanhi$keyboard$Keyboard$Shift = {$: 11};
var $ohanhi$keyboard$Keyboard$Super = {$: 12};
var $ohanhi$keyboard$Keyboard$Symbol = {$: 13};
var $ohanhi$keyboard$Keyboard$SymbolLock = {$: 14};
var $ohanhi$keyboard$Keyboard$modifierKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'Alt':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Alt);
		case 'AltGraph':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AltGraph);
		case 'CapsLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CapsLock);
		case 'Control':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Control);
		case 'Fn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Fn);
		case 'FnLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$FnLock);
		case 'Hyper':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Hyper);
		case 'Meta':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Meta);
		case 'NumLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$NumLock);
		case 'ScrollLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ScrollLock);
		case 'Shift':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Shift);
		case 'Super':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'OS':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'Symbol':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Symbol);
		case 'SymbolLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$SymbolLock);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$ArrowLeft = {$: 19};
var $ohanhi$keyboard$Keyboard$ArrowRight = {$: 20};
var $ohanhi$keyboard$Keyboard$End = {$: 22};
var $ohanhi$keyboard$Keyboard$Home = {$: 23};
var $ohanhi$keyboard$Keyboard$PageDown = {$: 24};
var $ohanhi$keyboard$Keyboard$PageUp = {$: 25};
var $ohanhi$keyboard$Keyboard$navigationKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'ArrowDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'ArrowLeft':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'ArrowRight':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'ArrowUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'Down':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'Left':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'Right':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'Up':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'End':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$End);
		case 'Home':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Home);
		case 'PageDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageDown);
		case 'PageUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageUp);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$oneOf = F2(
	function (fns, key) {
		oneOf:
		while (true) {
			if (!fns.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var fn = fns.a;
				var rest = fns.b;
				var _v1 = fn(key);
				if (!_v1.$) {
					var a = _v1.a;
					return $elm$core$Maybe$Just(a);
				} else {
					var $temp$fns = rest,
						$temp$key = key;
					fns = $temp$fns;
					key = $temp$key;
					continue oneOf;
				}
			}
		}
	});
var $ohanhi$keyboard$Keyboard$AppSwitch = {$: 73};
var $ohanhi$keyboard$Keyboard$Call = {$: 74};
var $ohanhi$keyboard$Keyboard$Camera = {$: 75};
var $ohanhi$keyboard$Keyboard$CameraFocus = {$: 76};
var $ohanhi$keyboard$Keyboard$EndCall = {$: 77};
var $ohanhi$keyboard$Keyboard$GoBack = {$: 78};
var $ohanhi$keyboard$Keyboard$GoHome = {$: 79};
var $ohanhi$keyboard$Keyboard$HeadsetHook = {$: 80};
var $ohanhi$keyboard$Keyboard$LastNumberRedial = {$: 81};
var $ohanhi$keyboard$Keyboard$MannerMode = {$: 83};
var $ohanhi$keyboard$Keyboard$Notification = {$: 82};
var $ohanhi$keyboard$Keyboard$VoiceDial = {$: 84};
var $ohanhi$keyboard$Keyboard$phoneKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'AppSwitch':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AppSwitch);
		case 'Call':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Call);
		case 'Camera':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Camera);
		case 'CameraFocus':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CameraFocus);
		case 'EndCall':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EndCall);
		case 'GoBack':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoBack);
		case 'GoHome':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoHome);
		case 'HeadsetHook':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$HeadsetHook);
		case 'LastNumberRedial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$LastNumberRedial);
		case 'Notification':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Notification);
		case 'MannerMode':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MannerMode);
		case 'VoiceDial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$VoiceDial);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Again = {$: 58};
var $ohanhi$keyboard$Keyboard$Attn = {$: 59};
var $ohanhi$keyboard$Keyboard$Cancel = {$: 60};
var $ohanhi$keyboard$Keyboard$ContextMenu = {$: 61};
var $ohanhi$keyboard$Keyboard$Execute = {$: 63};
var $ohanhi$keyboard$Keyboard$Find = {$: 64};
var $ohanhi$keyboard$Keyboard$Finish = {$: 65};
var $ohanhi$keyboard$Keyboard$Help = {$: 66};
var $ohanhi$keyboard$Keyboard$Pause = {$: 67};
var $ohanhi$keyboard$Keyboard$Play = {$: 68};
var $ohanhi$keyboard$Keyboard$Props = {$: 69};
var $ohanhi$keyboard$Keyboard$Select = {$: 70};
var $ohanhi$keyboard$Keyboard$ZoomIn = {$: 71};
var $ohanhi$keyboard$Keyboard$ZoomOut = {$: 72};
var $ohanhi$keyboard$Keyboard$uiKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'Again':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Again);
		case 'Attn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Attn);
		case 'Cancel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cancel);
		case 'ContextMenu':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ContextMenu);
		case 'Escape':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Escape);
		case 'Execute':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Execute);
		case 'Find':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Find);
		case 'Finish':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Finish);
		case 'Help':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Help);
		case 'Pause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Pause);
		case 'Play':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Play);
		case 'Props':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Props);
		case 'Select':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Select);
		case 'ZoomIn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomIn);
		case 'ZoomOut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomOut);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Spacebar = {$: 17};
var $ohanhi$keyboard$Keyboard$Tab = {$: 16};
var $ohanhi$keyboard$Keyboard$whitespaceKey = function (_v0) {
	var value = _v0;
	switch (value) {
		case 'Enter':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Enter);
		case 'Tab':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Tab);
		case 'Spacebar':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		case ' ':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$anyKeyWith = function (charParser) {
	return $ohanhi$keyboard$Keyboard$oneOf(
		_List_fromArray(
			[$ohanhi$keyboard$Keyboard$whitespaceKey, charParser, $ohanhi$keyboard$Keyboard$modifierKey, $ohanhi$keyboard$Keyboard$navigationKey, $ohanhi$keyboard$Keyboard$editingKey, $ohanhi$keyboard$Keyboard$functionKey, $ohanhi$keyboard$Keyboard$uiKey, $ohanhi$keyboard$Keyboard$phoneKey, $ohanhi$keyboard$Keyboard$mediaKey]));
};
var $ohanhi$keyboard$Keyboard$Character = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$toUpper = _String_toUpper;
var $ohanhi$keyboard$Keyboard$characterKeyUpper = function (_v0) {
	var value = _v0;
	return ($elm$core$String$length(value) === 1) ? $elm$core$Maybe$Just(
		$ohanhi$keyboard$Keyboard$Character(
			$elm$core$String$toUpper(value))) : $elm$core$Maybe$Nothing;
};
var $ohanhi$keyboard$Keyboard$anyKeyUpper = $ohanhi$keyboard$Keyboard$anyKeyWith($ohanhi$keyboard$Keyboard$characterKeyUpper);
var $ohanhi$keyboard$Keyboard$RawKey = $elm$core$Basics$identity;
var $ohanhi$keyboard$Keyboard$eventKeyDecoder = A2(
	$elm$json$Json$Decode$field,
	'key',
	A2($elm$json$Json$Decode$map, $elm$core$Basics$identity, $elm$json$Json$Decode$string));
var $robinheghan$keyboard_events$Keyboard$Events$messageSelector = function (decisionMap) {
	var helper = function (maybeKey) {
		if (maybeKey.$ === 1) {
			return $elm$json$Json$Decode$fail('No key we\'re interested in');
		} else {
			var key = maybeKey.a;
			return A2(
				$elm$core$Maybe$withDefault,
				$elm$json$Json$Decode$fail('No key we\'re interested in'),
				A2(
					$elm$core$Maybe$map,
					$elm$json$Json$Decode$succeed,
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$second,
						$elm$core$List$head(
							A2(
								$elm$core$List$filter,
								function (_v1) {
									var k = _v1.a;
									return _Utils_eq(k, key);
								},
								decisionMap)))));
		}
	};
	return A2(
		$elm$json$Json$Decode$andThen,
		helper,
		A2($elm$json$Json$Decode$map, $ohanhi$keyboard$Keyboard$anyKeyUpper, $ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var $robinheghan$keyboard_events$Keyboard$Events$on = F2(
	function (event, decisionMap) {
		return A2(
			$elm$html$Html$Events$on,
			$robinheghan$keyboard_events$Keyboard$Events$eventToString(event),
			$robinheghan$keyboard_events$Keyboard$Events$messageSelector(decisionMap));
	});
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Main$ToggleSelectedValueHighlight = function (a) {
	return {$: 27, a: a};
};
var $author$project$Main$optionsToValuesHtml = function (options) {
	return A2(
		$elm$core$List$map,
		function (option) {
			switch (option.$) {
				case 0:
					var display = option.a;
					var _v1 = option.b;
					var labelStr = _v1.a;
					var optionValue = option.c;
					switch (display) {
						case 0:
							return $elm$html$Html$text('');
						case 1:
							return $elm$html$Html$text('');
						case 2:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('value'),
										$author$project$Main$mousedownPreventDefaultAndStopPropagation(
										$author$project$Main$ToggleSelectedValueHighlight(optionValue))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(labelStr)
									]));
						case 3:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2('value', true),
												_Utils_Tuple2('selected-value', true)
											])),
										$author$project$Main$mousedownPreventDefaultAndStopPropagation(
										$author$project$Main$ToggleSelectedValueHighlight(optionValue))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(labelStr)
									]));
						case 4:
							return $elm$html$Html$text('');
						default:
							return $elm$html$Html$text('');
					}
				case 1:
					var display = option.a;
					var _v3 = option.b;
					var labelStr = _v3.a;
					var optionValue = option.c;
					switch (display) {
						case 0:
							return $elm$html$Html$text('');
						case 1:
							return $elm$html$Html$text('');
						case 2:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('value'),
										$author$project$Main$mousedownPreventDefaultAndStopPropagation(
										$author$project$Main$ToggleSelectedValueHighlight(optionValue))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(labelStr)
									]));
						case 3:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2('value', true),
												_Utils_Tuple2('selected-value', true)
											])),
										$author$project$Main$mousedownPreventDefaultAndStopPropagation(
										$author$project$Main$ToggleSelectedValueHighlight(optionValue))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(labelStr)
									]));
						case 4:
							return $elm$html$Html$text('');
						default:
							return $elm$html$Html$text('');
					}
				default:
					var display = option.a;
					var _v5 = option.b;
					var labelStr = _v5.a;
					switch (display) {
						case 0:
							return $elm$html$Html$text('');
						case 1:
							return $elm$html$Html$text('');
						case 2:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('value')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(labelStr)
									]));
						case 3:
							return $elm$html$Html$text('');
						case 4:
							return $elm$html$Html$text('');
						default:
							return $elm$html$Html$text('');
					}
			}
		},
		options);
};
var $author$project$Main$ClearAllSelectedOptions = {$: 26};
var $author$project$Main$rightSlotHtml = F4(
	function (rightSlot, focused, disabled, hasOptionSelected) {
		switch (rightSlot) {
			case 0:
				return $elm$html$Html$text('');
			case 1:
				return A3(
					$elm$html$Html$node,
					'slot',
					_List_fromArray(
						[
							$elm$html$Html$Attributes$name('loading-indicator')
						]),
					_List_fromArray(
						[$author$project$Main$defaultLoadingIndicator]));
			case 2:
				return A3($author$project$Main$dropdownIndicator, focused, disabled, hasOptionSelected);
			default:
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('clear-button-wrapper'),
							$elm$html$Html$Events$onClick($author$project$Main$ClearAllSelectedOptions)
						]),
					_List_fromArray(
						[
							A3(
							$elm$html$Html$node,
							'slot',
							_List_fromArray(
								[
									$elm$html$Html$Attributes$name('clear-button')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('✕')
								]))
						]));
		}
	});
var $author$project$Main$DeleteInputForSingleSelect = {$: 21};
var $robinheghan$keyboard_events$Keyboard$Events$customPerKey = F2(
	function (event, decisionMap) {
		return A2(
			$elm$html$Html$Events$custom,
			$robinheghan$keyboard_events$Keyboard$Events$eventToString(event),
			$robinheghan$keyboard_events$Keyboard$Events$messageSelector(decisionMap));
	});
var $elm$html$Html$Attributes$maxlength = function (n) {
	return A2(
		_VirtualDom_attribute,
		'maxlength',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$singleSelectInputField = F5(
	function (searchString, isDisabled, focused, placeholder_, hasSelectedOptions) {
		var typeAttr = $elm$html$Html$Attributes$type_('text');
		var showPlaceholder = (!hasSelectedOptions) && (!focused);
		var placeholderAttribute = showPlaceholder ? $elm$html$Html$Attributes$placeholder(placeholder_) : A2($elm$html$Html$Attributes$style, '', '');
		var onFocusAttr = $elm$html$Html$Events$onFocus($author$project$Main$InputFocus);
		var onBlurAttr = $elm$html$Html$Events$onBlur($author$project$Main$InputBlur);
		var keyboardEvents = A2(
			$robinheghan$keyboard_events$Keyboard$Events$customPerKey,
			0,
			_List_fromArray(
				[
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$Enter,
					{M: $author$project$Main$SelectHighlightedOption, P: false, R: false}),
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$Backspace,
					{M: $author$project$Main$DeleteInputForSingleSelect, P: false, R: false}),
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$Delete,
					{M: $author$project$Main$DeleteInputForSingleSelect, P: false, R: false}),
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$Escape,
					{M: $author$project$Main$EscapeKeyInInputFilter, P: false, R: false}),
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$ArrowUp,
					{M: $author$project$Main$MoveHighlightedOptionUp, P: true, R: false}),
					_Utils_Tuple2(
					$ohanhi$keyboard$Keyboard$ArrowDown,
					{M: $author$project$Main$MoveHighlightedOptionDown, P: true, R: false})
				]));
		var idAttr = $elm$html$Html$Attributes$id('input-filter');
		return isDisabled ? A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$disabled(true),
					idAttr,
					placeholderAttribute
				]),
			_List_Nil) : (hasSelectedOptions ? A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					typeAttr,
					idAttr,
					onBlurAttr,
					onFocusAttr,
					$elm$html$Html$Attributes$value(''),
					$elm$html$Html$Attributes$maxlength(0),
					placeholderAttribute,
					keyboardEvents
				]),
			_List_Nil) : A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					typeAttr,
					idAttr,
					onBlurAttr,
					onFocusAttr,
					$elm$html$Html$Events$onInput($author$project$Main$SearchInputOnInput),
					$elm$html$Html$Attributes$value(searchString),
					placeholderAttribute,
					keyboardEvents
				]),
			_List_Nil));
	});
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $author$project$Main$view = function (model) {
	var tabIndexAttribute = model.m ? A2($elm$html$Html$Attributes$style, '', '') : $elm$html$Html$Attributes$tabindex(0);
	var _v0 = model.l;
	if (!_v0.$) {
		var hasOptions = (!$elm$core$List$isEmpty(model.a)) && $elm$core$String$isEmpty(model.E);
		var hasOptionSelected = $author$project$Option$hasSelectedOption(model.a);
		var showPlaceholder = (!hasOptionSelected) && (!model.q);
		var valueStr = hasOptionSelected ? A2(
			$elm$core$Maybe$withDefault,
			'',
			$elm$core$List$head(
				A2(
					$elm$core$List$map,
					$elm$core$Tuple$second,
					$author$project$Option$selectedOptionsToTuple(model.a)))) : '';
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('wrapper')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('value-casing'),
							$elm$html$Html$Events$onMouseDown($author$project$Main$BringInputInFocus),
							$elm$html$Html$Events$onFocus($author$project$Main$BringInputInFocus),
							tabIndexAttribute,
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('show-placeholder', showPlaceholder),
									_Utils_Tuple2('has-option-selected', hasOptionSelected),
									_Utils_Tuple2('no-option-selected', !hasOptionSelected),
									_Utils_Tuple2('single', true),
									_Utils_Tuple2('disabled', model.m),
									_Utils_Tuple2('focused', model.q),
									_Utils_Tuple2('not-focused', model.q)
								]))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('selected-value')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(valueStr)
								])),
							A5($author$project$Main$singleSelectInputField, model.E, model.m, model.q, model.G, hasOptionSelected),
							function () {
							var _v1 = model.I;
							switch (_v1) {
								case 0:
									return $elm$html$Html$text('');
								case 1:
									return A3(
										$elm$html$Html$node,
										'slot',
										_List_fromArray(
											[
												$elm$html$Html$Attributes$name('loading-indicator')
											]),
										_List_fromArray(
											[$author$project$Main$defaultLoadingIndicator]));
								case 2:
									return A3($author$project$Main$dropdownIndicator, model.q, model.m, hasOptions);
								default:
									return A3(
										$elm$html$Html$node,
										'slot',
										_List_fromArray(
											[
												$elm$html$Html$Attributes$name('clear-button')
											]),
										_List_Nil);
							}
						}(),
							$author$project$Main$dropdown(model)
						]))
				]));
	} else {
		var inputFilter = A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$type_('text'),
					$elm$html$Html$Events$onBlur($author$project$Main$InputBlur),
					$elm$html$Html$Events$onFocus($author$project$Main$InputFocus),
					$elm$html$Html$Events$onInput($author$project$Main$SearchInputOnInput),
					$elm$html$Html$Attributes$value(model.E),
					$elm$html$Html$Attributes$id('input-filter'),
					$elm$html$Html$Attributes$disabled(model.m),
					A2(
					$robinheghan$keyboard_events$Keyboard$Events$on,
					0,
					_List_fromArray(
						[
							_Utils_Tuple2($ohanhi$keyboard$Keyboard$Enter, $author$project$Main$SelectHighlightedOption),
							_Utils_Tuple2($ohanhi$keyboard$Keyboard$Escape, $author$project$Main$EscapeKeyInInputFilter),
							_Utils_Tuple2($ohanhi$keyboard$Keyboard$ArrowUp, $author$project$Main$MoveHighlightedOptionUp),
							_Utils_Tuple2($ohanhi$keyboard$Keyboard$ArrowDown, $author$project$Main$MoveHighlightedOptionDown)
						]))
				]),
			_List_Nil);
		var hasOptionSelected = $author$project$Option$hasSelectedOption(model.a);
		var showPlaceholder = (!hasOptionSelected) && (!model.q);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('wrapper'),
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('disabled', model.m)
						]))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('value-casing'),
							$elm$html$Html$Events$onClick($author$project$Main$BringInputInFocus),
							$elm$html$Html$Events$onFocus($author$project$Main$BringInputInFocus),
							A2(
							$robinheghan$keyboard_events$Keyboard$Events$on,
							0,
							_List_fromArray(
								[
									_Utils_Tuple2($ohanhi$keyboard$Keyboard$Delete, $author$project$Main$DeleteSelectedAndHighlightedValues),
									_Utils_Tuple2($ohanhi$keyboard$Keyboard$Backspace, $author$project$Main$DeleteSelectedAndHighlightedValues)
								])),
							tabIndexAttribute,
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('placeholder', showPlaceholder),
									_Utils_Tuple2('multi', true),
									_Utils_Tuple2('disabled', model.m)
								]))
						]),
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('placeholder')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(model.G)
									]))
							]),
						_Utils_ap(
							$author$project$Main$optionsToValuesHtml(model.a),
							_List_fromArray(
								[
									inputFilter,
									A4($author$project$Main$rightSlotHtml, model.I, model.q, model.m, hasOptionSelected)
								])))),
					$author$project$Main$dropdown(model)
				]));
	}
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{ch: $author$project$Main$init, cP: $author$project$Main$subscriptions, cU: $author$project$Main$update, cV: $author$project$Main$view});
/*
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (value) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (size) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (placeholder) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (optionsJson) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (maxDropdownItems) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (loading) {
													return A2(
														$elm$json$Json$Decode$andThen,
														function (disabled) {
															return A2(
																$elm$json$Json$Decode$andThen,
																function (allowMultiSelect) {
																	return A2(
																		$elm$json$Json$Decode$andThen,
																		function (allowCustomOptions) {
																			return $elm$json$Json$Decode$succeed(
																				{aA: allowCustomOptions, aB: allowMultiSelect, m: disabled, aH: loading, g: maxDropdownItems, aN: optionsJson, G: placeholder, ag: size, aT: value});
																		},
																		A2($elm$json$Json$Decode$field, 'allowCustomOptions', $elm$json$Json$Decode$bool));
																},
																A2($elm$json$Json$Decode$field, 'allowMultiSelect', $elm$json$Json$Decode$bool));
														},
														A2($elm$json$Json$Decode$field, 'disabled', $elm$json$Json$Decode$bool));
												},
												A2($elm$json$Json$Decode$field, 'loading', $elm$json$Json$Decode$bool));
										},
										A2($elm$json$Json$Decode$field, 'maxDropdownItems', $elm$json$Json$Decode$int));
								},
								A2($elm$json$Json$Decode$field, 'optionsJson', $elm$json$Json$Decode$string));
						},
						A2($elm$json$Json$Decode$field, 'placeholder', $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$field, 'size', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string)))(0)}});}(this));
*/
export const Elm = {'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (value) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (size) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (placeholder) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (optionsJson) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (maxDropdownItems) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (loading) {
													return A2(
														$elm$json$Json$Decode$andThen,
														function (disabled) {
															return A2(
																$elm$json$Json$Decode$andThen,
																function (allowMultiSelect) {
																	return A2(
																		$elm$json$Json$Decode$andThen,
																		function (allowCustomOptions) {
																			return $elm$json$Json$Decode$succeed(
																				{aA: allowCustomOptions, aB: allowMultiSelect, m: disabled, aH: loading, g: maxDropdownItems, aN: optionsJson, G: placeholder, ag: size, aT: value});
																		},
																		A2($elm$json$Json$Decode$field, 'allowCustomOptions', $elm$json$Json$Decode$bool));
																},
																A2($elm$json$Json$Decode$field, 'allowMultiSelect', $elm$json$Json$Decode$bool));
														},
														A2($elm$json$Json$Decode$field, 'disabled', $elm$json$Json$Decode$bool));
												},
												A2($elm$json$Json$Decode$field, 'loading', $elm$json$Json$Decode$bool));
										},
										A2($elm$json$Json$Decode$field, 'maxDropdownItems', $elm$json$Json$Decode$int));
								},
								A2($elm$json$Json$Decode$field, 'optionsJson', $elm$json$Json$Decode$string));
						},
						A2($elm$json$Json$Decode$field, 'placeholder', $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$field, 'size', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string)))(0)}};
