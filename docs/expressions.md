## Expressions: Bidirectional Evaluation

Evaluation of expressions can occur in two directions: **Up** and **down** the abstraction stack. *[Maybe to be called push and pull evaluation?]*

**Down-evaluation** works like evaluation in traditional programming languages: return value is a function of one or more subexpressions. Down-evaluation is functionally pure. In general, down-evaluation is used to prepare data for output from the computer through a device.

**Up-evaluation** can be thought of as pushing a value into the expression from the "return" side, which then pushes values into its subexpressions. The evaluation either "matches" or "fails". It has semantics similar to the left side of a [destructuring assignment](http://coffeescript.org/#destructuring), but also like an assertion, both matching and capturing data like a regexp group. Up-evaluation is idempotent -- Only the first up-evaluation of an expression may have side effects. If a value matches, it will continue to match if up-evaluated with the same expression again. In general, up-evaluation is used to interpret input data from a device.

### Literals

bit literals: `'1010`, `'xAA55`  
symbol literals: `#abc`  
number literals: `42.1`  
unit literals: `3.3V`  

**down:** Evaluates to itself.  
**up:** Match if the pushed value is equal, or fail if the pushed value is not equal.

### Ignore

	_
(underscore `_` or `ignore`)

**down:** Evaluates to itself. When used as an operand, that operator returns `ignore` too. It is an error for `ignore` to reach hardware.  
**up:** Pushed value is discarded, match always succeeds.

### Arithmetic expressions

types: numbers

	a+b, a*b, a-b, a/b

**Down:** Down-evaluate both arguments and perform operation.  
**Up:** If one argument is constant, perform the inverse operation with the pushed value and the constant and up-evaluate the non-constant argument with the result.

### Switch

	x!y

**down:** Down-evaluation of `x`.  
**up:** Up-evaluate `y` with the pushed value.

*[Needs a better name; has nothing to do with switch/when block.]*

### Out

	<: x

**down:** Down-evaluation of `x`.  
**up:** Ignored.

Syntactic sugar for `x ! _`

### In

	:> y

**down:** `ignore`.  
**up:** Up-evaluate `y` with the pushed value,

Syntactic sugar for `_ ! y`

*[it is unambiguous for `:>` and `<:` to imply an `=` if used directly next to a parameter name or let declaration, so we may allow that. Ex: `sample(v = <: 3.3V, i = :> x)` -> `sample(v <: 3.3V, i :> x)`]*

### Ranges

	min..max

**down:** It is an error if a range is in a position where it is down-evaluated.  
**up:** Down-evaluate numbers `min` and `max`. Match if the pushed value is between `min` (inclusive) and `max` (exclusive), or fail if it is outside the range.

If min is omitted (`..max`), it defaults to -Infinity.
If max is omitted (`min..`), it defaults to Infinity.

Example: `nom!min..max`
Nominal value used for down-evaluation, match values between min..max on up-evaluation.

### Mappings

	e[a1=b1, a2=b2, a3=b3]

**down:** Down-evaluate `e`. Up-evaluate `a1`, `a2`, ..., `an` with that value. If one matches, down-evaluate and return the corresponding `bx`. Fail if none match.  
**up:** Up-evaluate `b1`, `b2`, ..., `bn` with the pushed value. If one matches, down-evaluate the corresponding `bx` and up-evaluate `e` with that value. Fail if none match.

Because of the properties of `ignore`, it can be used as an "else" clause, in either direction.

Mappings work like Haskell's case expressions, but bidirectionally.

Example:
`v[#l = 0V ! (..1.6V), #h = 3.3V ! (1.6V..)]` is a bidirectional definition of LVCMOS digital logic on top of an analog signal. On down-evaluation, if `v` is `#l`, the first arm matches, and the result is 0V; if `v` is `#h`, the second arm matches and the result is 3.3V. On up-evaluation, values below 1.6V cause `v` to be up-evaluated with `#l`, and values above 1.6V cause it to be up-evaluated with `#h`. A more robust implementation may want to use a Schmitt trigger, which is not a single expression because it maintains state between samples, but can be built out of `repeat` blocks.

### Tuples

	(a, b, x=y, ..f)

Tuples can contain positional and named elements. A tuple can be extended with the named elements from another tuple with the `..f` syntax. The new tuple inherits the named fields from `f` that are not overridden in the expression.

### Concatenations

types: vector

	[l1:a1, v1, l2:a2, :a3, v2, l4:a4]

**down:** Down-evaluate each part. Components without the `:`, like `v1`, are treated as length 1, and the value is inserted directly into the vector. Return the concatenation of `a1` + [`v1`] + `a2` + ... + `an`.  
**up:** Split the data into parts using the lengths `l1`...`ln`. Up-evaluate each `a1`...`an` field with the respective part. Components without the `:` are treated as length 1, and the expression up-evaluted with the element at that position.

The width expressions <code>l<var>x</var></code> must be compile-time constant, or can be omitted, in which case the length is inferred from the type of <code>a<var>x</var></code>.