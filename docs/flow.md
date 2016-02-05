# Blocks: Control and data flow

Blocks are connected via two streams of tokens.
Blocks have a "bottom" token stream connecting to the abstraction below (lower-level, closer to hardware), and optionally provide a "top" token stream connecting to the abstraction above. The block converts between lower-level tokens and higher-level tokens. The types of these streams are inferred


### <code><var>token</var> [<b>{</b> <var>body</var> <b>}</b>]</code>

A bare expression used as a statement transacts a token on the bottom token stream. The expression is down-evaluated and sent to the block below on the abstraction stack. That block replies with a value that is up-evaluated on the expression.

If the action on the upper block involves a sub-stream, a body may be provided, which will be matched against the sub-stream.

### `seq`

A series of statements separated by newlines or semicolons are matched in order.

The program `#a ; #b ; #c`, matches exactly the series of symbol tokens `#a`, `#b`, `#c`, and generates the same series of tokens.

### `with`

<code><b>with</b> <var>fn</var> <b>{</b> <var>body</var> <b>}</b></code>

The `with` block maps tokens bidirectionally through a function. Tokens received on the top token stream are down-evaluated through the provided function and passed to the bottom token stream. Tokens received on the bottom stream are up-evaluated through the function and passed to the top token stream.

Its main purpose is partial application / currying of streams. This can be optimized into run-length encoding for sampled signals or loop invariant code motion for signals backed by hardware registers.

### `repeat`

<code><b>repeat</b> <var>n</var> <b>{ </b> <var>(first) ; [rest]</var> <b>};</b> <var>(follow...)</var></code>

As a heuristic for determining the data direction of the count expression: if the body of the loop
contains no tokens where data is used in the up direction, the count is down-evaluated and the loop
is executed that number of times. Otherwise, it counts number of times the loop matches, and
up-evaulates the count with that number.

In sampled signals, this is used to measure and set the time duration of signal features.

### `for`

<code><b>for</b> <var>e1</var><b>=</b><var>v1</var>, <var>e2</var><b>=</b><var>v2</var> <var>...</var> { <var>body</var> }</code>

`for` breaks a vector into a series of actions.

`v1`, `v2`, ... `vn` must be vectors with the same length `x`. The body is executed `x` times (`i` from `0` to `x`), with `e1`, `e2` ... `en` bidirectionally bound to the `i`th element of the corresponding vector.


### `switch` / `when`

<code><b>switch</b> <var>expr</var> <b> { when</b> <var>expr1</var> <b> { </b><var>body1</var><b> } when</b> <var>expr2</var> <b> { </b><var>body2</var><b> }</b> <var>...</var> <b>}</b>

Just like `repeat`, as a heuristic for determining the data direction of the expression: if the body of all cases contain no tokens where data is used in the up direction, the count is down-evaluated and the appropriate branch is chosen. Otherwise, the expression is up-evaluated with the value corresponding to the branch that matches.

### `on`

<code><b>on</b> <var>expr</var> <b>{ }</b></code>

`on` blocks define the tokens exposed to the abstraction stacked on top of this one.

A token is received from the top stream and destructured into `expr`, binding down-evaluable variables in scope in the body. The body may up-evaluate into variables created in expr. On exit from the body, the up-evaluated components of `expr` are sent as a token on the top stream.

### `par`

Run multiple blocks in parallel.

TBD

---

### References

 - [Anne Brüggemann-Klein, Derick Wood. **One-Unambiguous Regular Languages** (1997)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.3277)
 - [C. A. R. Hoare. **Communicating Sequential Processes**](http://www.usingcsp.com/cspbook.pdf)
 - [David Harel. **Statecharts: A Visual Formalism for Complex Systems**](http://www.wisdom.weizmann.ac.il/~harel/SCANNED.PAPERS/Statecharts.pdf)
 - [Scott Owens, John Reppy, Aaron Turon. **Regular-expression Derivatives Reexamined**](http://www.mpi-sws.org/~turon/re-deriv.pdf)
