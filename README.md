# Signalspec

*a language for composable I/O abstractions*

Signalspec makes it easier to encode and decode the signals that embedded systems speak. With Signalspec, you define and use declarative, bidirectional transformations between signals, and mix-and-match them across protocol stacks and hardware platforms.

Applications include analysis and generation of digital, analog, and radio signals: prototyping and debugging embedded systems, information security research, capturing data from sensors, controlling actuators, and exploring the physical world with code.

 Signalspec is not intended to replace C, Verilog, Python, or Rust. Like regular expressions or SQL, it provides more streamlined abstractions in a limited problem domain, and is designed to interoperate with these and other languages in a complete application.
 
 With that limited domain, Signalspec can trade off general-purpose language features to gain useful properties:

   * Limited influence of state on conditionals and loops - control flow is readily represented as a finite state machine on FPGAs and microcontrollers.
   * No dynamically sized types or recursion - all memory is allocated statically at compile time.
   * Expressions are invertable - able to capture the symmetry between transmitter and receiver.

Signalspec is written in [Rust](https://rust-lang.org) and is open source software under the [Mozilla Public License, version 2.0](LICENSE).

_It's a work in progress and isn't very useful yet. This document refers to many unimplmented features in present tense. If a section sounds hand-wavy, it's probably because it's not fully figured out yet. It'll probably be easier to explain once more complete examples can be used to demonstrate._

# Conceptual overview

## Signals

Signalspec is about manipulating signals. For our purposes, signals are any discrete sequence of values, samples, etc. encoding data and ordered by time. We'll call each unit of the signal a "token".

<img src=docs/signal.png width=273 />

Examples of signals:

  * Bytes in a file
  * Physical quantities sampled by a sensor / ADC
  * Logic analyzer samples (vector of bits)
  * FPGA IOs (state and direction for each pin at each clock)
  * SPI transactions (transaction delimiters + master_out and master_in bytes)
  * Radio signals as transmitted or received by a SDR (quadrature samples)
  * Source-measure-unit samples (Voltage, Current)
  * Positions of a robot arm

A signal can involve input, output, or bidirectional data flow. Each token contains one or more fields which carry a data value in a particular direction. The direction of each field of a signal is inferred, depending on the underlying hardware and its configuration.

For example, when implementing a SPI master, each data token contains fields for the byte sent to the device (output) and the returned byte (input), while when sniffing SPI with a logic analyzer, both of these fields are inputs.

## Processes

A process transforms between two signals. Since processes encode and decode layered protocols, we refer to these signal connections by their direction in the abstraction stack: a process's "bottom" signal connects to a "lower-level" process towards the hardware, while a process's "top" signal carries more abstracted data to a "higher-level" process.

<img src=docs/process.png width=407 />

Some processes use only one of these, and produce or consume a single signal. For instance, hardware and IO processes at the bottom of the protocol stack have a null bottom signal, internally interact with hardware or the OS as a side effect, and communicate on the top signal, where more processes can be stacked. Similarly, a function generator or plot display at the top of the stack would use only its bottom signal, and have a null top signal.

Examples of processes:

  * `val(X)` transacts a single token `X` on its bottom signal
  * `spi(mode=2, speed=25MHz)` transforms between a bottom signal with pin states of (CS, MISO, MOSI, SCK) pins and a top signal with sequences of transactions and their bytes.
  * `adxl345()` transforms between SPI transactions on the Analog Devices accelerometer part and acceleration measurements.
  * `wav(datatype=#int16)` transforms between file bytes and audio samples.
  * `gfsk()` transforms between complex baseband I-Q samples and a sequence of bits.

There are three ways to define a process:

  * Stacking together processes to run in parallel with the `|` operator, where the top signal of one process is the bottom signal of another.
  * Sequencing processes with state machines to run different processes over time, and concatenate their signals.
  * Primitives written in Rust, Verilog or another language. There can exist multiple such definitions targeting different platforms.

A process decides when to complete. This may be when its bottom or top signals end, after a fixed number of samples, or based on received data. When a process completes, its parent state machine process may hand control of its signals to another process, or complete itself.

 When a process attempts to transact a token on the bottom signal which is not accepted by the process below, it fails to match. The parent process can try an alternative, or propagate the failure, eventually signalling an error.

## Stacking processes

Since protocols are commonly layered, one natural form of composition enabled by Signalspec is stacking processes atop each other to build processes that traverse multiple layers of abstraction.

<img src=docs/stack.png width=667 />

Examples of stacking processes:

`io_pins(sck=#out, cs=#out, mosi=#out, miso=#in) | spi(...) | adxl345()` - A SPI master is implemented on a FPGA to communicate with an accelerometer to produce acceleration samples.

`sigrok_la(...) | spi(...) | nrf24(...)` - A logic analyzer samples a SPI bus, and decodes SPI and chip-specific register IO.

`usrp(#rx, center=2.41GHz, rate=5MHz) | gfsk(...) | logitech_keyboard()` - A SDR is configured to sample the 2.4GHz band. The samples are GFSK-demodulated, and packets from a keyboard are decoded.

`linux_spi("/dev/spidev0") | nrf24(...) | logitech_keyboard()` - The SPI bus on a Linux SoC is used to communicate with a fixed-function radio IC, used to transmit packets to spoof the same keyboard.

`file("dial.wav", #write) | wav(...) | dtmf() | seq(5, 5, 5, 1, 2, 3, 4)` - A .wav file is written containing DTMF tones

Despite the syntax being inspired by `bash` pipes, the data flow is not necessarily left to right. The data direction is ultimately defined by the hardware or IO on the bottom of the stack (left side of the chain). The direction in successive processes is inferred from there as the signals become higher-level.

Because the data direction in processes is flexible and the signals are not linked to any particular hardware implementation, the same process is re-usable in different contexts -- in the examples above, `spi` is used both in an FPGA to implement a SPI master in terms of logic states of IO pins, and on a PC to decode SPI data from the samples captured by a logic analyzer. The `nrf24` process is used decode communications with the radio IC sniffed with a logic analyzer, and also used atop a spidev device to control the radio.

## Protocols

Signals implement protocols to define the data types of their tokens and determine which processes can be stacked on top. Protocols fill a role like Rust's traits.

The simplest and most common protocol is `Seq(T)`. It has a single token variant `val`, carrying one element of type `T`.

An example of a protocol with multiple variants is `I2C`, with token variants `start(addr: byte)`, `data(dir: #r|#w, d: byte)`, `stop()`. 

These token variants are exposed as processes that transact the single token. Compound processes are defined for signals implementing a particular protocol, and process calls are resolved based on the protocol of the signal below them. The process definition determines the protocol of its top signal.

Protocols can be parameterized to carry contextual metadata about the signal. For example,`Sampled(T, rate)` extends `Seq(T)` but carries a sample rate, allowing processes like `constant(value, time)` or `sine(amplitude, frequency)` to be defined with a way to relate samples to time.

Protocol parameters can also represent requrements about the configuration of the underlying process stack. The `SPI` protocol has parameters representing the clock speed and mode of the SPI device. Processes defined atop `SPI` can require particular values for these parameters.

## Expressions: Bidirectional Evaluation

Signalspec's value expressions are invertible functions used for data manipulation between layers of abstraction. To be invertible, expressions are defined to both compute a value based on evaluating sub-expressions ("down evaluation"), but also accept a value on the "result" side and push values into its subexpressions ("up evaluation").

Evaluation of expressions can occur in two directions: **Up** and **down** the abstraction stack. *[Maybe to be called push and pull evaluation?]*

**Down-evaluation** works like evaluation in traditional programming languages: return value is a function of one or more subexpressions. Down-evaluation is functionally pure.

**Up-evaluation** can be thought of as pushing a value into the expression from the "return" side, which then pushes values into its subexpressions. The evaluation either "matches" or "fails". It has semantics similar to the pattern in a Rust `match` arm, destructuring a value and testing or binding its compoenents.

### Literals

number literals: `42.1`  
unit literals: `3.3V`
integer literals: `42`
bit literals: `'b1010`, `'xAA55` - Produces a vector of integer 1 and 0.  
symbol literals: `#abc` - Symbols are discrete named values internally represented by integers.

**down:** Evaluates to the literal value.  
**up:** Match if the pushed value is equal, or fail if the pushed value is not equal.

### Arithmetic expressions

```
a+b
a*b
a-b
a/b
```

**down:** Down-evaluate both arguments and perform operation.  
**up:** If one argument is constant, perform the inverse operation with the pushed value and the constant and up-evaluate the non-constant argument with the result.

### Ignore

```
_
```
(underscore `_` or `ignore`)

**up:** Pushed value is discarded, match always succeeds.

### Ranges

```
min..max
```

**down:** It is an error if a range is in a position where it is down-evaluated (must appear on the right-hand side of `!`).  
**up:** Down-evaluate numbers `min` and `max`. Match if the pushed value is between `min` (inclusive) and `max` (exclusive), or fail if it is outside the range.

If min is omitted (`..max`), it defaults to -Infinity.
If max is omitted (`min..`), it defaults to Infinity.


### Switch

`x!y`  

**down:** Down-evaluation of `x`.  
**up:** Up-evaluate `y` with the pushed value.

`<: x` - Syntactic sugar for `x ! _`  
`:> y` - Syntactic sugar for `_ ! y`  

This is used to output a specific value when used as an output, but accept different values on input.

Common pattern: `nominal ! min..max` e.g. `3.3V ! 3.0V..3.6V`
Nominal value used for down-evaluation, but match values between min..max on up-evaluation.

### Mappings

```
e[a1=b1, a2=b2, a3=b3]
```

**down:** Down-evaluate `e`. Up-evaluate `a1`, `a2`, ..., `an` with that value. If one matches, down-evaluate and return the corresponding `bx`. Fail if none match.  
**up:** Up-evaluate `b1`, `b2`, ..., `bn` with the pushed value. If one matches, down-evaluate the corresponding `bx` and up-evaluate `e` with that value. Fail if none match.

Because of the properties of `_`, it can be used as an "else" clause, in either direction.

Mappings work like Haskell's `case` or Rust's `match` expressions, but bidirectionally.

Example:
`v[#l = 0V ! (..1.6V), #h = 3.3V ! (1.6V..)]` is a bidirectional definition of LVCMOS digital logic on top of an analog signal. On down-evaluation, if `v` is `#l`, the first arm matches, and the result is 0V; if `v` is `#h`, the second arm matches and the result is 3.3V. On up-evaluation, values below 1.6V cause `v` to be up-evaluated with `#l`, and values above 1.6V cause it to be up-evaluated with `#h`. A more robust implementation may want to use a Schmitt trigger, which is not a single expression because it maintains state between samples.

### Vector Concatenation

```
[*a1, v1, *a2, *a3, v2, *a4]
```

Vectors combine multiple values of the same type.

**down:** Down-evaluate each part. Components without the `*`, like `v1`, are treated as length 1, and the value is inserted directly into the vector. Return the concatenation of `a1` + [`v1`] + `a2` + ... + `an`.  
**up:** Split the data into parts using the lengths of the component parts. Up-evaluate each `a1`...`an` field with the respective part. Components without the `*` are treated as length 1, and the expression up-evaluated with the element at that position.

### Tuples

```
(a, b, .x=y, *f)
```

Tuples package together values of heterogeneous types. Tuples can contain positional and named elements. A tuple can be extended with the named elements from another tuple with the `*f` syntax. The new tuple inherits the named fields from `f`.

## Control flow

Signalspec's state machine control flow semantics are related to textual regular expressions, but Signalspec can both generate and parse strings ("signals") from the language ("protocol"), and operates over structured tokens of arbitrary data rather than characters. Other inspirations are [Communicating Sequential Processes](http://www.usingcsp.com/cspbook.pdf), and [Harel Statecharts](http://www.wisdom.weizmann.ac.il/~harel/SCANNED.PAPERS/Statecharts.pdf).

The amount of ambiguity is to be determined. Fully nondeterministic bidirectional finite state transducers cannot be determinized for implementation without backtracking, but [1-unambiguous](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.3277) grammars may be too limiting.

### Sequences

A series of statements separated by newlines or semicolons are matched in order.

The process `val(#a) ; val(#b) ; val(#c)`, matches exactly the series of symbol tokens `#a`, `#b`, `#c`, and generates the same series of tokens.

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


### `alt`

<code><b>alt</b> <var>expr</var> <b> { </b> <var>expr1</var> <b>=> { </b><var>body1</var><b> } </b> <var>expr2</var> <b>=> { </b><var>body2</var><b> }</b> <var>...</var> <b>}</b></code>

Just like `repeat`, as a heuristic for determining the data direction of the expression: if the body of all cases contain no tokens where data is used in the up direction, the count is down-evaluated and the appropriate branch is chosen. Otherwise, the expression is up-evaluated with the value corresponding to the branch that matches.

### `on`

<code><b>on</b> <var>token(expr)</var> <b>{ }</b></code>

`on` blocks define the tokens exposed to the abstraction stacked on top of this one.

A token is received from the top signal and destructured into `expr`, binding down-evaluable variables in scope in the body. The body may up-evaluate into variables created in `expr`. On exit from the body, the up-evaluated components of `expr` are sent as a token on the top stream.

## Future direction

  * Get the basics working and semantics better defined.
  * Compile for desktop and embedded via [Cranelift](https://github.com/CraneStation/cranelift).
  * Synthesis for FPGA via [Yosys](https://github.com/YosysHQ/yosys) / [SymbiFlow](https://symbiflow.github.io/).
  * Hardware interfaces via [SoapySDR](https://github.com/pothosware/SoapySDR) and [Sigrok](https://sigrok.org/).
  * Jupyter-style notebook UI for interactive exploration.
  