# Signalspec

**Signalspec** is a domain specific language for defining composable I/O abstractions. These definitions can be used in both directions: parsing higher-level meaning out of low-level events, as well as generating a sequence of low-level actions from higher-level ones. It can do both simultaneously to establish bidirectional communication or form a control loop. Definitions can be stacked for layering protocols on top of other protocols or hardware.

Applications include analysis and generation of digital, analog, and RF signals: debugging serial protocols, capturing data from sensors, controlling actuators, and software defined radio.

_It's a work in progress and doesn't do anything useful yet._

Signalspec is written in [Rust](https://rust-lang.org) and is open source software under the [Mozilla Public License, version 2.0](LICENSE).

## Documentation

 * [Bidirectional Expressions](docs/expressions.md) are invertible functions used for data manipulation between layers of abstraction. To be invertible, expressions are defined to both compute a value based on evaluating sub-expressions ("down evaluation"), but also accept a value on the "result" side and push values into its subexpressions ("up evaluation").

 * [Control and data flow](docs/flow.md) semantics are related to textual regular expressions, but Signalspec can both generate and parse strings ("signals") from the language ("protocol"), and operates over structured tokens of arbitrary data rather than characters. These blocks are stacked to parse layered protocols.

 * [Practical Considerations](docs/practical.md) -- how these primitives apply to real-world protocols, and how they map to hardware resources.  Data is statically typed, there are no dynamically sized types, and recursion is prohibited, so all memory is allocated statically at compile time. Control flow is readily represented as a finite state machine on FPGAs and microcontrollers.
