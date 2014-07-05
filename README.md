# Signalspec

**Signalspec** is a domain specific language for defining composable I/O abstractions. These definitions can be used in both directions: parsing higher-level meaning out of low-level events, as well as generating a sequence of low-level actions from higher-level ones. It can do both simultaneously to establish bidirectional communication or form a control loop. Definitions can be stacked for layering protocols on top of other protocols or hardware.

Applications include analysis and generation of digital, analog, and RF signals: debugging serial protocols, capturing data from sensors, controlling actuators, and software defined radio.

_It's a work in progress and doesn't do anything useful yet._

Signalspec is written in [Rust](https://rust-lang.org).