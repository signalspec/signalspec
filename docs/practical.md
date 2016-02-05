## Examples

### Servo

```
repeat {
  on v {
    let t = 1.0us + v*1.0us
    repeat t/sampleTime { #h }
    repeat <:(20ms - t)/sampleTime  { #l }
  }
}
```

### SPI

A minimal definition CPOL=0, CPHA=0 SPI that could be used as master, slave, or sniffer just by changing the direction of the pins:

```
def SPI {
  repeat _ {
    on _ { // SPI transaction
      with |x| (cs = #l, ..x) {
        // Everything within this block has CS low
        repeat _ {
          // It infers two conditions to exit this loop:
          // - The byte event is no longer possible to match
          //   because the inner block has ended (for master)
          // - CS goes high, as everything in this block
          //   specifies CS low due to the enclosing `with` (for slave/sniff)
          on (bi, bo) { // byte
            for mi=bi, mo=bo { // for each bit of the byte
              repeat (<:clkPer/2) {
                // If we control the timing (clk is an output),
                // make this be clkPer/2 samples.
                // While clk is low, set the mi and mo lines if they
                // are outputs, but ignore them as inputs
                (clk=#l, mi<:mi, mo<:mo)
              }
              // For the first sample of clk high, mi and mo are
              // bound bidirectionally. This is where we capture
              // the values on input.
              (clk=#h, mi=mi, mo=mo)
              repeat (<:clkPer/2 - 1) {
                // Same as above, but with clk high
                (clk=#h, mi<:mi, mo<:mo)
              }
            }
          }
        }
      }
    }

    (cs=#h, .._) // Must match at least one sample with cs=#h
    repeat <: clkPer {
      // Between transactions, CS is high, and ignore
      // all the other signals.
      (cs=#h, .._)
    }
  }
}
```

This could be simplified by building it out of a "clocked signal" abstraction.

## Implementation

### Desktop CPUs

  * compile user-defined protocols to DFAs, then JIT with LLVM
  * thread-per-protocol

### Microcontrollers

  * compile to interrupt-driven state machines

### FPGAs

IO pins could be represented as a tuple of #h, #l values, with one token per clock.

Control flow transforms to a state machine muxing expression inputs/outputs between the up and down streams.

Blocks that operate irregularly, not at a factor of the clock frequency, would use strobe/acknowlege handshaking.

Yosys / arachne-pnr / icestorm flow enables synthesis as a part of free and redistributable software.

### File IO

Files are sequences of bytes. Abstractions could transform between a stream of bytes in e.g. a WAV or VCD file to the domain data contained within.

Files are opened only from the command line in either read or write mode. Scripts cannot open files besides those named on the command line so it is safe to run untrusted scripts.

### Logic Analyzers

Tuple (vector?) of values for each pin, with a token representing a sample.

### Source-measure units

### Software defined radio

## UI
