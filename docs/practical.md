## Examples

### SPI

```
def SPI {
  repeat _ {
    on _ {
      with |x| (cs = #l, ..x) {
        repeat _ {
          on (bi, bo) {
            for mi=bi, mo=bo {
              repeat (<:clkPer/2) {
                (clk=#l, mi<mi, mo<mo)
              }
              (cs=#l, clk=#h, mi=mi, mo=mo)
              repeat (<:clkPer/2 - 1) {
                (clk=#h, mi<mi, mo<mo)
              }
            }
          }
        }
      }
    }
    repeat {
      (cs=#h, .._)
    }
  }
}
```

## Implementation

### Desktop CPUs

  * threads
  * SSE

### Microcontrollers

  * coroutines
  * Fractal bindings exposing peripherals as standardized interfaces

### FPGAs

IO pins could be represented as a tuple of #h, #l values, with one token per clock.

Control flow transforms to a state machine via Brzozowski derivatives or NFA / DFA construction (much simplified because control flow is 1-unambiguous). Expressions would be compiled to HDL, and the state machine would mux expression inputs/outputs between the up and down streams.

Blocks that operate irregularly, not at a factor of the clock frequency, would use strobe/acknowlege handshaking.

### File IO

Files are sequences of bytes. Abstractions could transform between a stream of bytes in e.g. a WAV or VCD file to the domain data contained within.

Files are opened only from the command line in either read or write mode. Scripts cannot open files besides those named on the command line so it is safe to run untrusted scripts.

### Logic Analyzers

Tuple (vector?) of values for each pin, with a token representing a sample.

### Source-measure units

### Software defined radio

## UI

