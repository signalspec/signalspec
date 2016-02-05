## Values

Values are the elements of signals. Types are sets of values with a fixed-size memory representation.

#### Symbols - `#h`, `#l`, `#foo`
Discrete named values internally represented by integers. Symbol types specify one or more allowed symbol values.

#### Integers - `#0`, `#1`, `#2`
Integer types specify an (inclusive) range of integer values.

#### Numbers - `3.1415`, `3.3V`
Number types specify a range of values, precision, and physical units, which are checked at compile time and erased at runtime. The implementation may use floating or fixed point math depending on the target platform and operations used.

#### Vectors - `[1.0, 2.0, 3.0]`, `'b1010`, `'hAA55`
Vectors combine multiple values of one of the above types. Bytes, words, and other binary strings are represented as vectors of the integer type `#0..#1`, and special syntax exists for creating these from binary or hex literals.

#### Tuples `(#foo, 5V)`, `(I=2.1A, V=1.26V)`
Tuples combine values of heterogeneous types. The items can be named.

## Signals

Signals are sequences of values -- a series of samples, tokens, packets, etc. in order of time or position in a file.

Signals can be hierarchical, where elements of the signal contain a child signal. This is used to represent transactions, packets, or states.

 Examples of signals:

  * Bytes in a file
  * Logic analyzer samples (vector of bits)
  * Radio signals (tuple of complex I/Q components)
  * Source-measure-unit samples (tuple of (Voltage, Current))
  * SPI transactions (sequence of transactions, which are sequences of (master_out, master_in) tuples of bytes)
  * Network packets (sequence of packets, which are sequences of bytes)

```
(slave_addr=#1) {
    (mo='b12, mi='b34)
    (mo='b33, mi='b55)
}
(slave_addr=#2) {
    (mo='b99, mi='b43)
    (mo='b5A, mi='b82)
    (mo='bC5, mi='b22)
}
```

Protocols are transformations between signals. We describe protocols as part of a stack, with hardware and low-level data on the bottom, and high-level meaning on top. Each protocol communicates with the protocol above and below it in the stack.

Examples of protocols:

  * SPI transforms between states of (CS, MISO, MOSI, SCK) pins and sequences of transactions and their bytes.
  * ADXL345 transforms between SPI transactions on the Analog Devices accelerometer part and acceleration measurements.
  * WAV transforms between file bytes and the signal of samples.
  * BPSK transforms between complex baseband and a sequence of bits.

The direction data travels in a signal between two protocols is determined by the bottom protocol, and ultimately the underlying hardware and its configuration.

For example:
  * A logic analyzer sends wire states upward to the SPI protocol stacked on top, which decodes them to produce transactions containing pairs of bytes.
  * A SPI master accepts transactions and MO bytes from the protocol above it, and produces CS, MOSI, and SCK wire states, It accepts the corresponding MISO wire states from below and sends the MI bytes to the protocol above it.
  * The protocol for an ADXL345 accelerometer, when stacked on top of a SPI master, produces register read commands and translates register values to acceleration measurements. Or, when stacked on top of SPI atop a logic analyzer watching an external microcontroller communicate with the chip, it snoops register reads to decode the acceleration measurements read by the microcontroller.
  * A VCD protocol transforms from bytes to values when reading a VCD file, and from values to bytes when writing a VCD file.
  * BPSK stacked on top of a SDR modulates when the SDR is set to transmit, and demodulates when the SDR is set to receive.

## Other items

The following items can be named and stored in variables and used some expressions, but are not values (that is, they cannot be part of a signal).

#### Protocol definitions

#### Types

#### Functions
Pure functions evaluated or inlined at compile time. Composed of bidirectional functions, they are bidirectional.

#### Strings
Are not values because they are of variable length. The only current application is filenames. Strings in signals should be represented by a sequence of bytes or characters.
