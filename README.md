Arduino Firmata for Erlang
==========================

This is a porting of the [Processing library for Firmata](http://playground.arduino.cc/Interfacing/Processing) (which, by the way, needs modifications to work with Processing 2.0, but this is another story).
The library is structured as a *gen_server*, with code for reading and writing digital and analog values to and from an Arduino running a compatible version of Firmata; the code has been tested with Arduino Yun and Firmata 2.3 (distributed with Arduino IDE 1.5.4). The example in *firmata_test.erl* simply blinks the test led (pin 13) every second, and reads values from a sensor connected to the first analog pin (A0) when it changes the led status.

The only dependency is the [serial library](https://github.com/tonyg/erlang-serial/), which I have included in the *deps* subdirectory, since I have changed the name of the serial application (otherwise it didn't find the *priv* directory).

This implementation supports only analog and digital read/write messages.
