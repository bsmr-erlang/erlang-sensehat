# Erlang Sense Hat
Raspberry Pi Sense Hat Erlang port driver

The Sense Hat is a framebuffer device and there is no native support in Erlang to open this through the file api.

This project implements a port driver in C, that will allow Erlang to control the 8x8 LED display on the Sense Hat.

http://erlang.org/doc/reference_manual/ports.html

Specification for the Sense Hat

https://www.raspberrypi.org/blog/astro-pi-tech-specs/

> 8×8 RGB LED matrix with ~60fps refresh rate and 15-bit colour resolution (RGB 5 5 5): Cree CLU6AFKW/CLX6AFKB (data sheet)
Accessible via frame buffer driver /dev/fb1 where each pixel is 16 bit (RGB 5 6 5).

**WARNING**

This code is not production ready.

## Example

This code will only run on a Raspberry Pi with a Sense Hat attached (!)

### Build

make all, and the c driver and the erlang code will be compiled.

Then, start erlang:

```
erl
```

Start the port driver, this will find the framebuffer device and map the device into memory. Logo will write a pixelated Erlang logo to the device and stop will close the connection with the Sense Hat.

```
sensehat:start().
sensehat:logo().
sensehat:stop().
```
