# Erlang Sense Hat
Raspberry Pi Sense Hat Erlang port driver

![Project image](https://farm2.staticflickr.com/1694/25082502244_e069cb5089_n.jpg)

The Sense Hat is a framebuffer device and there is no native support in Erlang to open this through the file api.

If you try to open the device from erlang, this is the result:

```
Erlang/OTP 17 [erts-6.2] [source] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)
1> file:open('/dev/fb0', []).
{error,eisdir}
```

This project implements a port driver in C, that will allow Erlang to control the 8x8 LED display on the Sense Hat.

http://erlang.org/doc/reference_manual/ports.html

Specification for the Sense Hat

https://www.raspberrypi.org/blog/astro-pi-tech-specs/

> 8×8 RGB LED matrix with ~60fps refresh rate and 15-bit colour resolution (RGB 5 5 5): Cree CLU6AFKW/CLX6AFKB (data sheet)
Accessible via frame buffer driver /dev/fb1 where each pixel is 16 bit (RGB 5 6 5).

**WARNING**

This code is not production ready. That is, ready for *Astro Pi* on the International Space Station [ISS](https://astro-pi.org/).  

## Example

This code will only run on a Raspberry Pi with a Sense Hat attached (!)

### Install Erlang OTP 18

Follow the instructions here http://elinux.org/Erlang

### Build

Compile the driver and erlang code

```
make all
```

Then, run erlang:

```
erl
```

Start the port driver, this will find the framebuffer device and map the device into memory. Logo will write a pixelated Erlang logo to the device and stop will close the connection with the Sense Hat.

```
Eshell V6.2  (abort with ^G)
1> sensehat:start().      
<0.35.0>
2> shexample:logo().
ok
3> sensehat:set_rotation(180).
ok
4> sensehat:stop().
sensehat_drv: stop
stop

```

## TODO

* Design font for writing text to the display
* Joystick integration

## Reference

* [Port driver example (erl and c)](http://erlang.org/doc/tutorial/c_portdriver.html)
* [open_port/2 (erl)](http://erlang.org/doc/man/erlang.html#open_port-2)
* [Port driver (erl)](http://erlang.org/doc/reference_manual/ports.html)
* [erl_driver C API](http://erlang.org/doc/man/erl_driver.html)
