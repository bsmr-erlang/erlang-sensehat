sensehat_drv : sensehat_drv.c
	cc -o ebin/sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat sensehat_drv.c

sensestick_drv: sensestick_drv.c
	cc -o ebin/sensestick_drv.so -fpic -shared -Wall -Wextra -Wformat sensestick_drv.c	

erl:
	erl -make

all: erl sensehat_drv sensestick_drv

clean:
	rm -f ebin/*.so ebin/*.beam
