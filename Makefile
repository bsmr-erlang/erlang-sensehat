sensehat_drv : sensehat_drv.c
	cc -o sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat sensehat_drv.c

sensestick_drv: sensestick_drv.c
	cc -o sensestick_drv.so -fpic -shared -Wall -Wextra -Wformat sensestick_drv.c	

sensehat: sensehat.erl shfb.erl
	erlc *.erl

all: sensehat sensehat_drv sensestick_drv

clean:
	rm -f *.so *.beam
