sensehat_drv : sensehat_drv.c
	cc -o sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat sensehat_drv.c

sensehat: sensehat.erl
	erl -compile sensehat.erl

all: sensehat sensehat_drv

clean:
	rm -f *.so *.beam
