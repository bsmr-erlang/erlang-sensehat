sensehat_drv : sensehat_drv.c
	cc -o sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat sensehat_drv.c

sensehat: sensehat.erl shfb.erl
	erlc *.erl

all: sensehat sensehat_drv

clean:
	rm -f *.so *.beam
