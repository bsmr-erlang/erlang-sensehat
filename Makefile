sensehat_drv : sensehat_drv.c
	gcc -o sensehat_drv.so -fpic -shared sensehat_drv.c

sensehat: sensehat.erl
	erl -compile sensehat.erl

all: sensehat sensehat_drv

clean:
	rm -f *.so *.beam
