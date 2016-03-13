/* sensestick_drv.c */
/* Raspberry Pi Sense Hat Joystick Erlang Integration */
/* morten.teinum@gmail.com */

#define _GNU_SOURCE
#define DEV_INPUT_EVENT "/dev/input"
#define EVENT_DEV_NAME "event"

#include <erl_driver.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/input.h>
#include <linux/fb.h>
#include <unistd.h>

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen); 
static void ready_input(ErlDrvData drv_data, ErlDrvEvent event);

static ErlDrvEntry sensestick_driver_entry = {
    NULL,                   /* F_PTR init, called when driver is loaded */
    start,     /* L_PTR start, called when port is opened */
    stop,      /* F_PTR stop, called when port is closed */
    NULL,        /* F_PTR output, called when erlang has sent */
    ready_input,                   /* F_PTR ready_input, called when input descriptor ready */
    NULL,                   /* F_PTR ready_output, called when output descriptor ready */
    "sensestick_drv",         /* char *driver_name, the argument to open_port */
    NULL,                   /* F_PTR finish, called when unloaded */
    NULL,                   /* void *handle, Reserved by VM */
    control,                   /* F_PTR control, port_command callback */
    NULL,                   /* F_PTR timeout, reserved */
    NULL,                   /* F_PTR outputv, reserved */
    NULL,                   /* F_PTR ready_async, only for async drivers */
    NULL,                   /* F_PTR flush, called when port is about  to be closed, but there is data in driver queue */
    NULL,                   /* F_PTR call, much like control, sync call to driver */
    NULL,                   /* F_PTR event, called when an event selected by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be  set to this value */
    0,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a  monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an  event object */
};

/**/

static int is_event_device(const struct dirent *dir)
{
	return strncmp(EVENT_DEV_NAME, dir->d_name,
		       strlen(EVENT_DEV_NAME)-1) == 0;
}

// from snake.c
static int open_evdev()
{
	struct dirent **namelist;
	int i, ndev;
	int fd = -1;

	ndev = scandir(DEV_INPUT_EVENT, &namelist, is_event_device, versionsort);
	if (ndev <= 0)
		return ndev;

	for (i = 0; i < ndev; i++)
	{
		char fname[64];
		char name[256];

		snprintf(fname, sizeof(fname),
			 "%s/%s", DEV_INPUT_EVENT, namelist[i]->d_name);

		fd = open(fname, O_RDONLY);

		printf("fname: %s %i\r\n", fname, fd);

		if (fd < 0)
			continue;

		ioctl(fd, EVIOCGNAME(sizeof(name)), name);

		if (strcmp("Raspberry Pi Sense HAT Joystick", name) == 0)
			break;

		close(fd);
	}

	for (i = 0; i < ndev; i++)
		free(namelist[i]);

	return fd;
}

/**/

typedef struct sensestick_data {
    ErlDrvPort port;
    int fd;
    /* prev state so we can pick up changes to the joystick */
    __u16 code;
    __u32 eid;
} sensestick_data_t;

DRIVER_INIT(pq_drv)
{
    return &sensestick_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, __attribute__((unused)) char *command) {
    sensestick_data_t* data = driver_alloc(sizeof(sensestick_data_t));
    data->port = port;
    data->fd = 0;
    data->code = 0;
    data->eid=0;
    return (ErlDrvData)data;}

static void stop(ErlDrvData drv_data) {
	printf("drv:stop\r\n");

	sensestick_data_t* d = (sensestick_data_t *) drv_data;

	if (d->fd){
		driver_select(d->port, (ErlDrvEvent)d->fd, DO_READ, 0);
		close(d->fd); /* ? */
	}

	driver_free(d);
}

/* The return value is the number of bytes returned in *rbuf. */

static int control(
	ErlDrvData drv_data,
	unsigned int command,
    __attribute__((unused)) char *buf,
    __attribute__((unused)) ErlDrvSizeT len,
	__attribute__((unused)) char **rbuf,
	__attribute__((unused)) ErlDrvSizeT rlen) {

	printf("drv:control command=%i\r\n", command);

	sensestick_data_t* d = (sensestick_data_t *) drv_data;
	int ret;

	if (command == 1) {
		// open device
		d->fd = open_evdev();

		if (!d->fd) {
			printf("drv:control failed to open_edev\r\n");
			// bummer; report error and close this port driver
			driver_failure_atom(d->port, "sensestick_no_dev");
		}
		else {
			ret = driver_select(d->port, (ErlDrvEvent)d->fd, ERL_DRV_READ, 1);

			printf("drv:control event fd=%i select=%i\r\n", d->fd, ret);
		}
	}
	else {
		return -1;
	}

	return 0;
}

static void change_state(sensestick_data_t* d, int code) {
	if (d->code == code)
		return;

	// send driver output (as atom? up down, left right, enter)
	switch (code) {
		case KEY_ENTER:
			driver_output(d->port, "enter", 5);
			break;
		case KEY_UP:
			driver_output(d->port, "up", 2);
			break;
		case KEY_RIGHT:
			driver_output(d->port, "right", 5);
			break;
		case KEY_DOWN:
			driver_output(d->port, "down", 4);
			break;
		case KEY_LEFT:
			driver_output(d->port, "left", 4);
			break;
	}

	d->code = code;
}

static void ready_input(ErlDrvData drv_data, __attribute__((unused)) ErlDrvEvent event) {
	
	// printf("drv:ready_input\r\n");

	/* data on the joystick input! */
	sensestick_data_t* d = (sensestick_data_t *) drv_data;

	struct input_event ev[64];
	unsigned int i, urd;
	int r;

	r = read(d->fd, ev, sizeof(struct input_event) * 64);

	// printf("drv:input read=%i struct=%i\r\n", r, sizeof(struct input_event));

	if (r <= 0) {
		fprintf(stderr, "expected %d bytes, got %d\r\n", (int) sizeof(struct input_event), r);
		return;
	}

	urd = (unsigned int)r / sizeof(struct input_event);


	for (i = 0; i < urd; i++) {
		
		if (ev[i].type != EV_KEY) /* 1 */
			continue;

		if (ev[i].value != 1)
			continue;

		// d->eid++;
		// printf("drv:ready_input %08x ev[%i].type=%i value=%i code=%i\r\n", d->eid, i, ev[i].type, ev[i].value, ev[i].code);

		change_state(d, ev->code);
	}

}


