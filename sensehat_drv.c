//  Erlang portdriver for Raspberry Pi Sense Hat
//  morten.teinum@gmail.com

#define _GNU_SOURCE
#define DEV_FB "/dev"
#define FB_DEV_NAME "fb"

#define SENSEHAT_DEBUG

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <linux/fb.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <time.h>
#include <poll.h>
#include <dirent.h>
#include <string.h>

#include <linux/input.h>
#include <linux/fb.h>

// https://github.com/erlang/otp/blob/7cb403e4aa044fd2cc7702dbe8e2d0eea68e81f3/erts/emulator/beam/erl_driver.h
#include "erl_driver.h"


#define SENSE_HAT_FB_FBIOGET_GAMMA 61696
#define SENSE_HAT_FB_FBIOSET_GAMMA 61697
#define SENSE_HAT_FB_FBIORESET_GAMMA 61698

#define SENSE_HAT_OP_SET_PIXEL 1
#define SENSE_HAT_OP_FILL 2
#define SENSE_HAT_OP_FILL_FB 3
#define SENSE_HAT_OP_GET_GAMMA 4
#define SENSE_HAT_OP_SET_GAMMA 5

// framebuffer
struct fb_t {
    uint16_t pixel[8][8];
};

// struct fb_t *fb;

typedef struct {
    ErlDrvPort port;
    int        fbfd;
    struct fb_t *fb;
} sensehat_data;


// start with fb

static int is_framebuffer_device(const struct dirent *dir)
{
    return strncmp(FB_DEV_NAME, dir->d_name, strlen(FB_DEV_NAME)-1) == 0;
}

static int open_fbdev()
{
    struct dirent **namelist;
    int i, ndev;
    int fd = -1;
    struct fb_fix_screeninfo fix_info;

    ndev = scandir(DEV_FB, &namelist, is_framebuffer_device, versionsort);

    if (ndev <= 0)
        return ndev;

    for (i = 0; i < ndev; i++)
    {
        char fname[64];
        char name[256];

        snprintf(fname, sizeof(fname),
             "%s/%s", DEV_FB, namelist[i]->d_name);
        fd = open(fname, O_RDWR);

        if (fd < 0)
            continue;

        ioctl(fd, FBIOGET_FSCREENINFO, &fix_info);
        
        if (strcmp("RPi-Sense FB", fix_info.id) == 0)
            break;
        
        close(fd);
        fd = -1;
    }

    for (i = 0; i < ndev; i++)
        free(namelist[i]);

    return fd;
}

static ErlDrvData sensehat_drv_start(ErlDrvPort port, char *buff)
{
//    printf ("sensehat_drv: start buff=%s\n", buff);
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 

    sensehat_data* d = (sensehat_data*)driver_alloc(sizeof(sensehat_data));
    d->port = port;
    d->fb = 0;
    d->fbfd = open_fbdev();

    if (d->fbfd == 0){
        driver_failure_atom(d->port, "open_fbdev");
        goto error;
    }

    d->fb = mmap(0, 128, PROT_READ | PROT_WRITE, MAP_SHARED, d->fbfd, 0);    

    if (d->fb == 0){
        driver_failure_atom(d->port, "error_mmap");
        goto error;
    }

    memset(d->fb, 0, 128);

    return (ErlDrvData)d;

    error:
        return (ErlDrvData)-1;
}

static void sensehat_drv_stop(ErlDrvData handle)
{
    sensehat_data* d = (sensehat_data*)handle;

#ifdef SENSEHAT_DEBUG
    printf("sensehat_drv: stop\r\n");
#endif

    if (d->fb){
        memset(d->fb, 0, 128);
        munmap(d->fb, 128);
    }

    if (d->fbfd) {
        close(d->fbfd);
    }

    driver_free((char*)handle);
}

static uint16_t rgb_to_bits16(int r, int g, int b) {
    r = (r >> 3) & 0x1F;
    g = (g >> 2) & 0x3F;
    b = (b >> 3) & 0x1F;

    return (r << 11) + (g << 5) + b;
}

static int sensehat_drv_set_pixel(sensehat_data* d, int x, int y, int r, int g, int b){
    d->fb->pixel[x][y] = rgb_to_bits16(r, g, b);
    return 0;
}

static int sensehat_drv_fill(sensehat_data* d, int r, int g, int b) {
    memset(d->fb, rgb_to_bits16(r, g, b), 128);
    return 0;
}

static int sensehat_drv_fill_fb(sensehat_data* d, int len, char* buff) {

    // assert: len % 3 == 0

    uint16_t rgb;
    int i, x, y, r, g, b;
    int pairs = len / 3;

    // assert: pairs >= 0 && pairs <= 8*8
#ifdef SENSEHAT_DEBUG
    printf("drv: fill_fb len=%i pairs=%i rem=%i\r\n", len, pairs, len % 3);
#endif

    for (i=0; i < pairs; i++, buff += 3) {
        x = i / 8;
        y = i % 8;

#ifdef SENSEHAT_DEBUG
        printf("x=%i y=%i r=%02x, g=%02x b=%02x\r\n", x, y, buff[0], buff[1], buff[2]);
#endif

        d->fb->pixel[x][y] = rgb_to_bits16(
            buff[0],
            buff[1],
            buff[2]);
    }

    return 0;
}

static void sensehat_drv_get_gamma(sensehat_data* d) {
    char gamma[32];
    memset(&gamma, 0, sizeof(gamma));

    ioctl(d->fbfd, SENSE_HAT_FB_FBIOGET_GAMMA, &gamma);

    driver_output(d->port, gamma, sizeof(gamma));

}

static void sensehat_drv_set_gamma(sensehat_data* d, char* buff) {
    ioctl(d->fbfd, SENSE_HAT_FB_FBIOSET_GAMMA, buff);    
}

static void sensehat_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen)
{
    sensehat_data* d = (sensehat_data*)handle;
    char fn = buff[0];

#ifdef SENSEHAT_DEBUG
    int i;

    printf("sensehat_drv: output bufflen=%i\r\n", bufflen);
    
    for (i=0; i < bufflen; i++) {
        printf("%02x ", buff[i]);
    }

    printf("\r\n");
#endif

    
    if (fn == SENSE_HAT_OP_SET_PIXEL && bufflen == 6) {
      sensehat_drv_set_pixel(
        d,
        buff[1], // x
        buff[2], // y
        buff[3], // r
        buff[4], // g
        buff[5]); // b
    }
    else if (fn == SENSE_HAT_OP_FILL && bufflen == 4) {
        sensehat_drv_fill(
            d,
            buff[1],
            buff[2],
            buff[3]);
    }
    else if (fn == SENSE_HAT_OP_FILL_FB && bufflen > 1) {
        sensehat_drv_fill_fb(
            d,
            bufflen - 1, // size of rgb pairs
            buff + 1);
    }
    else if (fn == SENSE_HAT_OP_GET_GAMMA) {
        sensehat_drv_get_gamma(d);
    }
    else if (fn == SENSE_HAT_OP_SET_GAMMA && bufflen == 33) {
        // assert bufflen == 33
        sensehat_drv_set_gamma(d, buff + 1);
    }
}

ErlDrvEntry sensehat_driver_entry = {
    NULL,                   /* F_PTR init, called when driver is loaded */
    sensehat_drv_start,     /* L_PTR start, called when port is opened */
    sensehat_drv_stop,      /* F_PTR stop, called when port is closed */
    sensehat_drv_output,        /* F_PTR output, called when erlang has sent */
    NULL,                   /* F_PTR ready_input, called when input descriptor ready */
    NULL,                   /* F_PTR ready_output, called when output descriptor ready */
    "sensehat_drv",         /* char *driver_name, the argument to open_port */
    NULL,                   /* F_PTR finish, called when unloaded */
    NULL,                   /* void *handle, Reserved by VM */
    NULL,                   /* F_PTR control, port_command callback */
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

DRIVER_INIT(sensehat_drv) /* must match name in driver_entry */
{
    return &sensehat_driver_entry;
}
