/* bfin_timer.c --- 
 * 
 * Filename: bfin_timer.c
 * Description: 
 * Author: Lumenosys Robotics
 * Maintainer: 
 * URL: 
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary:
 *
 * Erlang bfin_timer driver for erlang/uclinux on the Blackfin. Some
 * code inspired by Tony Rog's drivers.
 */

/* Change Log:
 * 
 * 
 */

/* Code: */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <linux/types.h>

//#define __DEBUG

#ifdef __DEBUG
#define DEBUG(x, ...) printf("DEBUG %s:" x, __PRETTY_FUNCTION__, ##__VA_ARGS__)
#else
#define DEBUG(x, ...)
#endif

#define ERROR(x, ...) printf("ERROR %s:" x, __PRETTY_FUNCTION__, ##__VA_ARGS__)

#include <erl_driver.h>

/* header for the blackfin timer driver */
#include <asm/bfin_simple_timer.h>

/* defines */
#define MAXBUFSIZE (sizeof(unsigned int) * 2)
#define STATE_CLOSED -1
#define STATE_IDLE    0
#define STATE_RUN     1
#define STATE_BUSY    2

#define CMD_OPEN       1
#define CMD_CLOSE      2
#define CMD_SET_PERIOD 3
#define CMD_SET_WIDTH  4
#define CMD_SET_MODE   5
#define CMD_START      6
#define CMD_STOP       7

#define MAX_BFIN_TIMERS 8

typedef struct {
	ErlDrvPort port;
	int fd[MAX_BFIN_TIMERS];
	int state[MAX_BFIN_TIMERS];
	/* uint32_t retval; */
	/* uint16_t size; */
	/* int written; */
	/* int rx_msg_state; */
	/* int read_cnt; */
	/* int rx_msg_len; */
	char data[MAXBUFSIZE];
} bfin_timer_ctx_t;

/* prototypes */
static ErlDrvData bfin_timer_start(ErlDrvPort port, char *buff);
static void bfin_timer_stop(ErlDrvData handle);
static void bfin_timer_ready_io(ErlDrvData handle, ErlDrvEvent event);

/* globals */

/* functions */

/* marshalling routines */
#define ALIGNBYTES_SHORT (sizeof(short) - 1)
#define ALIGN_SHORT(val)                                    \
  (((unsigned)val + ALIGNBYTES_SHORT) & ~ALIGNBYTES_SHORT)

#define ALIGNBYTES_LONG (sizeof(long) - 1)
#define ALIGN_LONG(val)                                     \
  (((unsigned)val + ALIGNBYTES_LONG) & ~ALIGNBYTES_LONG)

#define ALIGNBYTES_INT (sizeof(int) - 1)
#define ALIGN_INT(val)                                  \
  (((unsigned)val + ALIGNBYTES_INT) & ~ALIGNBYTES_INT)

#define ALIGNBYTES_LONG_LONG (sizeof(long long) - 1)
#define ALIGN_LONG_LONG(val)                                        \
  (((unsigned)val + ALIGNBYTES_LONG_LONG) & ~ALIGNBYTES_LONG_LONG)

#define ALIGNBYTES_FLOAT (sizeof(float) - 1)
#define ALIGN_FLOAT(val)                                    \
  (((unsigned)val + ALIGNBYTES_FLOAT) & ~ALIGNBYTES_FLOAT)

#define ALIGNBYTES_DOUBLE (sizeof(double) - 1)
#define ALIGN_DOUBLE(val)                                       \
  (((unsigned)val + ALIGNBYTES_DOUBLE) & ~ALIGNBYTES_DOUBLE)

#define ALIGNBYTES_LONG_DOUBLE (sizeof(long double) - 1)
#define ALIGN_LONG_DOUBLE(val)                                          \
  (((unsigned)val + ALIGNBYTES_LONG_DOUBLE) & ~ALIGNBYTES_LONG_DOUBLE)

/* unmarshal short */
int get_short(short *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_SHORT(bufidx);
	short *pShort = (short *)&buffer[bufidx];
	*value = *pShort;
	return bufidx += sizeof(short);
}

/* unmarshal unsigned Short */
int get_ushort(unsigned short *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_SHORT(bufidx);
	unsigned short *puShort = (unsigned short *)&buffer[bufidx];
	*value = *puShort;
	return bufidx += sizeof(unsigned short);
}

/* unmarshal signed int */
int get_int(int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	int *pint = (int *)&buffer[bufidx];
	*value = *pint;
	return bufidx += sizeof(int);
}

/* unmarshal unsigned int */
int get_uint(unsigned int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	unsigned int *puint = (unsigned int *)&buffer[bufidx];
	*value = *puint;
	return bufidx += sizeof(unsigned int);
}

/* unmarshal long */
int get_long(int *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	int *pint = (int *)&buffer[bufidx];
	*value = *pint;
	return bufidx += sizeof(int);
}

/* unmarshal unsigned long */
int get_ulong(unsigned long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_INT(bufidx);
	unsigned long *puint = (unsigned long *)&buffer[bufidx];
	*value = *puint;
	return bufidx += sizeof(unsigned long);
}

/* unmarshal long long */
int get_longlong(long long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_LONG(bufidx);
	long long *plong = (long long *)&buffer[bufidx];
	*value = *plong;
	return bufidx += sizeof(long long);
}

/* unmarshal unsigned long long */
int get_ulonglong(unsigned long long *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_LONG(bufidx);
	unsigned long long *pulong = (unsigned long long *)&buffer[bufidx];
	*value = *pulong;
	return bufidx += sizeof(unsigned long long);
}

/* unmarshal double */
int get_double(double *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_DOUBLE(bufidx);
	double *pdouble = (double *)&buffer[bufidx];
	*value = *pdouble;
	return bufidx += sizeof(double);
}

/* unmarshal long double */
int get_longdouble(long double *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_LONG_DOUBLE(bufidx);
	long double *pdouble = (long double *)&buffer[bufidx];
	*value = *pdouble;
	return bufidx += sizeof(long double);
}

/* unmarshal float */
int get_float(float *value, uint8_t *buffer, int bufidx)
{
	bufidx = ALIGN_FLOAT(bufidx);
	float *pfloat = (float *)&buffer[bufidx];
	*value = *pfloat;
	return bufidx += sizeof(float);
}

/* unmarshal octet */
int get_octet(uint8_t *value, uint8_t *buffer, int bufidx)
{
	uint8_t *poctet = (uint8_t *)&buffer[bufidx];
	*value = *poctet;
	return bufidx+1;
}

/* unmarshal char */
int get_char(char *value, uint8_t *buffer, int bufidx)
{
	char *pchar = (char *)&buffer[bufidx];
	*value = *pchar;
	return bufidx+1;
}

static ErlDrvData bfin_timer_start(ErlDrvPort port, char *buff)
{
	int i;

	bfin_timer_ctx_t* ctx = (bfin_timer_ctx_t *)driver_alloc(sizeof(bfin_timer_ctx_t));
	if (ctx == NULL) {
		return NULL;
	}

	for (i = 0; i < MAX_BFIN_TIMERS; i++) {
		ctx->fd[i] = -1;
		ctx->state[i] = STATE_CLOSED;
	}
	ctx->port = port;

	DEBUG("bfin_timer driver starting...\n");

	/* set port IO to binary */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	return (ErlDrvData)ctx;
}

static void bfin_timer_stop(ErlDrvData handle)
{
	bfin_timer_ctx_t *ctx = (bfin_timer_ctx_t *)handle;
	int i;

	for (i = 0; i < MAX_BFIN_TIMERS; i++) {
		if (ctx->state[i] != STATE_CLOSED) {
			/* call driver_select to stop event monitoring */
			driver_select(ctx->port, (ErlDrvEvent)ctx->fd[i], ERL_DRV_USE, 0);
			close(ctx->fd[i]);
			ctx->state[i] = STATE_CLOSED;
		}
	}
	driver_free((char*)handle);
}

static void bfin_timer_ready_io(ErlDrvData handle, ErlDrvEvent event)
{
	bfin_timer_ctx_t *ctx = (bfin_timer_ctx_t *)handle;
	int ret;
	char hbuf;
	int bfin_timer_dev;
	int i;
	char* err_str;

	/* find which timer this FD event is for, read the value, and
	 * send a message up to erlang with the timer# and event
	 * data */
	bfin_timer_dev = -1;
	for (i = 0; i < MAX_BFIN_TIMERS; i++) {
		if ((int)event == ctx->fd[i]) {
			bfin_timer_dev = i;
		}
	}
	if (bfin_timer_dev == -1) {
		ERROR("Ready event doesn't match any known device!!??\n");
		ret = -1;
		goto error;
	}

        /* FIXME: this bit of code will allow returning CAPTURE values */
	/* bfin_timer device ready for read */
	/* read from timer port and send what we get up to erlang */
	/* ret = read((int)event, ctx->data, MAXBUFSIZE); */
	/* if (ret > 0) { */
	
	/* deliver up to erlang */
	hbuf = 2; /* event response */
	//hbuf[1] = (char)bfin_timer_dev;
	driver_output2(ctx->port, &hbuf, 1, (char *)&bfin_timer_dev, 1);
	/* call driver_select with FD and turn on event monitoring */
	driver_select(ctx->port, (ErlDrvEvent)ctx->fd[bfin_timer_dev], ERL_DRV_READ, 1);
	goto done;
	/* } */
	/* ERROR("read returned %d: %s\n", ret, strerror(errno)); */
error:
	ctx->state[bfin_timer_dev] = STATE_IDLE;
	driver_select(ctx->port, (ErlDrvEvent)ctx->fd, ERL_DRV_USE, 0);
	err_str = erl_errno_id(ret);
	hbuf = 255;
	driver_output2(ctx->port, (char *)&hbuf, 1,  err_str, strlen(err_str));
done:
	return;
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
	char* ptr;
	if ((len+1) > rsize) {
		/* this will get freed automatically when control returns */
		ErlDrvBinary* bin = driver_alloc_binary(len+1);
		if (bin == NULL)
			return -1;
		ptr = bin->orig_bytes;	
		*rbuf = (char*) bin;
	} else {
		ptr = *rbuf;
	}
	*ptr++ = rep;
	if (buf != NULL) {
		memcpy(ptr, buf, len);
	}
	return len+1;
}

static ErlDrvSSizeT bfin_timer_control(ErlDrvData handle, 
				  unsigned int cmd, 
				  char* buf0, 
				  ErlDrvSizeT len, 
				  char** rbuf, 
				  ErlDrvSizeT rsize)
{
	bfin_timer_ctx_t* ctx = (bfin_timer_ctx_t*)handle;
	int bufidx;
	uint8_t *buf = (uint8_t *)buf0;
	int ret;
	unsigned int bfin_timer_dev;
	unsigned int period;
	unsigned int width;
	unsigned int mode;
	char devname[80];

	switch(cmd) {
	case CMD_OPEN:// <<bfin_timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] != STATE_CLOSED) {
			goto error;
		}

		/* contruct the complete device name */
		sprintf(devname, "/dev/timer%d", bfin_timer_dev);
		
		/* open the timer device */
		ctx->fd[bfin_timer_dev] = open(devname, O_RDWR);
		if (ctx->fd[bfin_timer_dev] < 0) {
			ERROR( "failed to open device: %s\n", strerror(errno));
			goto error;
		}

		DEBUG("CMD_OPEN: device = %s (%d)\n", devname, ctx->fd[bfin_timer_dev]);

		ctx->state[bfin_timer_dev] = STATE_IDLE;

		goto ok;
	case CMD_CLOSE:// <<bfin_timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] != STATE_IDLE) {
			DEBUG("Timer %d must be stopped before closing!\n", bfin_timer_dev);
			goto error;
		}

		/* tell erlang to clear event */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[bfin_timer_dev], ERL_DRV_USE, 0);
		close(ctx->fd[bfin_timer_dev]);
		ctx->state[bfin_timer_dev] = STATE_CLOSED;
		DEBUG("closed timer device %d\n", bfin_timer_dev);
		goto ok;
	case CMD_SET_PERIOD: // <<bfin_timer_dev:32, period:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] == STATE_CLOSED) {
			goto error;
		}

		/* grab the period */
		bufidx = get_uint(&period, buf, bufidx);

		DEBUG("CMD_SET_PERIOD: timer%d period -> %d\n", bfin_timer_dev, period);
		
		ret = ioctl(ctx->fd[bfin_timer_dev], BFIN_SIMPLE_TIMER_SET_PERIOD, period);
		if (ret < 0) {
			ERROR("timer%d: set period failed: %s\n", bfin_timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_SET_WIDTH: // <<bfin_timer_dev:32, width:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] == STATE_CLOSED) {
			goto error;
		}

		/* grab the width */
		bufidx = get_uint(&width, buf, bufidx);

		DEBUG("CMD_SET_WIDTH: timer%d width-> %d\n", bfin_timer_dev, width);
		
		ret = ioctl(ctx->fd[bfin_timer_dev], BFIN_SIMPLE_TIMER_SET_WIDTH, width);
		if (ret < 0) {
			ERROR("timer%d: set width failed: %s\n", bfin_timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_SET_MODE: // <<bfin_timer_dev:32, mode:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] != STATE_IDLE) {
			DEBUG("Timer must be opened and IDLE to set mode\n");
			goto error;
		}

		/* grab the mode */
		bufidx = get_uint(&mode, buf, bufidx);

		DEBUG("CMD_SET_MODE: timer%d mode -> %d\n", bfin_timer_dev, mode);
		
		ret = ioctl(ctx->fd[bfin_timer_dev], BFIN_SIMPLE_TIMER_SET_MODE, mode);
		if (ret < 0) {
			ERROR("timer%d: set mode failed: %s\n", bfin_timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_START:// <<bfin_timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] != STATE_IDLE) {
			DEBUG("Timer %d must be idle before starting!\n", bfin_timer_dev);
			goto error;
		}

		ret = ioctl(ctx->fd[bfin_timer_dev], BFIN_SIMPLE_TIMER_START);
		if (ret < 0) {
			ERROR("timer%d: start failed: %s\n", bfin_timer_dev, strerror(errno));
			goto error;
		}

		/* call driver_select with FD and turn on event monitoring */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[bfin_timer_dev], ERL_DRV_READ, 1);

		DEBUG("CMD_START: timer%d\n", 
		       bfin_timer_dev);

		ctx->state[bfin_timer_dev] = STATE_RUN;

		goto ok;
	case CMD_STOP:// <<bfin_timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&bfin_timer_dev, buf, bufidx);

		if ((bfin_timer_dev < 0) || (bfin_timer_dev > MAX_BFIN_TIMERS)) {
			goto badarg;
		}

		if (ctx->state[bfin_timer_dev] != STATE_RUN) {
			DEBUG("Timer %d must be running before stopping!\n", bfin_timer_dev);
			goto error;
		}

		ret = ioctl(ctx->fd[bfin_timer_dev], BFIN_SIMPLE_TIMER_STOP);
		if (ret < 0) {
			ERROR("timer%d: stop failed: %s\n", bfin_timer_dev, strerror(errno));
			goto error;
		}

		/* call driver_select with FD and turn off event monitoring */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[bfin_timer_dev], ERL_DRV_USE, 0);

		DEBUG("CMD_STOP: timer%d\n", 
		       bfin_timer_dev);

		ctx->state[bfin_timer_dev] = STATE_IDLE;

		goto ok;
	default:
		goto error;
	}
ok:
	return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
	errno = EINVAL;
	goto error;
error:
	{
		char* err_str = erl_errno_id(errno);
		return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
	}
}	

void bfin_timer_stop_select(ErlDrvEvent event, void* reserved)
{
	/* close FD? */
}

ErlDrvEntry bfin_timer_entry = {
    NULL,	        /* F_PTR init, called when driver is loaded */
    bfin_timer_start,          /* L_PTR start, called when port is opened */
    bfin_timer_stop,           /* F_PTR stop, called when port is closed */
    NULL,               /* F_PTR output, called when erlang has sent us a message*/
    bfin_timer_ready_io,    /* F_PTR ready_input, called when input descriptor ready */
    bfin_timer_ready_io,    /* F_PTR ready_output, called when output descriptor ready */
    "bfin_timer",	        /* char *driver_name, the argument to open_port */
    NULL,	        /* F_PTR finish, called when unloaded */
    NULL,               /* void *handle, Reserved by VM */
    bfin_timer_control,	/* F_PTR control, port_command callback */
    NULL,	        /* F_PTR timeout, reserved */
    NULL,		/* F_PTR outputv, reserved */
    NULL,               /* F_PTR ready_async, only for async drivers */
    NULL,               /* F_PTR flush, called when port is about to
				   be closed, but there is data in
				   driver queue */
    NULL,               /* F_PTR call, much like control, sync call to
				   driver */
    NULL,               /* F_PTR event, called when an event selected
				   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				       set to this value */
    0,                  /* int driver_flags, see documentation */
    NULL,               /* void *handle2, reserved for VM use */
    NULL,               /* F_PTR process_exit, called when a monitored
			   process dies */
    bfin_timer_stop_select                /* F_PTR stop_select, called to close an event
				   object */
};

DRIVER_INIT(bfin_timer) /* must match name in driver_entry */
{
    return &bfin_timer_entry;
}
	
/* bfin_timer.c ends here */
