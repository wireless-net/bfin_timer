/* timer.c --- 
 * 
 * Filename: timer.c
 * Description: 
 * Author: Devin Butterfield
 * Maintainer: 
 * Created: Sat Nov  2 11:48:15 2013 (-0700)
 * Version: 
 * Last-Updated: Mon Oct 20 22:07:30 2014 (-0700)
 *	     By: Devin Butterfield
 *     Update #: 187
 * URL: 
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary:
 *
 * Erlang timer driver for erlang/uclinux on the Blackfin. 
 */

/* Change Log:
 * 
 * 
 */

/* *This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth
 * Floor, Boston, MA 02110-1301, USA.
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
/* #include <sys/ioctl.h> */
#include <linux/types.h>

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

#define MAX_TIMERS 8

typedef struct {
	ErlDrvPort port;
	int fd[MAX_TIMERS];
	int state[MAX_TIMERS];
	/* uint32_t retval; */
	/* uint16_t size; */
	/* int written; */
	/* int rx_msg_state; */
	/* int read_cnt; */
	/* int rx_msg_len; */
	char data[MAXBUFSIZE];
} timer_ctx_t;

/* prototypes */
static ErlDrvData timer_start(ErlDrvPort port, char *buff);
static void timer_stop(ErlDrvData handle);
static void timer_ready_io(ErlDrvData handle, ErlDrvEvent event);

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

static ErlDrvData timer_start(ErlDrvPort port, char *buff)
{
	int i;

	timer_ctx_t* ctx = (timer_ctx_t *)driver_alloc(sizeof(timer_ctx_t));
	if (ctx == NULL) {
		return NULL;
	}

	for (i = 0; i < MAX_TIMERS; i++) {
		ctx->fd[i] = -1;
		stc->state[i] = STATE_CLOSED;
	}
	ctx->port = port;

	DEBUG("timer driver starting...\n");

	/* set port IO to binary */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	return (ErlDrvData)ctx;
}

static void timer_stop(ErlDrvData handle)
{
	timer_ctx_t *ctx = (timer_ctx_t *)handle;
	int i;

	for (i = 0; i < MAX_TIMERS; i++) {
		if (ctx->state[i] != STATE_CLOSED) {
			/* call driver_select to stop event monitoring */
			driver_select(ctx->port, (ErlDrvEvent)ctx->fd[i], ERL_DRV_USE, 0);
			close(ctx->fd[i]);
			ctx->state[i] = STATE_CLOSED;
		}
	}
	driver_free((char*)handle);
}

static void timer_ready_io(ErlDrvData handle, ErlDrvEvent event)
{
	timer_ctx_t *ctx = (timer_ctx_t *)handle;
	int ret;
	char hbuf[2];
	int timer_dev;
	int i;
	char* err_str;

	/* find which timer this FD event is for, read the value, and
	 * send a message up to erlang with the timer# and event
	 * data */
	timer_dev = -1;
	for (i = 0; i < MAX_TIMERS; i++) {
		if (event == ctx->fds[i]) {
			timer_dev = i;
		}
	}
	if (timer_dev == -1) {
		ERROR("Ready event doesn't match any known device!!??\n");
		ret = -1;
		goto error;
	}

	/* timer device ready for read */
	/* read from timer port and send what
	 * we get up to erlang */
	ret = read(event, ctx->data, MAXBUFSIZE);
	if (ret > 0) {
		/* deliver up to erlang */
		hbuf[0] = 2; /* event response */
		hbuf[1] = (char)timer_dev;
		driver_output2(ctx->port, &hbuf, 2, (char *)&ctx->data[0], ret);
		ctx->state = STATE_IDLE;
		/* done this event for now */
		//driver_select(ctx->port, (ErlDrvEvent)ctx->fd, ERL_DRV_USE, 0);
		/* call driver_select with FD and turn on event monitoring */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[timer_dev], ERL_DRV_READ, 1);
		goto done;
	}
	ERROR("read returned %d: %s\n", ret, strerror(errno));
error:
	ctx->state = STATE_IDLE;
	driver_select(ctx->port, (ErlDrvEvent)ctx->fd, ERL_DRV_USE, 0);
	err_str = erl_errno_id(ret);
	hbuf[0] = 255;
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

static ErlDrvSSizeT timer_control(ErlDrvData handle, 
				  unsigned int cmd, 
				  char* buf0, 
				  ErlDrvSizeT len, 
				  char** rbuf, 
				  ErlDrvSizeT rsize)
{
	timer_ctx_t* ctx = (timer_ctx_t*)handle;
	int bufidx;
	uint8_t *buf = (uint8_t *)buf0;
	int ret;
	unsigned int timer_dev;
	unsigned int period;
	unsigned int width;
	char devname[80];

	switch(cmd) {
	case CMD_OPEN:// <<timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] != STATE_CLOSED) {
			goto error;
		}

		/* contruct the complete device name */
		sprintf(devname, "/dev/timer%d", timer_dev);

		DEBUG("CMD_OPEN: device = %s\n", 
		      devname);
		
		/* open the timer device */
		ctx->fd[timer_dev] = open(devname, O_RDWR);
		if (ctx->fd < 0) {
			ERROR( "failed to open device: %s\n", strerror(errno));
			goto error;
		}

		ctx->state[timer_dev] = STATE_IDLE;

		goto ok;
	case CMD_CLOSE:// <<timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] != STATE_IDLE) {
			DEBUG("Timer %d must be stopped before closing!\n", timer_dev);
			goto error;
		}

		/* tell erlang to clear event */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[timer_dev], ERL_DRV_USE, 0);
		close(ctx->fd[timer_dev]);
		ctx->state[timer_dev] = STATE_CLOSED;
		DEBUG("closed timer device %d\n", timer_dev);
		goto ok;
	case CMD_SET_PERIOD: // <<timer_dev:32, period:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] == STATE_CLOSED) {
			goto error;
		}

		/* grab the period */
		bufidx = get_uint(&period, buf, bufidx);

		DEBUG("CMD_SET_PERIOD: timer%d period -> %d\n", timer_dev, period);
		
		ret = ioctl(ctx->fd[timer_dev], BFIN_SIMPLE_TIMER_SET_PERIOD, period);
		if (ret < 0) {
			ERROR("timer%d: set period failed: %s\n", timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_SET_WIDTH: // <<timer_dev:32, width:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] == STATE_CLOSED) {
			goto error;
		}

		/* grab the width */
		bufidx = get_uint(&width, buf, bufidx);

		DEBUG("CMD_SET_WIDTH: timer%d width-> %d\n", timer_dev, width);
		
		ret = ioctl(ctx->fd[timer_dev], BFIN_SIMPLE_TIMER_SET_WIDTH, width);
		if (ret < 0) {
			ERROR("timer%d: set width failed: %s\n", timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_SET_MODE: // <<timer_dev:32, mode:32>>
		bufidx = 0;		

		/* grab the timer number */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] != STATE_IDLE) {
			DEBUG("Timer must be opened and IDLE to set mode\n");
			goto error;
		}

		/* grab the mode */
		bufidx = get_uint(&mode, buf, bufidx);

		DEBUG("CMD_SET_MODE: timer%d mode -> %d\n", timer_dev, mode);
		
		ret = ioctl(ctx->fd[timer_dev], BFIN_SIMPLE_TIMER_SET_MODE, mode);
		if (ret < 0) {
			ERROR("timer%d: set mode failed: %s\n", timer_dev, strerror(errno));
			goto error;
		}
		goto ok;
	case CMD_START:// <<timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] != STATE_IDLE) {
			DEBUG("Timer %d must be idle before starting!\n", timer_dev);
			goto error;
		}

		ret = ioctl(ctx->fd[timer_dev], BFIN_SIMPLE_TIMER_START);
		if (ret < 0) {
			ERROR("timer%d: start failed: %s\n", timer_dev, strerror(errno));
			goto error;
		}

		/* call driver_select with FD and turn on event monitoring */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[timer_dev], ERL_DRV_READ, 1);

		DEBUG("CMD_START: timer%d\n", 
		       timer_dev);

		ctx->state[timer_dev] = STATE_RUN;

		goto ok;
	case CMD_STOP:// <<timer_dev:32>>

		bufidx = 0;		

		/* grab the timer device index */
		bufidx = get_uint(&timer_dev, buf, bufidx);

		if ((timer_dev < 0) || (timer_dev > MAX_TIMERS)) {
			goto error;
		}

		if (ctx->state[timer_dev] != STATE_RUN) {
			DEBUG("Timer %d must be running before stopping!\n", timer_dev);
			goto error;
		}

		ret = ioctl(ctx->fd[timer_dev], BFIN_SIMPLE_TIMER_STOP);
		if (ret < 0) {
			ERROR("timer%d: stop failed: %s\n", timer_dev, strerror(errno));
			goto error;
		}

		/* call driver_select with FD and turn off event monitoring */
		driver_select(ctx->port, (ErlDrvEvent)ctx->fd[timer_dev], ERL_DRV_USE, 0);

		DEBUG("CMD_STOP: timer%d\n", 
		       timer_dev);

		ctx->state[timer_dev] = STATE_IDLE;

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

void timer_stop_select(ErlDrvEvent event, void* reserved)
{
	/* close FD? */
}

ErlDrvEntry timer_entry = {
    NULL,	        /* F_PTR init, called when driver is loaded */
    timer_start,          /* L_PTR start, called when port is opened */
    timer_stop,           /* F_PTR stop, called when port is closed */
    NULL,               /* F_PTR output, called when erlang has sent us a message*/
    timer_ready_io,    /* F_PTR ready_input, called when input descriptor ready */
    timer_ready_io,    /* F_PTR ready_output, called when output descriptor ready */
    "timer",	        /* char *driver_name, the argument to open_port */
    NULL,	        /* F_PTR finish, called when unloaded */
    NULL,               /* void *handle, Reserved by VM */
    timer_control,	/* F_PTR control, port_command callback */
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
    timer_stop_select                /* F_PTR stop_select, called to close an event
				   object */
};

DRIVER_INIT(timer) /* must match name in driver_entry */
{
    return &timer_entry;
}
	
/* timer.c ends here */
