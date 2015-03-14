/*
 * Copyright (c) 2000-2007 Niels Provos <provos@citi.umich.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _EVENT2_BUFFEREVENT_H_
#define _EVENT2_BUFFEREVENT_H_

/** @file bufferevent.h

  Functions for buffering data for network sending or receiving.  Bufferevents
  are higher level than evbuffers: each has an underlying evbuffer for reading
  and one for writing, and callbacks that are invoked under certain
  circumstances.

 */

#ifdef __cplusplus
extern "C" {
#endif

#include <event-config.h>
#ifdef _EVENT_HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef _EVENT_HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* For int types. */
#include <event2/util.h>

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif


/* Just for error reporting - use other constants otherwise */
#define EVBUFFER_READ		0x01	/**< error encountered while reading */
#define EVBUFFER_WRITE		0x02	/**< error encountered while writing */
#define EVBUFFER_EOF		0x10	/**< eof file reached */
#define EVBUFFER_ERROR		0x20	/**< unrecoverable error encountered */
#define EVBUFFER_TIMEOUT	0x40	/**< user specified timeout reached */
struct bufferevent;
struct event_base;
struct evbuffer;

/**
   type definition for the read or write callback.

   The read callback is triggered when new data arrives in the input
   buffer and the amount of readable data exceed the low watermark
   which is 0 by default.

   The write callback is triggered if the write buffer has been
   exhausted or fell below its low watermark.

   @param bev the bufferevent that triggered the callback
   @param ctx the user specified context for this bufferevent
 */
typedef void (*evbuffercb)(struct bufferevent *bev, void *ctx);

/**
   type defintion for the error callback of a bufferevent.

   The error callback is triggered if either an EOF condition or another
   unrecoverable error was encountered.

   @param bev the bufferevent for which the error condition was reached
   @param what a conjunction of flags: EVBUFFER_READ or EVBUFFER write to
	  indicate if the error was encountered on the read or write path,
	  and one of the following flags: EVBUFFER_EOF, EVBUFFER_ERROR or
	  EVBUFFER_TIMEOUT.
   @param ctx the user specified context for this bufferevent
*/
typedef void (*everrorcb)(struct bufferevent *bev, short what, void *ctx);


/**
  Create a new bufferevent.

  libevent provides an abstraction on top of the regular event callbacks.
  This abstraction is called a buffered event.  A buffered event provides
  input and output buffers that get filled and drained automatically.  The
  user of a buffered event no longer deals directly with the I/O, but
  instead is reading from input and writing to output buffers.

  Once initialized, the bufferevent structure can be used repeatedly with
  bufferevent_enable() and bufferevent_disable().

  When read enabled the bufferevent will try to read from the file descriptor
  and call the read callback.  The write callback is executed whenever the
  output buffer is drained below the write low watermark, which is 0 by
  default.

  If multiple bases are in use, bufferevent_base_set() must be called before
  enabling the bufferevent for the first time.

  @param fd the file descriptor from which data is read and written to.
  		This file descriptor is not allowed to be a pipe(2).
  @param readcb callback to invoke when there is data to be read, or NULL if
         no callback is desired
  @param writecb callback to invoke when the file descriptor is ready for
         writing, or NULL if no callback is desired
  @param errorcb callback to invoke when there is an error on the file
         descriptor
  @param cbarg an argument that will be supplied to each of the callbacks
         (readcb, writecb, and errorcb)
  @return a pointer to a newly allocated bufferevent struct, or NULL if an
          error occurred
  @see bufferevent_base_set(), bufferevent_free()
  */
struct bufferevent *bufferevent_new(evutil_socket_t fd,
    evbuffercb readcb, evbuffercb writecb, everrorcb errorcb, void *cbarg);


/**
  Assign a bufferevent to a specific event_base.

  @param base an event_base returned by event_init()
  @param bufev a bufferevent struct returned by bufferevent_new()
  @return 0 if successful, or -1 if an error occurred
  @see bufferevent_new()
 */
int bufferevent_base_set(struct event_base *base, struct bufferevent *bufev);


/**
  Assign a priority to a bufferevent.

  @param bufev a bufferevent struct
  @param pri the priority to be assigned
  @return 0 if successful, or -1 if an error occurred
  */
int bufferevent_priority_set(struct bufferevent *bufev, int pri);


/**
  Deallocate the storage associated with a bufferevent structure.

  @param bufev the bufferevent structure to be freed.
  */
void bufferevent_free(struct bufferevent *bufev);


/**
  Changes the callbacks for a bufferevent.

  @param bufev the bufferevent object for which to change callbacks
  @param readcb callback to invoke when there is data to be read, or NULL if
         no callback is desired
  @param writecb callback to invoke when the file descriptor is ready for
         writing, or NULL if no callback is desired
  @param errorcb callback to invoke when there is an error on the file
         descriptor
  @param cbarg an argument that will be supplied to each of the callbacks
         (readcb, writecb, and errorcb)
  @see bufferevent_new()
  */
void bufferevent_setcb(struct bufferevent *bufev,
    evbuffercb readcb, evbuffercb writecb, everrorcb errorcb, void *cbarg);

/**
  Changes the file descriptor on which the bufferevent operates.

  @param bufev the bufferevent object for which to change the file descriptor
  @param fd the file descriptor to operate on
*/
void bufferevent_setfd(struct bufferevent *bufev, evutil_socket_t fd);

/**
  Write data to a bufferevent buffer.

  The bufferevent_write() function can be used to write data to the file
  descriptor.  The data is appended to the output buffer and written to the
  descriptor automatically as it becomes available for writing.

  @param bufev the bufferevent to be written to
  @param data a pointer to the data to be written
  @param size the length of the data, in bytes
  @return 0 if successful, or -1 if an error occurred
  @see bufferevent_write_buffer()
  */
int bufferevent_write(struct bufferevent *bufev,
    const void *data, size_t size);


/**
  Write data from an evbuffer to a bufferevent buffer.  The evbuffer is
  being drained as a result.

  @param bufev the bufferevent to be written to
  @param buf the evbuffer to be written
  @return 0 if successful, or -1 if an error occurred
  @see bufferevent_write()
 */
int bufferevent_write_buffer(struct bufferevent *bufev, struct evbuffer *buf);


/**
  Read data from a bufferevent buffer.

  The bufferevent_read() function is used to read data from the input buffer.

  @param bufev the bufferevent to be read from
  @param data pointer to a buffer that will store the data
  @param size the size of the data buffer, in bytes
  @return the amount of data read, in bytes.
 */
size_t bufferevent_read(struct bufferevent *bufev, void *data, size_t size);

/**
  Read data from a bufferevent buffer into an evbuffer.  This avoids
  memory copies.

  @param bufev the bufferevent to be read from
  @param buf the evbuffer to which to add data
  @return 0 if successful, or -1 if an error occurred.
 */
int bufferevent_read_buffer(struct bufferevent *bufev, struct evbuffer *buf);

/**
   Returns the input buffer.

   @param bufev the buffervent from which to get the evbuffer
   @return the evbuffer object for the input buffer
 */

struct evbuffer *bufferevent_get_input(struct bufferevent *bufev);

/**
   Returns the outut buffer.

   When filters are being used, the filters need to be manually
   triggered if the output buffer was manipulated.

   @param bufev the buffervent from which to get the evbuffer
   @return the evbuffer object for the output buffer
 */

struct evbuffer *bufferevent_get_output(struct bufferevent *bufev);

/**
  Enable a bufferevent.

  @param bufev the bufferevent to be enabled
  @param event any combination of EV_READ | EV_WRITE.
  @return 0 if successful, or -1 if an error occurred
  @see bufferevent_disable()
 */
int bufferevent_enable(struct bufferevent *bufev, short event);

/**
  Disable a bufferevent.

  @param bufev the bufferevent to be disabled
  @param event any combination of EV_READ | EV_WRITE.
  @return 0 if successful, or -1 if an error occurred
  @see bufferevent_enable()
 */
int bufferevent_disable(struct bufferevent *bufev, short event);


/**
  Set the read and write timeout for a buffered event.

  @param bufev the bufferevent to be modified
  @param timeout_read the read timeout
  @param timeout_write the write timeout
 */
void bufferevent_settimeout(struct bufferevent *bufev,
    int timeout_read, int timeout_write);

/**
  Sets the watermarks for read and write events.

  On input, a bufferevent does not invoke the user read callback unless
  there is at least low watermark data in the buffer.   If the read buffer
  is beyond the high watermark, the buffevent stops reading from the network.

  On output, the user write callback is invoked whenever the buffered data
  falls below the low watermark.

  @param bufev the bufferevent to be modified
  @param events EV_READ, EV_WRITE or both
  @param lowmark the lower watermark to set
  @param highmark the high watermark to set
*/

void bufferevent_setwatermark(struct bufferevent *bufev, short events,
    size_t lowmark, size_t highmark);

/** macro for getting access to the input buffer of a bufferevent */
#define EVBUFFER_INPUT(x)	bufferevent_get_input(x)
/** macro for getting access to the output buffer of a bufferevent */
#define EVBUFFER_OUTPUT(x)	bufferevent_get_output(x)

/**
   Support for filtering input and output of bufferevents.
 */

/**
   Flags that can be passed into filters to let them know how to
   deal with the incoming data.
*/
enum bufferevent_filter_state {
	/** usually set when processing data */
	BEV_NORMAL = 0,

	/** encountered EOF on read or done sending data */
	BEV_FLUSH = 1,
};

/**
   Values that filters can return.
 */
enum bufferevent_filter_result {
	/** everything is okay */
	BEV_OK = 0,

	/** the filter needs to read more data before output */
	BEV_NEED_MORE = 1,

	/** the filter enountered a critical error, no further data
	    can be processed. */
	BEV_ERROR = 2
};

struct bufferevent_filter;

/**
  Creates a new filtering object for a bufferevent.

  Filters can be used to implement compression, authentication, rate limiting,
  etc. for bufferevents.  Filters can be associated with the input or output
  path or both.   Filters need to be inserted with bufferevent_filter_insert()
  on either the input or output path.

  For example, when implementing compression, both an input and an
  output filters are required.   The output filter compress all output
  as it passes along whereas the input filter decompresses all input as
  it is being read from the network.

  Some filters may require specificaly behavior such as flushing their buffers
  on EOF.   To allom them to do that, a bufferevent will invoke the filter
  with BEV_FLUSH to let it know that EOF has been reached.

  When a filter needs more data before it can output any data, it may return
  BEV_NEED_MORE in which case the filter chain is being interrupted until
  more data arrives.   A filter can indicate a fatal error by returning
  BEV_ERROR.  Otherwise, it should return BEV_OK.

  @param init_context an optional function that initializes the ctx parameter.
  @param free_context an optional function to free memory associated with the
         ctx parameter.
  @param process the filtering function that should be invokved either during
         input or output depending on where the filter should be attached.
  @param ctx additional context that can be passed to the process function
  @return a bufferevent_filter object that can subsequently be installed
*/
struct bufferevent_filter *bufferevent_filter_new(
	void (*init_context)(void *),
	void (*free_context)(void *),
	enum bufferevent_filter_result (*process)(
		struct evbuffer *src, struct evbuffer *dst,
		enum bufferevent_filter_state state, void *ctx), void *ctx);

/**
   Frees the filter object.

   It must have been removed from the bufferevent before it can be freed.

   @param filter the filter to be freed
   @see bufferevent_filter_remove()
*/
void bufferevent_filter_free(struct bufferevent_filter *filter);

/** Filter types for inserting or removing filters */
enum bufferevent_filter_type {
	/** filter is being used for input */
	BEV_INPUT = 0,

	/** filter is being used for output */
	BEV_OUTPUT = 1
};

/**
   Inserts a filter into the processing of data for bufferevent.

   A filter can be inserted only once.  It can not be used again for
   another insert unless it have been removed via
   bufferevent_filter_remove() first.

   Input filters are inserted at the end, output filters at the
   beginning of the queue.

   @param bufev the bufferevent object into which to install the filter
   @param filter_type either BEV_INPUT or BEV_OUTPUT
   @param filter the filter object
   @see bufferevent_filter_remove()
 */
void bufferevent_filter_insert(struct bufferevent *bufev,
    enum bufferevent_filter_type filter_type,
    struct bufferevent_filter *filter);

/**
   Removes a filter from the bufferevent.

   A filter should be flushed via buffervent_trigger_filter before removing
   it from a bufferevent.  Any remaining intermediate buffer data is going
   to be lost.

   @param bufev the bufferevent object from which to remove the filter
   @param filter_type either BEV_INPUT or BEV_OUTPUT
   @param filter the filter object or NULL to trigger all filters
   @see bufferevent_trigger_filter()
*/
void bufferevent_filter_remove(struct bufferevent *bufev,
    enum bufferevent_filter_type filter_type,
    struct bufferevent_filter *filter);

/**
  Triggers the filter chain the specified filter to produce more
  data is possible.  This is primarily for time-based filters such
  as rate-limiting to produce more data as time passes.

  @param bufev the bufferevent object to which the filter belongs
  @param filter the bufferevent filter at which to start
  @param iotype either BEV_INPUT or BEV_OUTPUT depending on where the filter
	 was installed
  @param state either BEV_NORMAL or BEV_FLUSH
  @return -1 on failure, 0 if no data was produces, 1 if data was produced
 */

int
bufferevent_trigger_filter(struct bufferevent *bufev,
    struct bufferevent_filter *filter, int iotype,
    enum bufferevent_filter_state state);

#ifdef __cplusplus
}
#endif

#endif /* _EVENT2_BUFFEREVENT_H_ */
