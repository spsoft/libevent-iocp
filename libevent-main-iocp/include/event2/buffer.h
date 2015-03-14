/*
 * Copyright (c) 2007 Niels Provos <provos@citi.umich.edu>
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
#ifndef _EVENT2_BUFFER_H_
#define _EVENT2_BUFFER_H_

/** @file buffer.h

  Functions for buffering data for network sending or receiving.

  An evbuffer can be used for preparing data before sending it to
  the network or conversely for reading data from the network.
  Evbuffers try to avoid memory copies as much as possible.  As a
  result evbuffers can be used to pass data around witout actually
  incurring the overhead of copying the data.

  A new evbuffer can be allocated with evbuffer_new(), and can be
  freed with evbuffer_free().

  There are several guide lines for using evbuffers.

  - if you already know how much data you are going to add as a result
    of calling evbuffer_add() multiple times, it makes sense to use
    evbuffer_expand() first to make sure that enough memory is allocated
    before hand.

  - evbuffer_add_buffer() adds the contents of one buffer to the other
    without incurring any memory copies.

  - evbuffer_add() and evbuffer_add_buffer() do not mix very well.

  As the contents of an evbuffer can be stored into multiple different
  memory blocks, it cannot be accessed directly.  Instead, evbuffer_pullup()
  can be used to force a specified number of bytes to be continuous. This
  will cause memory reallocation and memory copies if the data is split
  across multiple blocks.

 */

#ifdef __cplusplus
extern "C" {
#endif

#include <event-config.h>
#include <stdarg.h>

struct evbuffer;

/**
  Allocate storage for a new evbuffer.

  @return a pointer to a newly allocated evbuffer struct, or NULL if an error
          occurred
 */
struct evbuffer *evbuffer_new(void);


/**
  Deallocate storage for an evbuffer.

  @param buf pointer to the evbuffer to be freed
 */
void evbuffer_free(struct evbuffer *buf);

/**
  Returns the total number of bytes stored in the event buffer

  @param buf pointer to the evbuffer
  @return the number of bytes stored in the event buffer
*/
size_t evbuffer_get_length(struct evbuffer *buf);

/**
   Returns the contiguous number of available bytes in the first buffer chain.

   This is useful when processing of all available data can be split up into
   chunks.  Calls to evbuffer_pullup() that cause reallocation and copying
   of data can thus be avoided.

   @param buf pointer to the evbuffer
   @return 0 if no data is available, otherwise the number of available bytes
     in the first buffer chain.
*/
size_t evbuffer_get_contiguous_space(struct evbuffer *buf);

/**
  Expands the available space in an event buffer.

  Expands the available space in the event buffer to at least datlen, so that
  appending datlen additional bytes will not require any new allocations.

  @param buf the event buffer to be expanded
  @param datlen the new minimum length requirement
  @return 0 if successful, or -1 if an error occurred
*/
int evbuffer_expand(struct evbuffer *buf, size_t datlen);

/**
   Reserves space in the last chain of an event buffer.

   Makes space available in the last chain of an event buffer that can
   be arbitrarily written to by a user.  The space does not become
   available for reading until it has been committed.

   Multiple subsequent calls to this function will make the same space
   available until evbuffer_commit_space() has been called.

   @param buf the event buffer in which to reserve space.
   @param size how much space to make available.
   @return the pointer to the available space or NULL on error.
   @see evbuffer_commit_space
*/

unsigned char *evbuffer_reserve_space(struct evbuffer *buf, size_t size);

/**
   Commits previously reserved space.

   Commits some of the space previously reserved.  It then becomes
   available for reading.

   @param buf the event buffer in which to reserve space.
   @param size how much space to commit.
   @return 0 on success, -1 on error
   @see evbuffer_reserve_space
*/

int evbuffer_commit_space(struct evbuffer *buf, size_t size);


/**
  Append data to the end of an evbuffer.

  @param buf the event buffer to be appended to
  @param data pointer to the beginning of the data buffer
  @param datlen the number of bytes to be copied from the data buffer
 */
int evbuffer_add(struct evbuffer *buf, const void *data, size_t datlen);


/**
  Read data from an event buffer and drain the bytes read.

  @param buf the event buffer to be read from
  @param data the destination buffer to store the result
  @param datlen the maximum size of the destination buffer
  @return the number of bytes read
 */
int evbuffer_remove(struct evbuffer *buf, void *data, size_t datlen);

/**
  Read data from an event buffer into another event buffer draining
  the bytes from the src buffer read.  This function avoids memcpy
  as possible.

  @param src the event buffer to be read from
  @param dst the destination event buffer to store the result into
  @param datlen the maximum numbers of bytes to transfer
  @return the number of bytes read
 */
int evbuffer_remove_buffer(struct evbuffer *src, struct evbuffer *dst,
    size_t datlen);

/** Used to tell evbuffer_readln what kind of line-ending to look for.
 */
enum evbuffer_eol_style {
	/** Any sequence of CR and LF characters is acceptable as an EOL. */
	EVBUFFER_EOL_ANY,
	/** An EOL is an LF, optionally preceded by a CR.  This style is
	 * most useful for implementing text-based internet protocols. */
	EVBUFFER_EOL_CRLF,
	/** An EOL is a CR followed by an LF. */
	EVBUFFER_EOL_CRLF_STRICT,
	/** An EOL is a LF. */
	EVBUFFER_EOL_LF
};

/**
 * Read a single line from an event buffer.
 *
 * Reads a line terminated by an EOL as determined by the evbuffer_eol_style
 * argument.  Returns a newly allocated nul-terminated string; the caller must
 * free the returned value.  The EOL is not included in the returned string.
 *
 * @param buffer the evbuffer to read from
 * @param n_read_out if non-NULL, points to a size_t that is set to the
 *       number of characters in the returned string.  This is useful for
 *       strings that can contain NUL characters.
 * @param eol_style the style of line-ending to use.
 * @return pointer to a single line, or NULL if an error occurred
 */
char *evbuffer_readln(struct evbuffer *buffer, size_t *n_read_out,
    enum evbuffer_eol_style eol_style);

/**
   Obsolete alias for evbuffer_readln(buffer, NULL, EOL_STYLE_ANY).

   @param buffer the evbuffer to read from
   @return pointer to a single line, or NULL if an error occurred
*/
char *evbuffer_readline(struct evbuffer *buffer);

/**
  Move data from one evbuffer into another evbuffer.

  This is a destructive add.  The data from one buffer moves into
  the other buffer. However, no memory copies occur.

  @param outbuf the output buffer
  @param inbuf the input buffer
  @return 0 if successful, or -1 if an error occurred
 */
int evbuffer_add_buffer(struct evbuffer *outbuf, struct evbuffer *inbuf);


/**
  Append a formatted string to the end of an evbuffer.

  @param buf the evbuffer that will be appended to
  @param fmt a format string
  @param ... arguments that will be passed to printf(3)
  @return The number of bytes added if successful, or -1 if an error occurred.

 */
int evbuffer_add_printf(struct evbuffer *buf, const char *fmt, ...)
#ifdef __GNUC__
  __attribute__((format(printf, 2, 3)))
#endif
;


/**
  Append a va_list formatted string to the end of an evbuffer.

  @param buf the evbuffer that will be appended to
  @param fmt a format string
  @param ap a varargs va_list argument array that will be passed to vprintf(3)
  @return The number of bytes added if successful, or -1 if an error occurred.
 */
int evbuffer_add_vprintf(struct evbuffer *buf, const char *fmt, va_list ap);


/**
  Remove a specified number of bytes data from the beginning of an evbuffer.

  @param buf the evbuffer to be drained
  @param len the number of bytes to drain from the beginning of the buffer
  @return 0 if successful, or -1 if an error occurred
 */
void evbuffer_drain(struct evbuffer *buf, size_t len);


/**
  Write the contents of an evbuffer to a file descriptor.

  The evbuffer will be drained after the bytes have been successfully written.

  @param buffer the evbuffer to be written and drained
  @param fd the file descriptor to be written to
  @return the number of bytes written, or -1 if an error occurred
  @see evbuffer_read()
 */
int evbuffer_write(struct evbuffer *buffer, evutil_socket_t fd);


/**
  Read from a file descriptor and store the result in an evbuffer.

  @param buf the evbuffer to store the result
  @param fd the file descriptor to read from
  @param howmuch the number of bytes to be read
  @return the number of bytes read, or -1 if an error occurred
  @see evbuffer_write()
 */
int evbuffer_read(struct evbuffer *buffer, evutil_socket_t fd, int howmuch);


/**
  Find a string within an evbuffer.

  @param buffer the evbuffer to be searched
  @param what the string to be searched for
  @param len the length of the search string
  @return a pointer to the beginning of the search string, or NULL if the search failed.
 */
unsigned char *evbuffer_find(struct evbuffer *buffer, const unsigned char *what, size_t len);

/**
  Set a callback to invoke when the evbuffer is modified.

  @param buffer the evbuffer to be monitored
  @param cb the callback function to invoke when the evbuffer is modified
  @param cbarg an argument to be provided to the callback function
 */
void evbuffer_setcb(struct evbuffer *buffer,
    void (*cb)(struct evbuffer *, size_t, size_t, void *), void *cbarg);

/**
  Makes the memory at the begging of an evbuffer contiguous

  @param buf the evbuffer to make contiguous
  @param size the number of bytes to make contiguous, or -1 to make the
         entire buffer contiguous.
  @return a pointer to the contigous memory areay
*/

unsigned char *evbuffer_pullup(struct evbuffer *buf, int size);

/**
  Prepends data to the beginning of the evbuffer

  @param buf the evbuffer to which to prepend data
  @param data a pointer to the memory to prepend
  @param size the number of bytes to prepend
  @return 0 if successful, or -1 otherwise
*/

int evbuffer_prepend(struct evbuffer *buf, const void *data, size_t size);

/**
  Prepends the src evbuffer to the beginning of the dst evbuffer

  @param dst the evbuffer to which to prepend data
  @param src the evbuffer to prepend; it will be emptied as a result
*/

void evbuffer_prepend_buffer(struct evbuffer *dst, struct evbuffer* src);

/* XXX missing APIs:

    A better find-string that returns a smart offset structure rather than a
    pointer. It should also be able to start searching _at_ an offset.

	A variant of evbuffer_write() that takes a maximum number of bytes to
	write.

	A check-representation functions for testing, so we can assert() that
	nothing has gone screwy inside an evbuffer.
*/

/** deprecated in favor of calling the functions directly */
#define EVBUFFER_LENGTH(x)	evbuffer_get_length(x)
#define EVBUFFER_DATA(x)	evbuffer_pullup(x, -1)

#ifdef __cplusplus
}
#endif

#endif /* _EVENT2_BUFFER_H_ */
