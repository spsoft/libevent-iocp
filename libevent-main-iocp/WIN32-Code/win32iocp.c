/*
 * win32iocp.c
 *
 * Integerate Windows I/O Completion Port into Libevent.
 *
 * Submitted by Stephen Liu <stephen.nil@gmail.com>
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

#ifdef _MSC_VER
#include "./config.h"
#else
/* Avoid the windows/msvc thing. */
#include "../config.h"
#endif

#ifndef _EVENT_NOIOCP

#include <winsock2.h>
#include <mswsock.h>
#include <windows.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "event.h"
#include "event-internal.h"
#include "evsignal.h"
#include "log.h"
#include "tree.h"

struct win32iocp_event {
	OVERLAPPED overlapped;

	struct event * ev;
	short event;

	struct win32iocp_event * next;
};

struct win32iocp_entry {
	RB_ENTRY(win32iocp_entry) node;
	SOCKET sock;
	char nread, nwrite;
};

static int
compare(struct win32iocp_entry *a, struct win32iocp_entry *b)
{
	if (a->sock < b->sock)
		return -1;
	else if (a->sock > b->sock)
		return 1;
	else
		return 0;
}

struct win32iocp_op {
	struct win32iocp_event * freelist;
	int nfreecount;

	HANDLE iocp;

	/* objects[0] for iocp operations, object[1..63] for accept */
	HANDLE objects[MAXIMUM_WAIT_OBJECTS];
	struct event * accepts[MAXIMUM_WAIT_OBJECTS];

	/* keep persist events between two win32iocp_dispatch call */
	struct win32iocp_event * persist;
	int npersist;

	RB_HEAD(win32iocp_map, win32iocp_entry) event_root;
};

RB_PROTOTYPE(win32iocp_map, win32iocp_entry, node, compare);
RB_GENERATE(win32iocp_map, win32iocp_entry, node, compare);

static void *win32iocp_init	(struct event_base *);
static int win32iocp_add	(void *, struct event *);
static int win32iocp_del	(void *, struct event *);
static int win32iocp_dispatch	(struct event_base *, void *, struct timeval *);
static void win32iocp_dealloc	(struct event_base *, void *);

static struct win32iocp_event * win32iocp_event_new (struct win32iocp_op *);
static void win32iocp_event_free (struct win32iocp_op *, struct win32iocp_event *);

static int win32iocp_accept_add (struct win32iocp_op *, struct event *);
static int win32iocp_accept_del (struct win32iocp_op *, struct event *);

static int win32iocp_event_loop (struct win32iocp_op *);
static int win32iocp_event_active (struct win32iocp_op * win32iocp_op,
		int fd, struct win32iocp_event * event);

static void win32iocp_persist_add (struct win32iocp_op *, struct win32iocp_event *);
static int win32iocp_persist_del (struct win32iocp_op *, int, short);
static int win32iocp_persist_dispatch (struct win32iocp_op *);

struct eventop win32iocpops = {
	"win32iocp",
	win32iocp_init,
	win32iocp_add,
	win32iocp_del,
	win32iocp_dispatch,
	win32iocp_dealloc,
	1 /* need reinit */
};

#define NEVENT	128000

static const char *
win32iocp_strerror( DWORD lastError, char * errmsg, size_t len )
{
	if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, lastError, 0,
			errmsg, len - 1, NULL)) {
		/* if we fail, call ourself to find out why and return that error */
		return win32iocp_strerror( GetLastError(), errmsg, len );  
	}

	return errmsg;
}

static struct win32iocp_entry *
win32iocp_get_entry(struct win32iocp_op *op, SOCKET s, int create)
{
	struct win32iocp_entry key, *val;
	key.sock = s;
	val = RB_FIND(win32iocp_map, &op->event_root, &key);
	if (val || !create)
		return val;
	if (!(val = calloc(1, sizeof(struct win32iocp_entry)))) {
		event_warn("win32iocp_get_entry: out of memory");
		return NULL;
	}
	val->sock = s;
	RB_INSERT(win32iocp_map, &op->event_root, val);
	return val;
}

static struct win32iocp_event *
win32iocp_event_new (struct win32iocp_op * op)
{
	struct win32iocp_event * event = NULL;
	if (NULL != op->freelist) {
		event = op->freelist;
		op->freelist = event->next;
		event->next = NULL;
		-- op->nfreecount;
	} else {
		event =  calloc( 1, sizeof (struct win32iocp_event) );
	}

	return event;
}

static void
win32iocp_event_free (struct win32iocp_op * op, struct win32iocp_event * event)
{
	if (op->nfreecount < 512) {
		memset (event, 0, sizeof (struct win32iocp_event) );
		event->next = op->freelist;
		op->freelist = event;
		++ op->nfreecount;
	} else {
		free( event );
	}
}

int
win32iocp_accept_add (struct win32iocp_op * op, struct event * event)
{
	int i = 0, index = -1, avail = -1;

	for (i = 1; i < MAXIMUM_WAIT_OBJECTS && index < 0; i++) {
		struct event * iter = op->accepts[i];
		if (iter == event) index = i;
		if (NULL != iter && iter->ev_fd == event->ev_fd) index = i;
		if (NULL == iter && avail < 0) avail = i;
	}

	if (index > 0) {
		op->accepts[index] = event;
	} else {
		if (avail > 0) {
			op->accepts[avail] = event;
			WSAEventSelect( event->ev_fd, op->objects[avail], FD_ACCEPT );
		}
	}

	return ( index < 0 && avail < 0 ) ? -1 : 0;
}

int
win32iocp_accept_del (struct win32iocp_op * op, struct event * event)
{
	int i = 0, index = -1;

	for (i = 1; i < MAXIMUM_WAIT_OBJECTS && index < 0; i++) {
		struct event * iter = op->accepts[i];
		if (iter == event) index = i;
		if (NULL != iter && iter->ev_fd == event->ev_fd) index = i;
	}

	if (index > 0) {
		WSAEventSelect( event->ev_fd, op->objects[index], 0 );
		op->accepts[index] = NULL;
	}

	return index > 0 ? -1 : 0;
}

void
win32iocp_persist_add (struct win32iocp_op * op, struct win32iocp_event * event)
{
	event->next = op->persist;
	op->persist = event;
	op->npersist++;
}

int
win32iocp_persist_del (struct win32iocp_op * op, int fd, short event)
{
	struct win32iocp_event ** curr = &( op->persist );
	for ( ; *curr; curr = &((*curr)->next) ) {
		if ((*curr)->ev->ev_fd == fd && (*curr)->event == event ) {
			struct win32iocp_event * todel = *curr;
			*curr = todel->next;
			win32iocp_event_free( op, todel );
			op->npersist--;
			return 0;
		}
	}

	return -1;
}

/* 0 : OK, -1 : has error and the error events have been actived */
int
win32iocp_persist_dispatch (struct win32iocp_op * op)
{
	int ret = 0, fd = -1;
	struct win32iocp_entry * entry = NULL;
	struct win32iocp_event * curr = NULL, * errorlist = NULL, * retrylist = NULL;

	for ( ; op->persist; ) {
		curr = op->persist;
		op->persist = curr->next;
		--op->npersist;

		fd = curr->ev->ev_fd;
		entry = win32iocp_get_entry (op, fd, 0);

		if (NULL == entry) {
			event_warn("win32iocp_persist_dispatch: cannot found fd(%d), dangerous", fd);
			curr->next = errorlist;
			errorlist = curr;
			continue;
		}

		if (EV_READ == curr->event) {
			DWORD recvBytes = 0;
			memset( &( curr->overlapped ), 0, sizeof( OVERLAPPED ) );
			curr->overlapped.hEvent = op->objects[0];

			if( FALSE == ReadFile( (HANDLE)fd, NULL, 0, &recvBytes, &( curr->overlapped ) ) ) {
				if( ERROR_IO_PENDING != WSAGetLastError() ) {
					event_warn("win32iocp_persist_dispatch: ReadFile failed, errno %d", WSAGetLastError());
					if (WSAENOBUFS == WSAGetLastError()) {
						/* recoverable error, left it for next loop */
						curr->next = retrylist;
						retrylist = curr;
					} else {
						curr->next = errorlist;
						errorlist = curr;
					}
					continue;
				}
			}
			entry->nread = 1;
		}

		if (EV_WRITE == curr->event) {
			DWORD sendBytes = 0;
			memset( &( curr->overlapped ), 0, sizeof( OVERLAPPED ) );
			curr->overlapped.hEvent = op->objects[0];

			if( FALSE == WriteFile( (HANDLE)fd, NULL, 0, &sendBytes, &( curr->overlapped ) ) ) {
				if( ERROR_IO_PENDING != WSAGetLastError() ) {
					event_warn("win32iocp_persist_dispatch: WriteFile failed, errno %d", WSAGetLastError());
					if (WSAENOBUFS == WSAGetLastError()) {
						/* recoverable error, left it for next loop */
						curr->next = retrylist;
						retrylist = curr;
					} else {
						curr->next = errorlist;
						errorlist = curr;
					}
					continue;
				}
			}
			entry->nwrite = 1;
		}
	}

	op->persist = retrylist;
	op->npersist = 0;
	for( curr = op->persist; curr; curr = curr->next)
		++op->npersist;

	for( curr = errorlist; curr; ) {
		event_debug (("win32iocp_persist_dispatch: active error event, fd %d, ev %p, event %d",
				curr->ev->ev_fd, curr->ev, curr->event));

		errorlist = curr->next;
		event_active (curr->ev, curr->event, 1);
		win32iocp_event_free (op, curr);
		curr = errorlist;
		ret = -1;
	}

	return ret;
}

static void *
win32iocp_init(struct event_base *base)
{
	HANDLE iocp;
	int nfiles = NEVENT, i = 0;
	struct win32iocp_op * win32iocp_op;

	/* Disable win32 iocp when this environment variable is set */
	if (getenv("EVENT_NOWIN32IOCP"))
		return (NULL);

	/* Initalize the kernel queue */
	iocp = CreateIoCompletionPort( INVALID_HANDLE_VALUE, NULL, 0, 0 );
	if (NULL == iocp) {
		event_warn("CreateIoCompletionPort");
		return (NULL);
	}

	if (!(win32iocp_op = calloc(1, sizeof(struct win32iocp_op)))) {
		event_warn("calloc");
		CloseHandle( iocp );
		return( NULL );
	}

	win32iocp_op->iocp = iocp;

	/* Initalize fields */
	RB_INIT(&win32iocp_op->event_root);

	for (i = 0; i < MAXIMUM_WAIT_OBJECTS; i++ ) {
		win32iocp_op->objects[i] = CreateEvent( NULL, FALSE, FALSE, NULL );
	}

	evsignal_init(base);

	return (win32iocp_op);
}

/* 0 : not active event, 1 : has active event */
static int
win32iocp_event_active (struct win32iocp_op * win32iocp_op,
		int fd, struct win32iocp_event * event)
{
	int count = 0;
	struct win32iocp_entry * entry = NULL;

	entry = win32iocp_get_entry (win32iocp_op, fd, 0);
	if (NULL == entry) {
		event_warn("win32iocp_event_active: cannot found fd(%d), dangerous", fd);
		win32iocp_event_free (win32iocp_op, event);
		return (count);
	}

	if (EV_READ == event->event && entry->nread) {
		++count;
		event_active (event->ev, EV_READ, 1);
		if (EV_PERSIST & event->ev->ev_events) {
			win32iocp_persist_add (win32iocp_op, event);
		} else {
			entry->nread = 0;
		}
	}
	if (EV_WRITE == event->event && entry->nwrite) {
		++count;
		event_active (event->ev, EV_WRITE, 1);
		if (EV_PERSIST & event->ev->ev_events) {
			win32iocp_persist_add (win32iocp_op, event);
		} else {
			entry->nwrite = 0;
		}
	}

	/* this event has been canceled by calling event_del */
	if (0 == count) {
		event_debug (("win32iocp_event_active: ignore pending operation, fd %d, ev %p, event %d",
				fd, event->ev, event->event));
	}

	if (0 == count || (! (EV_PERSIST & event->ev->ev_events) ))
		win32iocp_event_free (win32iocp_op, event);

	return count;
}

/* -1 : Fail, >= 0 : count of event */
static int
win32iocp_event_loop(struct win32iocp_op * win32iocp_op)
{
	int count = 0;

	for ( ; ; ) {
		BOOL isSuccess = FALSE;
		DWORD bytesTransferred = 0;
		DWORD iocpKey = 0;
		struct win32iocp_event * event = NULL;
		DWORD lastError = 0;

		isSuccess = GetQueuedCompletionStatus( win32iocp_op->iocp, &bytesTransferred,
				(DWORD*)&iocpKey, (OVERLAPPED**)&event, 0 );
		lastError = WSAGetLastError();

		if (!isSuccess) {
			if (NULL != event) {
				if (ERROR_NETNAME_DELETED != lastError) // client abort
					event_warn ("GetQueuedCompletionStatus failed, errno %d", lastError);
				if (win32iocp_event_active (win32iocp_op, iocpKey, event) > 0) ++count;
			} else {
				if (WAIT_TIMEOUT == lastError) {
					// time-out while waiting for completed I/O request, ignore
				} else {
					event_warn ("GetQueuedCompletionStatus failed, errno %d", lastError);
					return -1;
				}
			}
			return (count);
		}

		if (NULL == event) {
			event_warn("event is null, dangerous");
			return (count);
		}

		if (win32iocp_event_active (win32iocp_op, iocpKey, event) > 0) ++count;
	}

	return count;
}

static int
win32iocp_dispatch(struct event_base *base, void *arg, struct timeval *tv)
{
	struct win32iocp_op * win32iocp_op = arg;
	struct win32iocp_entry * entry = NULL;
	int timeout = INFINITE, index = 0, count = 0;

	if (0 != win32iocp_persist_dispatch (win32iocp_op) ) {
		/* has error, and the error events have been actived */
		return 0;
	}

	/* deal with the race condition of one event for multiple OVERLAPPED operations */
	count = win32iocp_event_loop (win32iocp_op);
	if (0 != count) return (count > 0 ? 0 : -1);

	if (tv != NULL)
		timeout = tv->tv_sec * 1000 + (tv->tv_usec + 999) / 1000;

	index = WSAWaitForMultipleEvents( MAXIMUM_WAIT_OBJECTS,
			win32iocp_op->objects, FALSE, timeout, FALSE );

	if (WAIT_FAILED == index || WAIT_TIMEOUT == index) {
		if (WAIT_FAILED == index) event_warn("WSAWaitForMultipleEvents");
		return (0);
	}

	index = index - WSA_WAIT_EVENT_0;

	/* accept event */
	if (index > 0 ) {
		struct event * event = win32iocp_op->accepts[index];
		if (NULL != event) {
			event_active (event, EV_READ, 1);

			/* if not persist accept event, then clear it */
			if( ! (EV_PERSIST & event->ev_events) ) {
				WSAEventSelect (event->ev_fd, win32iocp_op->objects[index], 0);
				win32iocp_op->accepts[index] = NULL;
			}
		}
		return 0;
	}

	/* iocp event */
	if (0 == index ) count = win32iocp_event_loop (win32iocp_op);

	return (count < 0 ? -1 : 0);
}

static int
win32iocp_add(void *arg, struct event *ev)
{
	struct win32iocp_op * win32iocp_op = arg;
	struct win32iocp_entry * entry = NULL;
	int fd = 0;

	if (ev->ev_events & EV_SIGNAL)
		return (evsignal_add(ev));

	fd = ev->ev_fd;

	/* Update events responsible */
	if (ev->ev_events & EV_READ) {
		int optval = 0, optlen = sizeof( int );
		if (SOCKET_ERROR != getsockopt( fd, SOL_SOCKET, SO_ACCEPTCONN, (char*)&optval, &optlen )) {
			if( optval ) {
				event_debug(("win32iocp_add: socket(%d) for accept",fd));
				return win32iocp_accept_add( win32iocp_op, ev );
			}
		} else {
			event_debug(("win32iocp_add: getsockopt fail, errno %d", WSAGetLastError()));
		}
	}

	if (NULL == (entry = win32iocp_get_entry (win32iocp_op, fd, 1)))
		return -1;

	if (0 == entry->nread && 0 == entry->nwrite) {
		if (NULL == CreateIoCompletionPort ((HANDLE)fd, win32iocp_op->iocp, fd, 0) ) {
			/* if ERROR_INVALID_PARAMETER, then this handle was already registered. */
			if (WSAGetLastError() != ERROR_INVALID_PARAMETER) {
				event_warn("win32iocp_add: CreateIoCompletionPort");
				return (-1);
			}
		}
	}

	if (ev->ev_events & EV_READ) {
		win32iocp_persist_del (win32iocp_op, ev->ev_fd, EV_READ);

		if (0 == entry->nread) {
			DWORD recvBytes = 0, flags = 0;
			struct win32iocp_event * event = win32iocp_event_new( win32iocp_op );
			if (NULL == event) {
				event_warn("win32iocp_add: out of memory");
				return (-1);
			}

			event->event = EV_READ;
			memset( &( event->overlapped ), 0, sizeof( OVERLAPPED ) );
			event->overlapped.hEvent = win32iocp_op->objects[0];
			event->ev = ev;

			if( FALSE == ReadFile( (HANDLE)fd, NULL, 0, &recvBytes, &( event->overlapped ) ) ) {
				if( ERROR_IO_PENDING != WSAGetLastError() ) {
					win32iocp_event_free (win32iocp_op, event);
					event_warn("win32iocp_add: ReadFile failed, errno %d", WSAGetLastError());
					return (-1);
				} 
			}
			event_debug(("win32iocp_add: register EV_READ for %d", fd));
			entry->nread = 1;
		} else {
			event_debug(("win32iocp_add: readd nonpersist event"));
			return (-1);
		}
	}

	if (ev->ev_events & EV_WRITE) {
		win32iocp_persist_del (win32iocp_op, ev->ev_fd, EV_READ);

		if (0 == entry->nwrite) {
			DWORD sendBytes = 0;
			struct win32iocp_event * event = win32iocp_event_new( win32iocp_op );
			if (NULL == event) {
				event_warn("win32iocp_add: out of memory");
				return (-1);
			}

			event->event = EV_WRITE;
			memset( &( event->overlapped ), 0, sizeof( OVERLAPPED ) );
			event->overlapped.hEvent = win32iocp_op->objects[0];
			event->ev = ev;

			if( FALSE == WriteFile( (HANDLE)fd, NULL, 0, &sendBytes, &( event->overlapped ) ) ) {
				if( ERROR_IO_PENDING != WSAGetLastError() ) {
					win32iocp_event_free (win32iocp_op, event);
					event_warn("win32iocp_add: WriteFile failed, errno %d", WSAGetLastError());
					return (-1);
				}
			}
			event_debug(("win32iocp_add: register EV_WRITE for %d", fd));
			entry->nwrite = 1;
		} else {
			event_debug(("win32iocp_add: readd nonpersist event"));
			return (-1);
		}
	}

	return (0);
}

static int
win32iocp_del(void *arg, struct event *ev)
{
	struct win32iocp_op *win32iocp_op = arg;
	struct win32iocp_entry * entry = NULL;
	int fd = 0;

	if (ev->ev_events & EV_SIGNAL)
		return (evsignal_del(ev));

	if (ev->ev_events & EV_READ) {
		int optval = 0, optlen = sizeof( int );
		if (SOCKET_ERROR != getsockopt( ev->ev_fd, SOL_SOCKET, SO_ACCEPTCONN, (char*)&optval, &optlen )) {
			if( optval ) return win32iocp_accept_del( win32iocp_op, ev );
		}
	}

	fd = ev->ev_fd;
	entry = win32iocp_get_entry (win32iocp_op, fd, 0);
	if (NULL == entry)
		return (0);

	if (ev->ev_events & EV_READ) {
		win32iocp_persist_del (win32iocp_op, ev->ev_fd, EV_READ);
		entry->nread = 0; 
		event_debug(("win32iocp_del: unregister EV_READ for %d", fd));
	}
	if (ev->ev_events & EV_WRITE) {
		win32iocp_persist_del (win32iocp_op, ev->ev_fd, EV_WRITE);
		entry->nwrite = 0;
		event_debug(("win32iocp_del: unregister EV_WRITE for %d", fd));
	}

	if ( ( !entry->nread ) && ( !entry->nwrite ) ) {
		RB_REMOVE(win32iocp_map, &win32iocp_op->event_root, entry);
		free (entry);
	}

	return (0);
}

static void
win32iocp_dealloc(struct event_base *base, void *arg)
{
	struct win32iocp_op * win32iocp_op = arg;
	struct win32iocp_event * event = NULL, * next = NULL;
	int i = 0;

	evsignal_dealloc(base);

	for (event = win32iocp_op->freelist; NULL != event; ) {
		next = event->next;
		free( event );
		event = next;
	}

	for (event = win32iocp_op->persist; NULL != event; ) {
		next = event->next;
		free( event );
		event = next;
	}

	for (i = 0; i < MAXIMUM_WAIT_OBJECTS; i++) {
		struct event * ev = win32iocp_op->accepts[i];
		if (NULL != ev) {
			WSAEventSelect (ev->ev_fd, win32iocp_op->objects[i], 0);
		}
		CloseHandle( win32iocp_op->objects[i] );
	}

	if (NULL != win32iocp_op->iocp)
		CloseHandle(win32iocp_op->iocp);

	memset(win32iocp_op, 0, sizeof(struct win32iocp_op));
	free(win32iocp_op);
}

#endif
