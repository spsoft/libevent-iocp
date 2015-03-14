/*
 * Copyright (c) 2008 Niels Provos <provos@citi.umich.edu>
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
#ifndef _EVENT2_THREAD_H_
#define _EVENT2_THREAD_H_

/** @file thread.h
    
  Functions for multi-threaded applications using libevent.

  When using a multi-threaded application in which multiple threads
  add and delete events from a single event base, libevent needs to
  lock its data structures.

  A multi-threaded application must provide locking functions to
  libevent via evthread_set_locking_callback().  Libevent will invoke
  this callback whenever a lock needs to be acquired or released.

  The total number of locks employed by libevent can be determined
  via the evthread_num_locks() function.  An application must provision
  that many locks.

  If the owner of an event base is waiting for events to happen,
  libevent may signal the thread via a special file descriptor to wake
  up.   To enable this feature, an application needs to provide a
  thread identity function via evthread_set_id_callback().

 */

#ifdef __cplusplus
extern "C" {
#endif

#include <event-config.h>

/* combine (lock|unlock) with (read|write) */
#define EVTHREAD_LOCK	0x01
#define EVTHREAD_UNLOCK	0x02
#define EVTHREAD_WRITE	0x04
#define EVTHREAD_READ	0x08

/**
   Sets the functions libevent should use for allocating and freeing
   locks.  This needs to be called in addition to
   evthread_set_locking_callback() before using libevent in a
   multi-threaded application.

   @param alloc_fn function to be called when allocating a new lock
   @param free_fn function to be called to a free a lock
*/
void evthread_set_lock_create_callbacks(struct event_base *base,
    void *(*alloc_fn)(void), void (*free_fn)(void *));

struct event_base;
/**
   Sets the function libevent should use for locking.

   @param base the event base for which the locking function should be set
   @param locking_fn the function that libevent should invoke to acquire
     or release a lock.  mode has either EVTHREAD_LOCK or EVTHREAD_UNLOCK
     set, and in addition, either EVHTREAD_WRITE or EVTREAD_READ.
 */
void evthread_set_locking_callback(struct event_base *base,
    void (*locking_fn)(int mode, void *lock));

/**
   Sets the function for derminting the thread id.

   @param base the event base for which to set the id function
   @param id_fn the identify function libevent should invoke to
     determine the identity of a thread.
*/
void evthread_set_id_callback(struct event_base *base,
    unsigned long (*id_fn)(void));

#ifdef __cplusplus
}
#endif

#endif /* _EVENT2_THREAD_H_ */
