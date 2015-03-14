/*
 * Compile with:
 * cc -I/usr/local/include -o event-test event-test.c -L/usr/local/lib -levent
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#ifndef WIN32
#include <sys/queue.h>
#include <unistd.h>
#include <sys/time.h>
#else
#include <windows.h>
#include <io.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <event.h>

static const char * win32iocp_strerror( DWORD lastError, char * errmsg, size_t len )
{
	if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, lastError, 0,
			errmsg, len - 1, NULL)) {
		/* if we fail, call ourself to find out why and return that error */
		return win32iocp_strerror( GetLastError(), errmsg, len );  
	}

	return errmsg;
}

int ev_readfile( int fd, char * buf, int buflen )
{
	DWORD dwBytesRead = 0;
	OVERLAPPED overlap;
	BOOL ret = FALSE;

	static HANDLE auxEvent = 0;

	if( 0 == auxEvent ) {
		/* one event for every operation */
		auxEvent = CreateEvent( NULL, TRUE, FALSE, NULL );
	}

	memset(&overlap, 0, sizeof(overlap));

	/* ReadFile : If lpOverlapped is not NULL, the read operation starts at 
	   the offset that is specified in the OVERLAPPED structure and ReadFile 
	   does not return until the read operation is complete. The system 
	   updates the OVERLAPPED offset before ReadFile returns. */
	overlap.Offset = SetFilePointer( (HANDLE)fd, 0, 0, FILE_CURRENT );

	/* http://msdn.microsoft.com/en-us/library/aa364986(VS.85).aspx */
	/* Even if you have passed the function a file handle associated with a 
	   completion port and a valid OVERLAPPED structure, an application can 
	   prevent completion port notification. This is done by specifying a valid 
	   event handle for the hEvent member of the OVERLAPPED structure, and 
	   setting its low-order bit. A valid event handle whose low-order bit is 
	   set keeps I/O completion from being queued to the completion port.
	*/
	overlap.hEvent = (HANDLE)( (DWORD)auxEvent | 0x1 );

	/* A pointer to an OVERLAPPED structure is required if the hFile parameter 
	   was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL. */
	ret = ReadFile((HANDLE)fd, buf, buflen, &dwBytesRead, &overlap);

	if( ret ) ret = GetOverlappedResult( (HANDLE)fd, &overlap, &dwBytesRead, TRUE );

	/* Check for end of file. */
	if( FALSE == ret && dwBytesRead == 0) return 0;

	/* system will not update the offset when using Overlapped I/O, we update it here */
	SetFilePointer( (HANDLE)fd, dwBytesRead, 0, FILE_CURRENT );

	return dwBytesRead;
}

static void
fifo_read(int fd, short event, void *arg)
{
	char buf[255], errmsg[ 256 ];
	int len = 0;
	struct event *ev = arg;

	/* Reschedule this event */
	event_add(ev, NULL);

	fprintf(stderr, "fifo_read called with fd: %d, event: %d, arg: %p\n",
		fd, event, arg);
#ifdef WIN32

	len = ev_readfile( fd, buf, sizeof( buf ) - 1 );
	if( len <= 0 )
	{
		win32iocp_strerror( GetLastError(), errmsg, sizeof( errmsg ) );
		fprintf(stderr, "End Of File, errno %d, %s", GetLastError(), errmsg );
		event_del( ev );
		return;
	}

	buf[len] = '\0';
#else
	len = read(fd, buf, sizeof(buf) - 1);

	if (len == -1) {
		perror("read");
		return;
	} else if (len == 0) {
		fprintf(stderr, "Connection closed\n");
		return;
	}

	buf[len] = '\0';
#endif
	fprintf(stdout, "Read: %s\n", buf);
}

int
main (int argc, char **argv)
{
	struct event evfifo;
#ifdef WIN32
	WSADATA wsaData;
	HANDLE socket;
	
	int err = WSAStartup( MAKEWORD( 2, 0 ), &wsaData );
	if ( err != 0 ) {
		printf( "Couldn't find a useable winsock.dll.\n" );
		return -1;
	}

	// Open a file. 
	socket = CreateFile("test.txt",       // open File 
			GENERIC_READ,                 // open for reading 
			0,                            // do not share 
			NULL,                         // no security 
			OPEN_EXISTING,                // existing file only 
			FILE_ATTRIBUTE_NORMAL |       // normal file
			FILE_FLAG_OVERLAPPED,         // asynchronous I/O
			NULL);                        // no attr. template 

	if(socket == INVALID_HANDLE_VALUE)
		return 1;

#else
	struct stat st;
	const char *fifo = "event.fifo";
	int socket;
 
	if (lstat (fifo, &st) == 0) {
		if ((st.st_mode & S_IFMT) == S_IFREG) {
			errno = EEXIST;
			perror("lstat");
			exit (1);
		}
	}

	unlink (fifo);
	if (mkfifo (fifo, 0600) == -1) {
		perror("mkfifo");
		exit (1);
	}

	/* Linux pipes are broken, we need O_RDWR instead of O_RDONLY */
#ifdef __linux
	socket = open (fifo, O_RDWR | O_NONBLOCK, 0);
#else
	socket = open (fifo, O_RDONLY | O_NONBLOCK, 0);
#endif

	if (socket == -1) {
		perror("open");
		exit (1);
	}

	fprintf(stderr, "Write data to %s\n", fifo);
#endif
	/* Initalize the event library */
	event_init();

	/* Initalize one event */
#ifdef WIN32
	event_set(&evfifo, (int)socket, EV_READ, fifo_read, &evfifo);
#else
	event_set(&evfifo.ev, socket, EV_READ, fifo_read, &evfifo);
#endif

	/* Add it to the active events, without a timeout */
	event_add(&evfifo, NULL);
	
	event_dispatch();

#ifdef WIN32
	CloseHandle(socket);
	WSACleanup();
#endif
	return (0);
}

