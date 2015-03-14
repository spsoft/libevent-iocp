/*
 * libevent non block connect example.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <winsock2.h>
#include <mswsock.h>
#include <windows.h>
#endif

#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <event.h>
#include <log.h>
#include <evutil.h>

/**
 * A struct for client specific data, in this simple case the only
 * client specific data is the read event.
 */
struct client {
	struct event ev_read;
	struct event ev_write;
	char buffer[ 8192 ];
	int total, wlen;
};

typedef int evutil_socket_t;

int is_try_again()
{
#ifdef WIN32
	return WSAEWOULDBLOCK == WSAGetLastError();
#else
	return EAGAIN == errno;
#endif
}

void
close_client(struct client *client, int fd)
{
	EVUTIL_CLOSESOCKET(fd);
	free(client);
}

/**
 * This function will be called by libevent when the client socket is
 * ready for reading.
 */
void
on_read(evutil_socket_t fd, short ev, void *arg)
{
	struct client *client = (struct client *)arg;
	int len;
	
	len = recv(fd, client->buffer, sizeof( client->buffer ) - 1, 0);
	if (len == 0) {
		printf( "%s\n", client->buffer );
		printf("Client(%d) disconnected.\n",fd);
		close_client(client, fd);
		return;
	}
	else if (len < 0 && ( ! is_try_again() ) ) {
		printf("recv(%d) fail, len %d, errno %d\n", fd, len, EVUTIL_SOCKET_ERROR() );

	}
	
	if( len > 0 ) {
		client->total = len;
		client->wlen = 0;
		if(0 != event_add( &( client->ev_write ), NULL ) ) {
			close_client( client, fd );
		}
	} else {
		if( 0!= event_add( &( client->ev_read ), NULL ) ) {
			close_client( client, fd );
		}
	}
}

/**
 * This function will be called by libevent when the client socket is
 * ready for writing.
 */
void
on_write(evutil_socket_t fd, short ev, void *arg)
{
	struct client *client = (struct client *)arg;
	int len;
	
	len = send(fd, client->buffer + client->wlen, client->total - client->wlen, 0);
	if (len == 0) {
		printf("Client(%d) disconnected.\n",fd);
		close_client(client, fd);
		return;
	}
	else if (len < 0 && ( ! is_try_again() ) ) {
		printf("send(%d) fail, len %d, errno %d\n", fd, len, EVUTIL_SOCKET_ERROR() );
		close_client( client, fd );
		return;
	}

	if( len > 0 ) client->wlen += len;
	
	if( client->wlen >= client->total ) {
		client->buffer[ client->total ] = '\0';
		if( NULL != strstr( client->buffer, "quit" ) ) {
			//printf("Client(%d) quit.\n",fd);
			shutdown( fd, SD_BOTH );
			close_client( client, fd );
			return;
		}
		if( 0 != event_add( &( client->ev_read ), NULL ) ) {
			close_client( client, fd );
		}
	} else {
		if( 0 != event_add( &( client->ev_write ), NULL ) ) {
			close_client( client, fd );
		}
	}
}

/**
 * This function will be called by libevent when the connection is connected.
 */
void
on_connect(evutil_socket_t fd, short ev, void *arg)
{
	struct client *client = (struct client*)arg;
	
	if (0 != ev) {
		event_set(&client->ev_write, fd, EV_WRITE, on_write, client );
	
		if( 0 != event_add(&client->ev_write, NULL) ) {
			close_client( client, fd );
		}
	} else {
		close_client( client, fd );
	}
}

int
main(int argc, char **argv)
{
	evutil_socket_t conn_fd;
	struct sockaddr_in conn_addr;
	
	struct event_base * evbase = NULL;
	struct client *client = NULL;
	int ret = 0;
	
#ifdef WIN32
	WSADATA wsaData;
	
	int err = WSAStartup( MAKEWORD( 2, 0 ), &wsaData );
	if ( err != 0 ) {
		printf( "Couldn't find a useable winsock.dll.\n" );
		return -1;
	}
#endif

	/* Initialize libevent. */
	evbase = event_init();
	printf( "method : %s\n", event_base_get_method (evbase) );
	
	conn_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (conn_fd < 0)
		event_err(1, "socket failed");

	/* connect to google.com */
	memset(&conn_addr, 0, sizeof(conn_addr));
	conn_addr.sin_family = AF_INET;
	conn_addr.sin_addr.s_addr = inet_addr( "72.14.207.99" );
	conn_addr.sin_port = htons(80);

    /* Set the socket to non-blocking, this is essential in event
	 * based programming with libevent. */
	if (evutil_make_socket_nonblocking(conn_fd) < 0)
		event_err(1, "failed to set client socket to non-blocking");

	ret = connect( conn_fd, (struct sockaddr*)&conn_addr, sizeof( struct sockaddr_in ) );

	client = (struct client*)calloc(1, sizeof(*client));
	strncpy( client->buffer,
		"GET / HTTP/1.0\r\nHost: google.com\r\nConnection: Close\r\n\r\n",
		sizeof( client->buffer ) );
	client->total = strlen( client->buffer );
	event_set(&client->ev_read, conn_fd, EV_READ, on_read, client );

	if (0 == ret) {
		event_set( &(client->ev_write), conn_fd, EV_WRITE, on_write, client );
		event_add( &(client->ev_write), NULL );
	} else if( is_try_again() ) {
		event_set( &(client->ev_write), conn_fd, EV_CONNECT | EV_WRITE, on_connect, client );
		event_add( &(client->ev_write), NULL );
	} else {
		close_client (client, conn_fd);
		printf( "connect fail, ret %d, errno %d\n", ret, WSAGetLastError() );
	}
	
	/* Start the libevent event loop. */
	event_dispatch();
	
#ifdef WIN32
	WSACleanup();
#endif

	return 0;
}
