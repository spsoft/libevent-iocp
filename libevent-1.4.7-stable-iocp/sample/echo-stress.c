/*
 * Submitted by Stephen Liu <stephen.nil@gmail.com>
 *
 * stress tools for echo server
 */

#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#endif

#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <string.h>

#include "event.h"
#include "evutil.h"

static const char * gHost = "127.0.0.1";
static int gPort = 5555;
static int gMsgs = 10;
static int gClients = 10;

static time_t gStartTime = 0;

typedef int evutil_socket_t;

typedef struct tagSP_TestClient {
	evutil_socket_t mFd;
	struct event mReadEvent;
	struct event mWriteEvent;
	int mSendMsgs;
	int mRecvMsgs;
	char mBuffer[ 512 ];
} SP_TestClient;

void showUsage( const char * program )
{
	printf( "Stress Test Tools\n" );
	printf( "Usage: %s [-h <host>] [-p <port>] [-c <clients>] [-m <messages>]\n", program );
	printf( "\t-h default is %s\n", gHost );
	printf( "\t-p default is %d\n", gPort );
	printf( "\t-c how many clients, default is %d\n", gClients );
	printf( "\t-m messages per client, default is %d\n", gMsgs );
	printf( "\n" );
}

int is_try_again()
{
#ifdef WIN32
	return WSAEWOULDBLOCK == WSAGetLastError();
#else
	return EAGAIN == errno;
#endif
}

void close_read( SP_TestClient * client )
{
	//fprintf( stderr, "#%d close read\n", client->mFd );
	event_del( &client->mReadEvent );
	gClients--;
}

void close_write( SP_TestClient * client )
{
	//fprintf( stderr, "#%d close write\n", client->mFd );
	event_del( &client->mWriteEvent );
}

void close_client( SP_TestClient * client )
{
	close_write( client );
	close_read( client );
}

void on_read( evutil_socket_t fd, short events, void *arg )
{
	SP_TestClient * client = ( SP_TestClient * ) arg;
	int i = 0;

	if( EV_READ & events ) {
		int len = recv( fd, client->mBuffer, sizeof( client->mBuffer ), 0 );
		if( len <= 0 ) {
			if( len < 0 && ( ! is_try_again() ) ) {
				fprintf( stderr, "#%d on_read error, errno %d\n", fd, EVUTIL_SOCKET_ERROR() );
			}
			close_client( client );
		} else {
			for( i = 0; i < len; i++ ) {
				if( '\n' == client->mBuffer[i] ) client->mRecvMsgs++;
			}
		}
	} else {
		fprintf( stderr, "#%d on_read timeout\n", fd );
		close_client( client );
	}
}

void on_write( evutil_socket_t fd, short events, void *arg )
{
	SP_TestClient * client = ( SP_TestClient * ) arg;
	int len = 0;

	if( EV_WRITE & events ) {
		client->mSendMsgs++;

		if( client->mSendMsgs >= gMsgs ) {
			evutil_snprintf( client->mBuffer, sizeof( client->mBuffer ), "quit\n" );
		} else {
			evutil_snprintf( client->mBuffer, sizeof( client->mBuffer ),
				"mail #%d, It's good to see how people hire; "
				"that tells us how to market ourselves to them.\n", client->mSendMsgs );
		}

		len = send( fd, client->mBuffer, strlen( client->mBuffer ), 0 );

		if( len <= 0 && ( ! is_try_again() ) ) {
			fprintf( stderr, "#%d on_write error, errno %d\n", fd, EVUTIL_SOCKET_ERROR() );
			close_client( client );
		} else {
			if( client->mSendMsgs >= gMsgs ) close_write( client );
		}
	} else {
		fprintf( stderr, "#%d on_write timeout\n", fd );
		close_client( client );
	}
}

void parse_arg( int argc, char * argv[] )
{
	int i = 0;
	for( i = 1; i < argc; i+=2 ) {
		if( 0 == strcmp( argv[i], "-h" ) ) {
			gHost = argv[i+1];
		}else if( 0 == strcmp( argv[i], "-p" ) ) {
			gPort = atoi( argv[i+1] );
		} else if( 0 == strcmp( argv[i], "-c" ) ) {
			gClients = atoi( argv[i+1] );
		} else if( 0 == strcmp( argv[i], "-m" ) ) {
			gMsgs = atoi( argv[i+1] );
		} else {
			showUsage( argv[0] );
			exit( 0 );
		}
	}
}


int main( int argc, char * argv[] )
{
	SP_TestClient * clientList = NULL;
	struct sockaddr_in sin;
	int totalClients = 0, i = 0;
	struct timeval startTime, stopTime;
	time_t lastInfoTime = 0;
	double totalTime = 0;
	int totalSend = 0, totalRecv = 0;

#ifdef WIN32
	WSADATA wsaData;
	
	int err = WSAStartup( MAKEWORD( 2, 0 ), &wsaData );
	if ( err != 0 ) {
		printf( "Couldn't find a useable winsock.dll.\n" );
		return -1;
	}
#endif

#ifdef SIGPIPE
	signal( SIGPIPE, SIG_IGN );
#endif

	parse_arg( argc, argv );

	event_init();

	clientList = (SP_TestClient*)calloc( gClients, sizeof( SP_TestClient ) );
	
	memset( &sin, 0, sizeof(sin) );
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = inet_addr( gHost );
	sin.sin_port = htons( gPort );

	totalClients = gClients;
	i = 0;

	printf( "Create %d connections to server, it will take some minutes to complete.\n", gClients );
	for( i = 0; i < gClients; i++ ) {
		SP_TestClient * client = clientList + i;

		client->mFd = socket( AF_INET, SOCK_STREAM, 0 );
		if( client->mFd < 0 ) {
			fprintf(stderr, "#%d, socket failed, errno %d\n", i, EVUTIL_SOCKET_ERROR() );
			return -1;
		}

		if( connect( client->mFd, (struct sockaddr *)&sin, sizeof(sin) ) != 0) {
			fprintf(stderr, "#%d, connect failed, errno %d\n", i, EVUTIL_SOCKET_ERROR() );
			return -1;
		}

		event_set( &client->mWriteEvent, client->mFd, EV_WRITE | EV_PERSIST, on_write, client );
		event_add( &client->mWriteEvent, NULL );

		event_set( &client->mReadEvent, client->mFd, EV_READ | EV_PERSIST, on_read, client );
		event_add( &client->mReadEvent, NULL );

		if( 0 == ( i % 10 ) ) printf( "." );
	}

	printf( "\n" );

	time( &gStartTime );

	evutil_gettimeofday( &startTime, NULL );

	lastInfoTime = time( NULL );

	// start event loop until all clients are exit
	while( gClients > 0 ) {
		event_loop( EVLOOP_ONCE );

		if( time( NULL ) - lastInfoTime > 5 ) {
			time( &lastInfoTime );
			printf( "waiting for %d client(s) to exit\n", gClients );
		}
	}

	evutil_gettimeofday( &stopTime, NULL );

	totalTime = (double) ( 1000000 * ( stopTime.tv_sec - startTime.tv_sec )
			+ ( stopTime.tv_usec - startTime.tv_usec ) ) / 1000000;

	// show result
	printf( "\n\nTest result :\n" );
	printf( "Clients : %d, Messages Per Client : %d\n", totalClients, gMsgs );
	printf( "ExecTimes: %.6f seconds\n\n", totalTime );

	printf( "client\tSend\tRecv\n" );
	totalSend = 0;
	totalRecv = 0;
	for( i = 0; i < totalClients; i++ ) {
		SP_TestClient * client = clientList + i;

		//printf( "client#%d : %d\t%d\n", i, client->mSendMsgs, client->mRecvMsgs );

		totalSend += client->mSendMsgs;
		totalRecv += client->mRecvMsgs;

		EVUTIL_CLOSESOCKET( client->mFd );
	}

	printf( "total   : %d\t%d\n", totalSend, totalRecv );
	printf( "average : %.0f/s\t%.0f/s\n", totalSend / totalTime, totalRecv / totalTime );

	free( clientList );

#ifdef WIN32
	WSACleanup();
#endif

	return 0;
}

