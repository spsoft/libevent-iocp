/**
 * http://unx.ca/log/2006/10/17/new-libevent-dns-and-http-support/
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

#include <stdlib.h>
#include <stdio.h>

#include <event.h>
#include <evdns.h>

void
usage(void)
{
        fprintf(stderr, "USAGE: evdns-demo <hostname>\n");
        exit(1);
}

void
evdns_cb(int result, char type, int count, int ttl, void *addresses, void *arg)
{
        struct in_addr *addrs = addresses;
        int i;

        if (result != 0) {
                printf("Error looking up address.\n");
                exit(1);
        }
        else {
                for (i = 0; i < count; i++) {
                        printf("%s\n", inet_ntoa(addrs[i]));
                }
                exit(0);
        }
}

int
main(int argc, char **argv)
{
        const char * domain = "www.google.com";

#ifdef WIN32
        WSADATA wsaData;
	
        int err = WSAStartup( MAKEWORD( 2, 0 ), &wsaData );
        if ( err != 0 ) {
                printf( "Couldn't find a useable winsock.dll.\n" );
                return -1;
        }
#endif

		//putenv( "EVENT_NOWIN32IOCP=1" );

        printf( "method %s\n", event_base_get_method( event_init() ) );

        evdns_init();
        evdns_resolve_ipv4(domain, 0, evdns_cb, NULL);
        event_dispatch();

        return (0);
}
