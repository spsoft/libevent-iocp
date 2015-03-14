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
#include <evhttp.h>

void
root_handler(struct evhttp_request *req, void *arg)
{
        struct evbuffer *buf;

        buf = evbuffer_new();
        if (buf == NULL)
                printf("failed to create response buffer");
        evbuffer_add_printf(buf, "Hello World!\n");
        evhttp_send_reply(req, HTTP_OK, "OK", buf);
}

void
generic_handler(struct evhttp_request *req, void *arg)
{
        struct evbuffer *buf;

        buf = evbuffer_new();
        if (buf == NULL)
                printf("failed to create response buffer");
        evbuffer_add_printf(buf, "Requested: %s\n", evhttp_request_uri(req));
        evhttp_send_reply(req, HTTP_OK, "OK", buf);
}

int
main(int argc, char **argv)
{
        struct evhttp *httpd;
#ifdef WIN32
        WSADATA wsaData;
	
        int err = WSAStartup( MAKEWORD( 2, 0 ), &wsaData );
        if ( err != 0 ) {
                printf( "Couldn't find a useable winsock.dll.\n" );
                return -1;
        }
#endif

        event_init();
        httpd = evhttp_start("0.0.0.0", 8080);

        /* Set a callback for requests to "/". */
        evhttp_set_cb(httpd, "/", root_handler, NULL);

        /* Set a callback for all other requests. */
        evhttp_set_gencb(httpd, generic_handler, NULL);

        event_dispatch();

        /* Not reached in this code as it is now. */

        evhttp_free(httpd);

        return 0;
}
