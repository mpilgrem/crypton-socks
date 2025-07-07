crypton-socks
=============

Originally forked from [socks-0.6.1](https://hackage.haskell.org/package/socks-0.6.1).

Haskell library implementation of the SOCKS 5 protocol.

TODO
----

 * more socks authentification methods: only no authentification is supported for now.
 * support of socks' bind for server to client connection (like FTP).
 * add socks4a and socks4 support.

Usage
-----

See `example/Example.hs` for a really simple and straighforward example. The
main API is only two calls:

* `socksConnectAddr` which connects to a `SockAddr` (`SockAddrInet` or
  `SockAddrInet6`). The name resolution is left on the client side.
* `socksConnectName` which connects to a fully qualified domain name
  `www.example.com`. The proxy server will do the name resolution.

History
-------

The [`socks`](https://hackage.haskell.org/package/socks) package was originated
and then maintained by Vincent Hanquez. For published reasons, he does not
intend to develop the package further after version 0.6.1 but he also does not
want to introduce other maintainers.
