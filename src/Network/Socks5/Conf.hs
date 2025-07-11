{- |
Module      : Network.Socks5.Conf
License     : BSD-style
Maintainer  : Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Typical SOCKS configuration.
-}

module Network.Socks5.Conf
  ( SocksConf (..)
  , socksHost
  , defaultSocksConf
  , defaultSocksConfFromSockAddr
  ) where

import           Network.Socket ( SockAddr )
import           Network.Socks5.Types ( SocksVersion (..) )

-- | Type representing SOCKS identification and configuration structures.
--
-- The data constructors may be extended in the future to support
-- authentification. Use the smart constructor 'defaultSocksConf'
-- and 'socksHost'.
data SocksConf = SocksConf
  { socksServer  :: SockAddr     -- ^ The address of the server.
  , socksVersion :: SocksVersion -- ^ The SOCKS protocol version to use.
  }

-- | Yield the socket address of the server from the specified configuration.
socksHost :: SocksConf -> SockAddr
socksHost = socksServer

-- | Yield a configuration given the specified socket addresss.
defaultSocksConf ::
     SockAddr
     -- ^ The address of the server.
  -> SocksConf
defaultSocksConf host = SocksConf host SocksVer5

-- | Same as 'defaultSocksConf'.
--
-- Soft deprecation: use 'defaultSocksConf'.
defaultSocksConfFromSockAddr :: SockAddr -> SocksConf
defaultSocksConfFromSockAddr = defaultSocksConf
