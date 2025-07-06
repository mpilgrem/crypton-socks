module Network.Socks5.Lowlevel
    ( socksListen
    -- * lowlevel types
    , module Network.Socks5.Wire
    , module Network.Socks5.Command
    ) where

import Data.Functor ( void )
import Network.Socket
import Network.Socks5.Command
import Network.Socks5.Wire
import Network.Socks5.Types

socksListen :: Socket -> IO SocksRequest
socksListen sock = do
  void (waitSerialized sock :: IO SocksHello)
  sendSerialized sock (SocksHelloResponse SocksMethodNone)
  waitSerialized sock
