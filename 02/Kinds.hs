{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

data UserType = UserK | AdminK

data Proxy (a :: UserType) = Proxy

data User = User { userAdminToken :: Maybe (Proxy 'AdminK) }

doSensitiveThings :: Proxy 'AdminK -> IO ()
doSensitiveThings = undefined
