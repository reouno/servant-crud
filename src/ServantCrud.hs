{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
module ServantCrud
  ( APIFor
  ) where

import           Servant

-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i
   = Get '[ JSON] [(i, a)] -- list 'a's
      :<|> ReqBody '[ JSON] a :> PostNoContent '[ JSON] NoContent -- add an 'a'
      :<|> Capture "id" i :> (Get '[ JSON] a -- view an 'a' given its "identifier" of type 'i'
                               :<|> ReqBody '[ JSON] a :> PutNoContent '[ JSON] NoContent -- update an 'a'
                               :<|> DeleteNoContent '[ JSON] NoContent -- delete an 'a'
                              )
