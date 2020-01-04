port module Ports exposing (..)

import Json.Encode as E


port storeStateCache : E.Value -> Cmd msg


port restoredStateCache : (E.Value -> msg) -> Sub msg
