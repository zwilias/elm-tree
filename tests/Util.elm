module Util exposing (listAndSublist)

import Fuzz exposing (list, int, intRange)
import List.Extra


listAndSublist : Fuzz.Fuzzer ( List Int, List Int )
listAndSublist =
    Fuzz.map2 takeEveryNth (list int) (intRange 1 4)


takeEveryNth : List Int -> Int -> ( List Int, List Int )
takeEveryNth list every =
    list
        |> List.Extra.indexedFoldl
            (\idx val ->
                if idx % every == 0 then
                    (::) val
                else
                    identity
            )
            []
        |> (,) list
