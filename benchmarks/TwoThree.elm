module TwoThree exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Char
import Dict
import Dict.TwoThree as AltDict
import Time exposing (Time)


forValues : String -> Int -> (Int -> comparable) -> (Int -> b) -> Benchmark
forValues name size keyer valuer =
    let
        source =
            List.map
                (\n -> ( keyer n, valuer n ))
                (List.range 1 size)

        key =
            keyer size

        value =
            valuer size
    in
        describe (name ++ " (" ++ toString size ++ " items)")
            [ Benchmark.compare "insert"
                (Benchmark.benchmark3 "Dict" Dict.insert key value (Dict.fromList source))
                (Benchmark.benchmark3 "Dict.TwoThree" AltDict.insert key value (AltDict.fromList source))
            , Benchmark.compare "get"
                (Benchmark.benchmark2 "Dict" Dict.get key (Dict.fromList source))
                (Benchmark.benchmark2 "Dict.TwoThree" AltDict.get key (AltDict.fromList source))
            , Benchmark.compare "remove"
                (Benchmark.benchmark2 "Dict" Dict.remove key (Dict.fromList source))
                (Benchmark.benchmark2 "Dict.TwoThree" AltDict.remove key (AltDict.fromList source))
            , Benchmark.compare "toList"
                (Benchmark.benchmark1 "Dict" Dict.toList (Dict.fromList source))
                (Benchmark.benchmark1 "Dict.TwoThree" AltDict.toList (AltDict.fromList source))
            ]


suiteOfSize : Int -> Benchmark
suiteOfSize size =
    describe ("dicts of size " ++ toString size)
        [ forValues "string" size toString (always ())
        , forValues "int" size identity (always ())
        , forValues "float" size toFloat (always ())
        , forValues "time" size (toFloat >> (*) Time.millisecond) (always ())
        , forValues "char" size Char.fromCode (always ())
        , forValues "tuple of int" size (\i -> ( i, i )) (always ())
        ]


main : BenchmarkProgram
main =
    program <| describe "dicts" <| List.map suiteOfSize [ 0, 1, 10, 100, 1000, 10000 ]
