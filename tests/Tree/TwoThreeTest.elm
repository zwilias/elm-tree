module Tree.TwoThreeTest exposing (..)

import Test exposing (..)
import Expect
import Tree.TwoThree as Tree exposing (..)
import Fuzz exposing (list, int)
import Util


all : Test
all =
    describe "2-3 tree"
        [ describe "Basic sanity"
            [ test "Empty" <|
                \() ->
                    Tree.empty
                        |> Tree.size
                        |> Expect.equal 0
            , fuzz int "Singleton" <|
                \item ->
                    item
                        |> Tree.singleton
                        |> Expect.all
                            [ Tree.member item >> Expect.true "Singleton should contain value"
                            , Tree.size >> Expect.equal 1
                            ]
            , fuzz (list int) "Member" <|
                \items ->
                    items
                        |> Tree.fromList
                        |> flip Tree.member
                        |> flip List.all items
                        |> Expect.true "Expect all items to be present in list"
            ]
        , describe "Invariants for 2-3 tree"
            [ invariant
                "BST"
                checker0
            , invariant
                "All leave nodes are at an equal depth"
                checker1
            ]
        ]



-- BST invariant


checker0 : Tree comparable -> Bool
checker0 tree =
    case tree of
        Empty ->
            True

        Tree.TwoNode left value right ->
            allInTree ((<) value) left
                && allInTree ((>) value) right
                && checker0 left
                && checker0 right

        Tree.ThreeNode lower left between right higher ->
            allInTree ((<) left) lower
                && allInTree ((>) left) between
                && allInTree ((<) right) between
                && allInTree ((>) right) higher
                && checker0 lower
                && checker0 between
                && checker0 higher


allInTree : (comparable -> Bool) -> Tree comparable -> Bool
allInTree predicate tree =
    Tree.filter (predicate) tree
        |> \tree -> Tree.size tree == 0



-- invariants


getLeaveDepths : Int -> Tree comparable -> List Int
getLeaveDepths depth tree =
    case tree of
        Empty ->
            []

        Tree.TwoNode Empty self Empty ->
            [ depth ]

        Tree.ThreeNode Empty left Empty right Empty ->
            [ depth ]

        Tree.TwoNode left _ right ->
            getLeaveDepths (depth + 1) left
                ++ getLeaveDepths (depth + 1) right

        Tree.ThreeNode lower _ between _ greater ->
            getLeaveDepths (depth + 1) lower
                ++ getLeaveDepths (depth + 1) between
                ++ getLeaveDepths (depth + 1) greater


checker1 : Tree comparable -> Bool
checker1 tree =
    case getLeaveDepths 0 tree of
        [] ->
            True

        [ _ ] ->
            True

        head :: tail ->
            List.all ((==) head) tail



-- Invariant checker


invariant : String -> (Tree Int -> Bool) -> Test
invariant desc checker =
    describe desc
        [ testInsertion checker
        , testRemoval checker
        ]


testInsertion : (Tree Int -> Bool) -> Test
testInsertion checker =
    fuzz (list int) "Invariant holds during insertions" <|
        \members ->
            members
                |> Tree.fromList
                |> checker
                |> Expect.true "Invariant did not hold"


testRemoval : (Tree Int -> Bool) -> Test
testRemoval checker =
    fuzz Util.listAndSublist "Invariant holds during removal" <|
        \( members, remove ) ->
            let
                removeItems : Tree Int -> Tree Int
                removeItems tree =
                    remove
                        |> List.foldl Tree.remove tree
            in
                members
                    |> Tree.fromList
                    |> removeItems
                    |> checker
                    |> Expect.true "Invariant did not hold"
