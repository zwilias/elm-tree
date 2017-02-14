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
                    Tree.singleton item ()
                        |> Expect.all
                            [ Tree.member item >> Expect.true "Singleton should contain value"
                            , Tree.size >> Expect.equal 1
                            ]
            , fuzz (list int) "Member" <|
                \items ->
                    items
                        |> List.map (flip (,) ())
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


checker0 : Tree comparable a -> Bool
checker0 tree =
    case tree of
        Empty ->
            True

        Tree.TwoNode left key _ right ->
            allInTree (\k v -> k < key) left
                && allInTree (\k v -> k > key) right
                && checker0 left
                && checker0 right

        Tree.ThreeNode lower left _ between right _ greater ->
            allInTree (\k v -> k < left) lower
                && allInTree (\k v -> k > left && k < right) between
                && allInTree (\k v -> k > right) greater
                && checker0 lower
                && checker0 between
                && checker0 greater


allInTree : (comparable -> a -> Bool) -> Tree comparable a -> Bool
allInTree predicate tree =
    Tree.filter (\k v -> not (predicate k v)) tree
        |> \tree -> Tree.size tree == 0



-- invariants


getLeaveDepths : Int -> Tree k v -> List Int
getLeaveDepths depth tree =
    case tree of
        Empty ->
            []

        Tree.TwoNode Empty self _ Empty ->
            [ depth ]

        Tree.ThreeNode Empty left _ Empty right _ Empty ->
            [ depth ]

        Tree.TwoNode left _ _ right ->
            getLeaveDepths (depth + 1) left
                ++ getLeaveDepths (depth + 1) right

        Tree.ThreeNode lower _ _ between _ _ greater ->
            getLeaveDepths (depth + 1) lower
                ++ getLeaveDepths (depth + 1) between
                ++ getLeaveDepths (depth + 1) greater


checker1 : Tree k v -> Bool
checker1 tree =
    case getLeaveDepths 0 tree of
        [] ->
            True

        [ _ ] ->
            True

        head :: tail ->
            List.all ((==) head) tail



-- Invariant checker


invariant : String -> (Tree Int () -> Bool) -> Test
invariant desc checker =
    describe desc
        [ testInsertion checker
        , testRemoval checker
        ]


testInsertion : (Tree Int () -> Bool) -> Test
testInsertion checker =
    fuzz (list int) "Invariant holds during insertions" <|
        \members ->
            members
                |> List.map (flip (,) ())
                |> Tree.fromList
                |> checker
                |> Expect.true "Invariant did not hold"


testRemoval : (Tree Int () -> Bool) -> Test
testRemoval checker =
    fuzz Util.listAndSublist "Invariant holds during removal" <|
        \( members, remove ) ->
            let
                removeItems : Tree Int () -> Tree Int ()
                removeItems tree =
                    remove
                        |> List.foldl Tree.remove tree
            in
                members
                    |> List.map (flip (,) ())
                    |> Tree.fromList
                    |> removeItems
                    |> checker
                    |> Expect.true "Invariant did not hold"
