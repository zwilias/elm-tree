module Tree.AATest exposing (..)

import Test exposing (..)
import Expect
import Tree.AA as Tree exposing (..)
import Fuzz exposing (list, int)
import Util


all : Test
all =
    describe "AA tree"
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
        , describe "Invariants for AA tree"
            [ invariant
                "BST"
                checker0
            , invariant
                "The level of every leaf node is one."
                checker1
            , invariant
                "The level of every left child is exactly one less than that of its parent."
                checker2
            , invariant
                "The level of every right child is equal to or one less than that of its parent."
                checker3
            , invariant
                "The level of every right grandchild is strictly less than that of its grandparent."
                checker4
            , invariant
                "Every node of level greater than one has two children."
                checker5
            ]
        ]



-- BST invariant


checker0 : Tree comparable a -> Bool
checker0 tree =
    case tree of
        Empty ->
            True

        Tree.Node _ left key _ right ->
            allInTree (\k v -> k < key) left
                && allInTree (\k v -> k > key) right
                && checker0 left
                && checker0 right


allInTree : (comparable -> a -> Bool) -> Tree comparable a -> Bool
allInTree predicate tree =
    Tree.filter (\k v -> not (predicate k v)) tree
        |> \tree -> Tree.size tree == 0



-- invariants


{-| The level of every leaf node is one.
-}
checker1 : Tree k v -> Bool
checker1 tree =
    case tree of
        Tree.Empty ->
            True

        Tree.Node level (Tree.Empty) _ _ (Tree.Empty) ->
            level == 1

        Tree.Node _ left _ _ right ->
            checker1 left && checker1 right


{-| The level of every left child is exactly one less than that of its parent.
-}
checker2 : Tree k v -> Bool
checker2 tree =
    case tree of
        Tree.Empty ->
            True

        Tree.Node level left _ _ right ->
            level
                == (Tree.getLevel left + 1)
                && checker2 left
                && checker2 right


{-| The level of every right child is equal to or one less than that of its parent
-}
checker3 : Tree k v -> Bool
checker3 tree =
    case tree of
        Tree.Empty ->
            True

        Tree.Node level left _ _ right ->
            level
                - Tree.getLevel right
                <= 1
                && checker3 left
                && checker3 right


{-| The level of every right grandchild is strictly less than that of its grandparent.
-}
checker4 : Tree k v -> Bool
checker4 tree =
    case tree of
        Tree.Node level left _ _ ((Tree.Node _ _ _ _ grandChild) as right) ->
            level
                > Tree.getLevel grandChild
                && checker4 left
                && checker4 right

        Tree.Node _ left _ _ right ->
            checker4 left && checker4 right

        _ ->
            True


{-| Every node of level greater than one has two children.
-}
checker5 : Tree k v -> Bool
checker5 tree =
    let
        notEmpty : Tree k v -> Bool
        notEmpty tree =
            case tree of
                Tree.Empty ->
                    False

                _ ->
                    True
    in
        case tree of
            Empty ->
                True

            Tree.Node level left _ _ right ->
                if level > 1 then
                    notEmpty left
                        && notEmpty right
                        && checker5 left
                        && checker5 right
                else
                    checker5 left
                        && checker5 right



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
