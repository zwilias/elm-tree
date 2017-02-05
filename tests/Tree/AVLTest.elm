module Tree.AVLTest exposing (..)

import Test exposing (..)
import Expect exposing (Expectation, pass, fail)
import Tree.AVL as Tree exposing (..)
import Fuzz exposing (list, int)


all : Test
all =
    describe "AVL tree"
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
        , describe "Invariants for AVL tree"
            [ invariant
                "BST"
                checker0
            , invariant
                "The absolute balancefactor at each node is at most 1."
                checker1
            ]
        ]



-- BST invariant


checker0 : Tree comparable -> Bool
checker0 tree =
    case tree of
        Empty ->
            True

        Tree.Node _ value left right ->
            allInTree ((<) value) left
                && allInTree ((>) value) right
                && checker0 left
                && checker0 right


allInTree : (comparable -> Bool) -> Tree comparable -> Bool
allInTree predicate tree =
    Tree.filter (predicate) tree
        |> \tree -> Tree.size tree == 0



-- invariants


{-| The level of every leaf node is one.
-}
checker1 : Tree comparable -> Bool
checker1 tree =
    case tree of
        Tree.Empty ->
            True

        Tree.Node _ _ left right ->
            (abs <| Tree.heightDiff tree)
                <= 1
                && checker1 left
                && checker1 right



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
    fuzz2 (list int) (list int) "Invariant holds during removal" <|
        \members toBeDeleted ->
            members
                |> Tree.fromList
                |> (\tree -> List.foldl Tree.remove tree members)
                |> checker
                |> Expect.true "Invariant did not hold"
