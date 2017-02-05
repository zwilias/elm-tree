module Tree.AA exposing (..)

{-|

# Types
@docs Tree

# Creation
@docs empty, singleton

# Basic operations
@docs insert, remove, member, size, foldl, foldr

# AA tree operations
@docs unsafeMinimum, unsafeMaximum, skew, split, skewRight, skewRightRight, splitRight, rebalance, decreaseLevel, getLevel, setLevel, mapRight

# Fold-based operations
@docs map, filter, toList, fromList, union, remove, intersect, diff, partition

-}

import Debug
import Function exposing (swirlr)


{-|
-}
type Tree comparable
    = Empty
    | Node Int (Tree comparable) comparable (Tree comparable)


{-|
-}
empty : Tree comparable
empty =
    Empty


{-|
-}
singleton : comparable -> Tree comparable
singleton item =
    Node 1 empty item empty


{-|
-}
insert : comparable -> Tree comparable -> Tree comparable
insert item tree =
    let
        fixup : Tree comparable -> Tree comparable
        fixup =
            skew >> split
    in
        case tree of
            Empty ->
                singleton item

            Node level left self right ->
                if item < self then
                    Node
                        level
                        (insert
                            item
                            left
                        )
                        self
                        right
                        |> fixup
                else if item == self then
                    tree
                else
                    Node
                        level
                        left
                        self
                        (insert
                            item
                            right
                        )
                        |> fixup


{-|
-}
remove : comparable -> Tree comparable -> Tree comparable
remove item tree =
    case tree of
        Empty ->
            Empty

        Node level left self right ->
            if item < self then
                Node level (remove item left) self right
            else if item == self then
                case ( left, right ) of
                    ( Empty, Empty ) ->
                        Empty

                    ( Empty, _ ) ->
                        let
                            successor : comparable
                            successor =
                                unsafeMinimum right

                            newRight : Tree comparable
                            newRight =
                                remove successor right
                        in
                            Node level left successor newRight
                                |> rebalance

                    ( _, _ ) ->
                        let
                            predecessor : comparable
                            predecessor =
                                unsafeMaximum left

                            newLeft : Tree comparable
                            newLeft =
                                remove predecessor left
                        in
                            Node level newLeft predecessor right
                                |> rebalance
            else
                Node level left self (remove item right)


{-|
-}
member : comparable -> Tree comparable -> Bool
member item tree =
    case tree of
        Empty ->
            False

        Node _ left self right ->
            if item < self then
                member item left
            else if item == self then
                True
            else
                member item right


{-|
-}
foldl : (comparable -> a -> a) -> a -> Tree comparable -> a
foldl operator acc tree =
    case tree of
        Empty ->
            acc

        Node _ left self right ->
            foldl operator acc left
                |> operator self
                |> swirlr foldl right operator


{-|
-}
foldr : (comparable -> a -> a) -> a -> Tree comparable -> a
foldr operator acc tree =
    case tree of
        Empty ->
            acc

        Node _ left self right ->
            foldr operator acc right
                |> operator self
                |> swirlr foldr left operator



-- Internal functions


{-|
-}
unsafeMinimum : Tree comparable -> comparable
unsafeMinimum tree =
    case tree of
        Empty ->
            Debug.crash "Can't get minimal value of empty tree."

        Node _ Empty item _ ->
            item

        Node _ left _ _ ->
            unsafeMinimum left


{-|
-}
unsafeMaximum : Tree comparable -> comparable
unsafeMaximum tree =
    case tree of
        Empty ->
            Debug.crash "Can't get maximal value of empty tree."

        Node _ _ item Empty ->
            item

        Node _ _ _ right ->
            unsafeMaximum right


{-|
-}
getLevel : Tree comparable -> Int
getLevel tree =
    case tree of
        Empty ->
            0

        Node level _ _ _ ->
            level


{-|
-}
skew : Tree comparable -> Tree comparable
skew tree =
    case tree of
        Node level (Node lLevel lLeft lSelf lRight) self right ->
            if level == lLevel then
                Node level lRight self right
                    |> Node level lLeft lSelf
            else
                tree

        _ ->
            tree


{-|
-}
split : Tree comparable -> Tree comparable
split tree =
    case tree of
        Node level left self (Node rLevel rLeft rSelf rRight) ->
            if level == rLevel && level == getLevel rRight then
                Node
                    (level + 1)
                    (Node level left self rLeft)
                    rSelf
                    rRight
            else
                tree

        _ ->
            tree


{-|
-}
mapRight : (Tree comparable -> Tree comparable) -> Tree comparable -> Tree comparable
mapRight op tree =
    case tree of
        Empty ->
            Empty

        Node level left self right ->
            Node level left self (op right)


{-|
-}
skewRight : Tree comparable -> Tree comparable
skewRight =
    mapRight skew


{-|
-}
splitRight : Tree comparable -> Tree comparable
splitRight =
    mapRight split


{-|
-}
skewRightRight : Tree comparable -> Tree comparable
skewRightRight =
    mapRight skewRight


{-|
-}
setLevel : Int -> Tree comparable -> Tree comparable
setLevel level tree =
    case tree of
        Empty ->
            Empty

        Node _ left self right ->
            Node level left self right


{-|
-}
decreaseLevel : Tree comparable -> Tree comparable
decreaseLevel tree =
    case tree of
        Empty ->
            Empty

        Node level left self Empty ->
            let
                shouldBe =
                    getLevel left + 1
            in
                if level > shouldBe then
                    Node shouldBe left self Empty
                else
                    tree

        Node level left self right ->
            let
                shouldBe =
                    1 + min (getLevel left) (getLevel right)
            in
                if level > shouldBe then
                    let
                        newRight =
                            if getLevel right > shouldBe then
                                setLevel shouldBe right
                            else
                                right
                    in
                        Node shouldBe left self newRight
                else
                    tree


{-|
-}
rebalance : Tree comparable -> Tree comparable
rebalance =
    decreaseLevel >> skew >> skewRight >> skewRightRight >> split >> splitRight



-- Fold-based operations


{-| Convert tree to list in ascending order, using foldl.
-}
toList : Tree comparable -> List comparable
toList =
    foldl (::) []


{-| Create tree from list by folding over the list and inserting into an
initially empty tree.
-}
fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert empty


{-| Foldl over the list and incrementing an accumulator by one for each value
that passes through the accumulator operation.
-}
size : Tree comparable -> Int
size =
    foldl (\_ acc -> acc + 1) 0


{-| Fold over the tree, executing the specified operation on each value, and
accumulating these values into a new tree.
-}
map : (comparable -> comparable2) -> Tree comparable -> Tree comparable2
map operator =
    foldl
        (insert << operator)
        empty


{-| Create a new set with elements that match the predicate.
-}
filter : (comparable -> Bool) -> Tree comparable -> Tree comparable
filter predicate =
    foldl
        (\item ->
            if predicate item then
                insert item
            else
                identity
        )
        empty


{-| Union is implemented by folding over the second list and inserting it into
the first list.
-}
union : Tree comparable -> Tree comparable -> Tree comparable
union =
    foldl insert


{-| Tree intersection creates a new Tree containing only those values found in
both trees. This is implemented by filtering the right-hand set, only keeping
values found in the left-hand set.
-}
intersect : Tree comparable -> Tree comparable -> Tree comparable
intersect left =
    filter (flip member left)


{-| The differences between two trees is, in Elm land, defined as the elements
of the left tree that do not exists in the right tree. As such, this is
implemented by filtering the left tree for values that do not exist in the
right set.
-}
diff : Tree comparable -> Tree comparable -> Tree comparable
diff left right =
    filter (not << flip member right) left


{-| Similar to filtering, this does not throw away the values that do not match
the predicate, but creating a second tree from those values. The resulting
trees are then returned as a tuple.
-}
partition : (comparable -> Bool) -> Tree comparable -> ( Tree comparable, Tree comparable )
partition predicate =
    foldl
        (\item ->
            if predicate item then
                Tuple.mapFirst <| insert item
            else
                Tuple.mapSecond <| insert item
        )
        ( empty, empty )
