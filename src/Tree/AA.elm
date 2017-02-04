module Tree.AA exposing (..)

import Debug
import Function exposing (swirlr)


type Tree comparable
    = Empty
    | Node Int (Tree comparable) comparable (Tree comparable)


empty : Tree comparable
empty =
    Empty


singleton : comparable -> Tree comparable
singleton item =
    Node 1 empty item empty


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


remove : comparable -> Tree comparable -> Tree comparable
remove item tree =
    tree


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


foldl : (comparable -> a -> a) -> a -> Tree comparable -> a
foldl operator acc tree =
    case tree of
        Empty ->
            acc

        Node _ left self right ->
            foldl operator acc left
                |> operator self
                |> swirlr foldl right operator


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


getLevel : Tree comparable -> Int
getLevel tree =
    case tree of
        Empty ->
            0

        Node level _ _ _ ->
            level


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
