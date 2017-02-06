module Tree.AA exposing (..)

{-|

AA trees are a variation of the red-black tree, where "red" nodes can only be
added on the right-hand side. This results in a simulation of 2-3 trees,
whereas red-black trees could be considered a simulation or 2-3-4 trees.

Due to the stricter invariant, maintenance operations needed to keep the tree
balanced are quite a bit simpler than they are for regular red-black trees.

Five invariants hold for AA trees:

1. The level of every leaf node is one.
2. The level of every left child is exactly one less than that of its parent.
3. The level of every right child is equal to or one less than that of its
parent.
4. The level of every right grandchild is strictly less than that of its
grandparent.
5. Every node of level greater than one has two children.

These invariants are maintained by skewing and splitting the tree after
insertion and deletion, as well as updating the levels after deletion of
internal nodes.

# Types
@docs Tree

# Creation
@docs empty, singleton

# Basic operations
@docs insert, remove, member, size, foldl, foldr

# AA tree operations
@docs unsafeMinimum, unsafeMaximum, skew, split, rebalance, decreaseLevel, getLevel, setLevel, mapRight

# Fold-based operations
@docs map, filter, toList, fromList, union, remove, intersect, diff, partition

-}

import Debug
import Function exposing (swirlr)


{-| A node in an AA tree can be either Empty, a branch node with one or two
child-trees, or a leaf-node. A leaf-node is represented as a branch-node with
both branch-points left empty.

Additionally, nodes hold an extra Int keeping track of the level.
-}
type Tree comparable
    = Empty
    | Node Int (Tree comparable) comparable (Tree comparable)


{-| Constructs an empty tree.
-}
empty : Tree comparable
empty =
    Empty


{-| Constructs a tree with one leaf holding the provided value.
-}
singleton : comparable -> Tree comparable
singleton item =
    Node 1 empty item empty


{-| Insertion in an AA tree is quite simple:

- Find the insertion point
- Replace that empty node by a singleton
- Fixup:

    - `skew` if required
    - `split` if required

Both `skew` and `split` are able to decide whether or not they need to do
anything, so calling them after every insertion is safe and guarantees the
invariants to hold.
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


{-| Removal from AA trees works as follows:

- Recursively locate the node to be removed.
- If it's an internal node, substitute for its successor or predecessor.
- If it's leaf-node, replace by empty.
- While unwinding the stack, `rebalance` - fixing up levels and balance by skewing and splitting.
-}
remove : comparable -> Tree comparable -> Tree comparable
remove item tree =
    case tree of
        Empty ->
            Empty

        Node level left self right ->
            if item < self then
                Node level (remove item left) self right |> rebalance
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
                Node level left self (remove item right) |> rebalance


{-| Check if an item is a member of a tree, recursively.
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


{-| Fold over the values in the tree, left to right, strictly ascending.
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


{-| Fold over the values in the tree, right to left, strictly descending.
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


{-| Finds the minimal value in the provided tree. Crashes when called on an
Empty tree -- meant for internal use within delete where its used to find the
predecessor from an *internal* node, hence guaranteeing safety.
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


{-| Finds the maximal value in the provided tree. Crashes when called on an
Empty tree -- meant for internal use within delete where its used to find the
successor from an *internal* node, hence guaranteeing safety.
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


{-| Get the level of a node in a tree. For an Empty tree, this is always 0. For
every other node, this is a piece of information kept within the node.
-}
getLevel : Tree comparable -> Int
getLevel tree =
    case tree of
        Empty ->
            0

        Node level _ _ _ ->
            level


{-| Skewing is a right rotation to replace a subtree containing a
left-horizontal link with one containing a right horizontal link.

![Courtesy of wikipedia](https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/AA_Tree_Skew2.svg/560px-AA_Tree_Skew2.svg.png)
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


{-| Split is a left rotation and level increase to replace a subtree containing
two or more consecutive right horizontal links with one containing two fewer
consecutive right horizontal links.

![Courtesy of wikipedia](https://upload.wikimedia.org/wikipedia/commons/thumb/0/0e/AA_Tree_Split2.svg/510px-AA_Tree_Split2.svg.png)
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


{-| Executes the provided operation on the right subtree and return the tree
including the mapped result. Helper function of the skew/split operations
required for rebalancing after removal.
-}
mapRight : (Tree comparable -> Tree comparable) -> Tree comparable -> Tree comparable
mapRight op tree =
    case tree of
        Empty ->
            Empty

        Node level left self right ->
            Node level left self (op right)


{-| Sets the level in the provided tree to the provided value.
-}
setLevel : Int -> Tree comparable -> Tree comparable
setLevel level tree =
    case tree of
        Empty ->
            Empty

        Node _ left self right ->
            Node level left self right


{-| Decrease the level in the tree and subnodes to restore the AA invariants,
if required.
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


{-| Rebalance the tree. This is a three-step process:

- Decrease the level
- Skew the tree
- Split the tree

-}
rebalance : Tree comparable -> Tree comparable
rebalance =
    let
        skewTree : Tree comparable -> Tree comparable
        skewTree =
            skew >> mapRight skew >> mapRight (mapRight skew)

        splitTree : Tree comparable -> Tree comparable
        splitTree =
            split >> mapRight split
    in
        decreaseLevel >> skewTree >> splitTree



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
