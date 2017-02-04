module Tree.AVL exposing (..)

{-| AVL Trees are [..]

# Types
@docs Tree

# Creation
@docs empty, singleton

# Basic operations
@docs insert, remove, member, size, foldl, foldr

# Fold-based operations
@docs map, filter, toList, fromList, union, remove, intersect, diff, partition

# Internals
@docs tree, rotateLeft, rotateRight, height, heightDiff, balance
-}

import Function exposing (swirlr)


{-| A node in an avl tree is either a node with one key and zero, one or two
branches, or is empty. Some trees (like Two Three trees) impose the restriction
that a node must be either a leaf node (having no children), or a completely
internal node, i.e. having all branch-points filled in by a non-empty node.

The left branch of a node contains values smaller than the node's own value, the
right branch contains values greater than the node's own value.
-}
type Tree comparable
    = Node Int comparable (Tree comparable) (Tree comparable)
    | Empty



-- Basics: Creation


{-| A singleton in an AVL tree is a Node with both branch-points empty.
-}
singleton : comparable -> Tree comparable
singleton item =
    Node 1 item empty empty


{-| An empty AVL tree is a single `Empty` node.
-}
empty : Tree comparable
empty =
    Empty



-- Basics: Insertion, deletion, member


{-| Insertion semantics in AVL trees are rather simple:

- Locate where the item has to be inserted, which will always be at a
branch-point, or not at all
- Insert if it does not exist yet by adding a singleton in the appropriate
branch-point
- Rebalance the tree, which might require some rotating.
-}
insert : comparable -> Tree comparable -> Tree comparable
insert item set =
    case set of
        Empty ->
            singleton item

        Node height head left right ->
            if item < head then
                tree head (insert item left) right |> balance
            else if item > head then
                tree head left (insert item right) |> balance
            else
                set


{-| Removal in AVL trees works by searching for the node to be removed, and
replacing that node by the union of its branch-points (which might be Empty if
the node to be removed was a leaf-node).

After the actual removal, the parent branches are rebalanced during the
recursive bubble up, so as to maintain the AVL invariants.
-}
remove : comparable -> Tree comparable -> Tree comparable
remove item set =
    case set of
        Empty ->
            set

        Node _ head left right ->
            if item < head then
                tree head (remove item left) right |> balance
            else if item > head then
                tree head left (remove item right) |> balance
            else
                union left right


{-| Member checks happen the same way as in other binary trees, recursively
selecting the most likely place to find the item to check for. As with other
self-balancing binary trees, this can happen in `O (log n)`.
-}
member : comparable -> Tree comparable -> Bool
member item set =
    case set of
        Empty ->
            False

        Node _ head left right ->
            if item < head then
                member item left
            else if item > head then
                member item right
            else
                True



-- Folds


{-| As with other trees, left-folding works by recursively folding over the
left hand side, then the own value, then the right hand side. Since our initial
node definition specified that all values contained in the left branch must be
smaller than the node-value, and all values in the right branch must be greater
than the node-value, this ensures values are folded over in strictly ascending
order.
-}
foldl : (comparable -> a -> a) -> a -> Tree comparable -> a
foldl operation acc set =
    case set of
        Empty ->
            acc

        Node _ head left right ->
            foldl operation acc left
                |> operation head
                |> swirlr foldl right operation


{-| Simply the mirror of `foldl` -- rather than recursing left-hand first, fold
right-hand first, which ensures the values are folded over in descending order.
-}
foldr : (comparable -> a -> a) -> a -> Tree comparable -> a
foldr operation acc set =
    case set of
        Empty ->
            acc

        Node _ head left right ->
            foldr operation acc right
                |> (\acc -> operation head acc)
                |> (\acc -> foldr operation acc left)



-- Internal, implementation-specific operations


{-| Create a new set with a given element and a predecided lefthand and
righthand value.

Note that this constructor does *not* use insertion internally, and - hence -
**does not sustain the AVL invariant**.

```
import Tree.Self as Tree exposing (Tree)

customSingleton : Int -> Tree Int
customSingleton val =
    Tree.tree val Tree.empty Tree.empty


customSingleton 1 == < 1 . . >
```
-}
tree : comparable -> Tree comparable -> Tree comparable -> Tree comparable
tree item left right =
    let
        maxHeight : Int
        maxHeight =
            max
                (height left)
                (height right)
                |> (+) 1
    in
        Node
            maxHeight
            item
            left
            right


{-| The height of a set is something baked right into the Tree, and is important
for balancing the internal tree. A properly balanced tree will have a maximal
height-difference between branches of |1|.

```
import Tree.Self as Tree exposing (Tree)


Tree.height Tree.empty == 0
Tree.height Tree.singleton 0 == 1

Tree.fromList [ 1, 2, 3 ]
    |> Tree.height == 2
```
-}
height : Tree comparable -> Int
height set =
    case set of
        Empty ->
            0

        Node height _ _ _ ->
            height


{-| Rotate a tree to the left (for balancing).

![Example from wikipedia](https://www.brianthicks.com/images/sets/Node_rotation_animation_250x250.gif)
-}
rotateLeft : Tree comparable -> Tree comparable
rotateLeft set =
    case set of
        Node _ root less (Node _ pivot between greater) ->
            tree pivot (tree root less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Tree comparable -> Tree comparable
rotateRight set =
    case set of
        Node _ root (Node _ pivot less between) greater ->
            tree pivot less (tree root between greater)

        _ ->
            set


{-| Calculate the difference in height between our left and right branches.

A *valid* AVL tree has a maximal height difference of |1| over its branches,
which allows checking if a random element is in the tree in `O (log n)`.
-}
heightDiff : Tree comparable -> Int
heightDiff set =
    case set of
        Empty ->
            0

        Node _ _ left right ->
            height right - height left


{-| Rebalances a tree (if it is, in fact, unbalanced).
-}
balance : Tree comparable -> Tree comparable
balance set =
    case set of
        Empty ->
            set

        Node _ head left right ->
            if heightDiff set == -2 && heightDiff left > 0 then
                -- Node leaning to the left with subtree leaning to the right
                tree head (rotateLeft left) right |> rotateRight
            else if heightDiff set < -1 then
                -- Node leaning to the left
                rotateRight set
            else if heightDiff set == 2 && heightDiff right < 0 then
                -- Node leaning to the right with subtree leaning left
                tree head left (rotateRight right) |> rotateLeft
            else if heightDiff set > 1 then
                -- Node leaning to the right
                rotateLeft set
            else
                -- Balanced tree
                set



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
