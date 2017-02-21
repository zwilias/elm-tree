module Tree.AVLold exposing (..)

{-| AVL trees are the earliest example of self-balancing binary search trees.

An AVL tree upholds only a single invariant:

> The difference of maximal heights between the left- and righthand sides of
> every node is, for every internal node in the tree, either -1, 0 or 1.

This invariant is maintained by rebalancing the tree after each modification
(insertion or removal). Rebalancing, here, is implemented using one or two
rotations for every internal node while the stack unwinds.

![Tree rotations, courtesy of wikipedia](https://upload.wikimedia.org/wikipedia/commons/3/31/Tree_rotation_animation_250x250.gif)

# Types
@docs Tree

# Creation
@docs empty, singleton

# Basic operations
@docs insert, remove, member, get, foldl, foldr, size, filter, fromList

# Internals
@docs tree, rotateLeft, rotateRight, height, heightDiff, balance
-}


{-| A node in an avl tree is either a node with one key and zero, one or two
branches, or is empty. Some trees (like Two Three trees) impose the restriction
that a node must be either a leaf node (having no children), or a completely
internal node, i.e. having all branch-points filled in by a non-empty node.

The left branch of a node contains values smaller than the node's own value, the
right branch contains values greater than the node's own value.
-}
type Tree k v
    = Node Int k v (Tree k v) (Tree k v)
    | Empty



-- Basics: Creation


{-| A singleton in an AVL tree is a Node with both branch-points empty.
-}
singleton : comparable -> v -> Tree comparable v
singleton key val =
    Node 1 key val empty empty


{-| An empty AVL tree is a single `Empty` node.
-}
empty : Tree k v
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
insert : comparable -> v -> Tree comparable v -> Tree comparable v
insert key value set =
    case set of
        Empty ->
            singleton key value

        Node height head headVal left right ->
            if key < head then
                tree head headVal (insert key value left) right |> balance
            else if key > head then
                tree head headVal left (insert key value right) |> balance
            else
                tree head value left right


{-| Removal in AVL trees works by searching for the node to be removed, and
replacing that node by the union of its branch-points (which might be Empty if
the node to be removed was a leaf-node).

After the actual removal, the parent branches are rebalanced during the
recursive bubble up, so as to maintain the AVL invariants.
-}
remove : comparable -> Tree comparable v -> Tree comparable v
remove key set =
    case set of
        Empty ->
            set

        Node _ head headVal left right ->
            if key < head then
                tree head headVal (remove key left) right |> balance
            else if key > head then
                tree head headVal left (remove key right) |> balance
            else
                foldl insert right left


{-| Member checks happen the same way as in other binary trees, recursively
selecting the most likely place to find the item to check for. As with other
self-balancing binary trees, this can happen in `O (log n)`.
-}
member : comparable -> Tree comparable v -> Bool
member key set =
    case set of
        Empty ->
            False

        Node _ head _ left right ->
            if key < head then
                member key left
            else if key > head then
                member key right
            else
                True


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
tree.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Tree comparable v -> Maybe v
get key tree =
    case tree of
        Empty ->
            Nothing

        Node _ head value left right ->
            if key < head then
                get key left
            else if key > head then
                get key right
            else
                Just value



-- Folds


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> a -> a) -> a -> Tree k v -> a
foldl op acc tree =
    case tree of
        Empty ->
            acc

        Node _ key val left right ->
            foldl op acc left
                |> op key val
                |> (\acc -> foldl op acc right)


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr op acc tree =
    case tree of
        Empty ->
            acc

        Node _ key val left right ->
            foldr op acc right
                |> op key val
                |> (\acc -> foldr op acc left)



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
tree : k -> v -> Tree k v -> Tree k v -> Tree k v
tree key value left right =
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
            key
            value
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
height : Tree k v -> Int
height set =
    case set of
        Empty ->
            0

        Node height _ _ _ _ ->
            height


{-| Rotate a tree to the left (for balancing).

-}
rotateLeft : Tree k v -> Tree k v
rotateLeft set =
    case set of
        Node _ root rootVal less (Node _ pivot pivotVal between greater) ->
            tree pivot pivotVal (tree root rootVal less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Tree k v -> Tree k v
rotateRight set =
    case set of
        Node _ root rootVal (Node _ pivot pivotVal less between) greater ->
            tree pivot pivotVal less (tree root rootVal between greater)

        _ ->
            set


{-| Calculate the difference in height between our left and right branches.

A *valid* AVL tree has a maximal height difference of |1| over its branches,
which allows checking if a random element is in the tree in `O (log n)`.
-}
heightDiff : Tree k v -> Int
heightDiff set =
    case set of
        Empty ->
            0

        Node _ _ _ left right ->
            height right - height left


{-| Rebalances a tree (if it is, in fact, unbalanced).

If a tree becomes unbalanced by |2|, this restores the balance by rotating and
-- if a child is unbalanced in the opposite direction -- rotating the child in
the opposite direction.

For more information on how this works, please refer to [Brian Hick's excellent
series](https://www.brianthicks.com/post/2016/11/27/functional-sets-part-3-balancing/)
on implementing AVL trees in Elm.
-}
balance : Tree k v -> Tree k v
balance set =
    case set of
        Empty ->
            set

        Node _ head hVal left right ->
            if heightDiff set == -2 && heightDiff left > 0 then
                -- Node leaning to the left with subtree leaning to the right
                tree head hVal (rotateLeft left) right |> rotateRight
            else if heightDiff set < -1 then
                -- Node leaning to the left
                rotateRight set
            else if heightDiff set == 2 && heightDiff right < 0 then
                -- Node leaning to the right with subtree leaning left
                tree head hVal left (rotateRight right) |> rotateLeft
            else if heightDiff set > 1 then
                -- Node leaning to the right
                rotateLeft set
            else
                -- Balanced tree
                set



-- Helpers


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Tree comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Tree k v -> Int
size =
    foldl (\k v -> (+) 1) 0


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Tree comparable v -> Tree comparable v
filter predicate =
    foldl
        (\k v ->
            if predicate k v then
                insert k v
            else
                identity
        )
        empty
