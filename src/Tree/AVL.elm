module Tree.AVL exposing (..)

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

import Function exposing (swirlr)


{-| A node in an avl tree is either a node with one key and zero, one or two
branches, or is empty. Some trees (like Two Three trees) impose the restriction
that a node must be either a leaf node (having no children), or a completely
internal node, i.e. having all branch-points filled in by a non-empty node.

The left branch of a node contains values smaller than the node's own value, the
right branch contains values greater than the node's own value.
-}
type Tree k v
    = Node Int k v (Tree k v) (Tree k v)
    | Singleton k v
    | Empty



-- Basics: Creation


{-| A singleton in an AVL tree is a Node with both branch-points empty.
-}
singleton : k -> v -> Tree k v
singleton key val =
    Singleton key val


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

        Singleton head headVal ->
            case compare key head of
                LT ->
                    Node 2
                        head
                        headVal
                        (singleton key value)
                        empty
                        |> balance

                GT ->
                    Node 2
                        head
                        headVal
                        empty
                        (singleton key value)
                        |> balance

                EQ ->
                    Singleton head value

        Node height head headVal left right ->
            case compare key head of
                LT ->
                    tree head headVal (insert key value left) right |> balance

                GT ->
                    tree head headVal left (insert key value right) |> balance

                EQ ->
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

        Singleton head value ->
            if head == key then
                empty
            else
                set

        Node _ head headVal left right ->
            case compare key head of
                LT ->
                    tree head headVal (remove key left) right |> balance

                GT ->
                    tree head headVal left (remove key right) |> balance

                EQ ->
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

        Singleton head _ ->
            head == key

        Node _ head _ left right ->
            case compare key head of
                LT ->
                    member key left

                GT ->
                    member key right

                EQ ->
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

        Singleton head value ->
            if head == key then
                Just value
            else
                Nothing

        Node _ head value left right ->
            case compare key head of
                LT ->
                    get key left

                GT ->
                    get key right

                EQ ->
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

        Singleton key val ->
            op key val acc

        Node _ key val left right ->
            foldl op acc left
                |> op key val
                |> swirlr foldl right op


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr op acc tree =
    case tree of
        Empty ->
            acc

        Singleton key val ->
            op key val acc

        Node _ key val left right ->
            foldr op acc right
                |> op key val
                |> swirlr foldr left op



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
    case ( left, right ) of
        ( Empty, Empty ) ->
            singleton key value

        ( _, _ ) ->
            Node
                (max
                    (height left)
                    (height right)
                    |> (+) 1
                )
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

        Singleton _ _ ->
            1

        Node height _ _ _ _ ->
            height


{-| Rotate a tree to the left (for balancing).

-}
rotateLeft : Tree k v -> Tree k v
rotateLeft set =
    case set of
        Node _ root rootVal less (Node _ pivot pivotVal between greater) ->
            tree pivot pivotVal (tree root rootVal less between) greater

        Node _ root rootVal less (Singleton pivot pivotVal) ->
            tree pivot pivotVal (tree root rootVal less empty) empty

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Tree k v -> Tree k v
rotateRight set =
    case set of
        Node _ root rootVal (Node _ pivot pivotVal less between) greater ->
            tree pivot pivotVal less (tree root rootVal between greater)

        Node _ root rootVal (Singleton pivot pivotVal) greater ->
            tree pivot pivotVal empty (tree root rootVal empty greater)

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

        Singleton _ _ ->
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
balance : Tree comparable v -> Tree comparable v
balance set =
    case set of
        Empty ->
            set

        Singleton _ _ ->
            set

        Node _ key value left right ->
            let
                setDiff =
                    heightDiff set
            in
                if setDiff == -2 then
                    if heightDiff left == 1 then
                        -- left leaning tree with right-leaning left subtree. Rotate left, then right.
                        tree key value (rotateLeft left) right |> rotateRight
                    else
                        -- left leaning tree, generally. Rotate right.
                        rotateRight set
                else if setDiff == 2 then
                    if heightDiff right == -1 then
                        -- right leaning tree with left-leaning right subtree. Rotate right, then left.
                        tree key value left (rotateRight right) |> rotateLeft
                    else
                        -- right leaning tree, generally. Rotate left.
                        rotateLeft set
                else
                    -- diff is -1, 0, or 1. Already balanced, no operation required.
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
