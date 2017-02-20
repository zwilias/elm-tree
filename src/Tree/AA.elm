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
@docs insert, remove, member, foldl, foldr

# AA tree operations
@docs unsafeMinimum, unsafeMaximum, skew, split, rebalance, decreaseLevel, getLevel, setLevel, mapRight

-}

import Debug
import Function exposing (swirlr)


{-| A node in an AA tree can be either Empty, a branch node with one or two
child-trees, or a leaf-node. A leaf-node is represented as a branch-node with
both branch-points left empty.

Additionally, nodes hold an extra Int keeping track of the level.
-}
type Tree k v
    = Empty
    | Node Int (Tree k v) k v (Tree k v)


{-| Constructs an empty tree.
-}
empty : Tree k v
empty =
    Empty


{-| Constructs a tree with one leaf holding the provided value.
-}
singleton : k -> v -> Tree k v
singleton key value =
    Node 1 empty key value empty


get : comparable -> Tree comparable v -> Maybe v
get key tree =
    case tree of
        Empty ->
            Nothing

        Node _ left self value right ->
            if key < self then
                get key left
            else if key > self then
                get key right
            else
                Just value


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
insert : comparable -> v -> Tree comparable v -> Tree comparable v
insert key value tree =
    let
        fixup : Tree k v -> Tree k v
        fixup =
            skew >> split
    in
        case tree of
            Empty ->
                singleton key value

            Node level left selfk selfv right ->
                if key < selfk then
                    Node
                        level
                        (insert
                            key
                            value
                            left
                        )
                        selfk
                        selfv
                        right
                        |> fixup
                else if key == selfk then
                    Node
                        level
                        left
                        key
                        value
                        right
                else
                    Node
                        level
                        left
                        selfk
                        selfv
                        (insert
                            key
                            value
                            right
                        )
                        |> fixup


{-| Removal from AA trees works as follows:

- Recursively locate the node to be removed.
- If it's an internal node, substitute for its successor or predecessor.
- If it's leaf-node, replace by empty.
- While unwinding the stack, `rebalance` - fixing up levels and balance by skewing and splitting.
-}
remove : comparable -> Tree comparable v -> Tree comparable v
remove key tree =
    case tree of
        Empty ->
            Empty

        Node level left selfk selfv right ->
            if key < selfk then
                Node level (remove key left) selfk selfv right |> rebalance
            else if key == selfk then
                case ( left, right ) of
                    ( Empty, Empty ) ->
                        Empty

                    ( Empty, _ ) ->
                        let
                            ( successork, successorv ) =
                                unsafeMinimum right

                            newRight : Tree comparable v
                            newRight =
                                remove successork right
                        in
                            Node level left successork successorv newRight
                                |> rebalance

                    ( _, _ ) ->
                        let
                            ( predecessork, predecessorv ) =
                                unsafeMaximum left

                            newLeft : Tree comparable v
                            newLeft =
                                remove predecessork left
                        in
                            Node level newLeft predecessork predecessorv right
                                |> rebalance
            else
                Node level left selfk selfv (remove key right) |> rebalance


{-| Check if an item is a member of a tree, recursively.
-}
member : comparable -> Tree comparable v -> Bool
member item tree =
    case tree of
        Empty ->
            False

        Node _ left self _ right ->
            if item < self then
                member item left
            else if item == self then
                True
            else
                member item right


foldl : (k -> v -> a -> a) -> a -> Tree k v -> a
foldl op acc tree =
    case tree of
        Empty ->
            acc

        Node _ left key val right ->
            foldl op acc left
                |> op key val
                |> swirlr foldl right op


foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr op acc tree =
    case tree of
        Empty ->
            acc

        Node _ left key val right ->
            foldr op acc right
                |> op key val
                |> swirlr foldr left op



-- Internal functions


{-| Finds the minimal key/value pair in the provided tree. Crashes when called on an
Empty tree -- meant for internal use within delete where its used to find the
predecessor from an *internal* node, hence guaranteeing safety.
-}
unsafeMinimum : Tree k v -> ( k, v )
unsafeMinimum tree =
    case tree of
        Empty ->
            Debug.crash "Can't get minimal value of empty tree."

        Node _ Empty key value _ ->
            ( key, value )

        Node _ left _ _ _ ->
            unsafeMinimum left


{-| Finds the maximal value in the provided tree. Crashes when called on an
Empty tree -- meant for internal use within delete where its used to find the
successor from an *internal* node, hence guaranteeing safety.
-}
unsafeMaximum : Tree k v -> ( k, v )
unsafeMaximum tree =
    case tree of
        Empty ->
            Debug.crash "Can't get maximal value of empty tree."

        Node _ _ key value Empty ->
            ( key, value )

        Node _ _ _ _ right ->
            unsafeMaximum right


{-| Get the level of a node in a tree. For an Empty tree, this is always 0. For
every other node, this is a piece of information kept within the node.
-}
getLevel : Tree k v -> Int
getLevel tree =
    case tree of
        Empty ->
            0

        Node level _ _ _ _ ->
            level


{-| Skewing is a right rotation to replace a subtree containing a
left-horizontal link with one containing a right horizontal link.

![Courtesy of wikipedia](https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/AA_Tree_Skew2.svg/560px-AA_Tree_Skew2.svg.png)
-}
skew : Tree k v -> Tree k v
skew tree =
    case tree of
        Node level (Node lLevel lLeft lKey lVal lRight) key val right ->
            if level == lLevel then
                Node level lRight key val right
                    |> Node level lLeft lKey lVal
            else
                tree

        _ ->
            tree


{-| Split is a left rotation and level increase to replace a subtree containing
two or more consecutive right horizontal links with one containing two fewer
consecutive right horizontal links.

![Courtesy of wikipedia](https://upload.wikimedia.org/wikipedia/commons/thumb/0/0e/AA_Tree_Split2.svg/510px-AA_Tree_Split2.svg.png)
-}
split : Tree k v -> Tree k v
split tree =
    case tree of
        Node level left key val (Node rLevel rLeft rKey rVal rRight) ->
            if level == rLevel && level == getLevel rRight then
                Node
                    (level + 1)
                    (Node level left key val rLeft)
                    rKey
                    rVal
                    rRight
            else
                tree

        _ ->
            tree


{-| Executes the provided operation on the right subtree and return the tree
including the mapped result. Helper function of the skew/split operations
required for rebalancing after removal.
-}
mapRight : (Tree k v -> Tree k v) -> Tree k v -> Tree k v
mapRight op tree =
    case tree of
        Empty ->
            Empty

        Node level left key value right ->
            Node level left key value (op right)


{-| Sets the level in the provided tree to the provided value.
-}
setLevel : Int -> Tree k v -> Tree k v
setLevel level tree =
    case tree of
        Empty ->
            Empty

        Node _ left key value right ->
            Node level left key value right


{-| Decrease the level in the tree and subnodes to restore the AA invariants,
if required.
-}
decreaseLevel : Tree k v -> Tree k v
decreaseLevel tree =
    case tree of
        Empty ->
            Empty

        Node level left key value right ->
            if level > getLevel left + 1 || level > getLevel right + 1 then
                let
                    shouldBe =
                        level - 1

                    newRight =
                        if getLevel right > shouldBe then
                            setLevel shouldBe right
                        else
                            right
                in
                    Node shouldBe left key value newRight
            else
                tree


{-| Rebalance the tree. This is a three-step process:

- Decrease the level
- Skew the tree
- Split the tree

-}
rebalance : Tree k v -> Tree k v
rebalance =
    let
        skewTree : Tree k v -> Tree k v
        skewTree =
            skew >> mapRight skew >> mapRight (mapRight skew)

        splitTree : Tree k v -> Tree k v
        splitTree =
            split >> mapRight split
    in
        decreaseLevel >> skewTree >> splitTree



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
