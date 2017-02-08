module Tree.TwoThree exposing (..)

{-| 2-3 trees are B-trees of order 3 (using Knuth's definition of order as
the maximum amount of children a node may have). Rather than having exactly one
left and one right hand branch - as regular BST's do, 2-3 trees can have nodes
with *2* values and *3* children. As a result, a 2-3 tree can be perfectly
height balanced over all its nodes.

In fact, that is the exact invariant that holds for 2-3 trees:

> All children of each node *always* have the same height.

Conceptually, 2-3 trees are much simpler than AVL or AA trees:

- No rotation required
- Balance is maintained by absorption ro remove unwanted nodes and splitting to
eliminate 4-nodes

Implementation is hairy, though.

# Types
@docs Tree

# Creation
@docs empty, singleton

# Basic operations
@docs InsertionResult, insert, DeletionResult, remove, member, size, foldl, foldr

# Fold-based operations
@docs map, filter, toList, fromList, union, remove, intersect, diff, partition

# Debugging
@docs debugMode, trace

-}

import Debug
import Function exposing (swirlr)


--


{-| Whether or not debug mode is enabled. This is mostly for convenience,
allowing the deletion algorithm to log every step it goes through.
-}
debugMode : Bool
debugMode =
    False


{-| `debugMode` aware wrapper for `Debug.log`
-}
trace : String -> a -> a
trace msg =
    if debugMode then
        Debug.log msg
    else
        identity


{-| A two-three tree has nodes that have either one value and *two* children,
or two values and *three* children. Hence, the name. Of particular note is how
this configuration allows for a perfectly balanced tree, at the cost of having
to deal with slightly more complicated rebalancing operations when deleting
nodes. As with all our trees so far, we include an explicit sentinel node to
mark empty branch-points (which denote leaf nodes)
-}
type Tree comparable
    = Empty
    | TwoNode (Tree comparable) comparable (Tree comparable)
    | ThreeNode (Tree comparable) comparable (Tree comparable) comparable (Tree comparable)


{-| Creates an empty tree.
-}
empty : Tree comparable
empty =
    Empty


{-| Creates a singleton tree with the provided value.
-}
singleton : comparable -> Tree comparable
singleton item =
    TwoNode empty item empty


{-| A types used to handle the different cases that may arise during `remove`
operations, and that need specific handling while unwinding the stack.
-}
type
    DeletionResult comparable
    -- NotFound means simply that the node could not be removed, for it does not exists
    = NotFound
      -- Orphaned means the subtree became one level less deep. This needs taking care of recursively, in order to restore the "all leaves at the same level" invariant
    | Merged (Tree comparable)
      -- The simplest of cases - a subtree was replaced by something with an equivalent amount of levels, which means the break of invariants has been contained, and the replacement can simply bubble up
    | Replaced (Tree comparable)


{-| Remove a value from the tree.

- In case it's a leaf-node, there are 2 options:
    - Single-value leaf-node: this means the node is completely removed, hence
    changing the depth of this branch. This might require changes further up
    the tree in order to restore the 2-3 invariant of being perfectly height
    balanced.
    - Dual-value leaf-node: this is the easiest scenario - the leaf-node is
    replaced by a single-value leaf-node. Done, and no further rebalancing
    required.
- In case it's an internal node, the value to be removed is swapped with its
successor or predecessor and the algorithm is followed as if it were a
leaf-node.
- In case the value does not occur in the tree, nothing needs to happen.
-}
remove : comparable -> Tree comparable -> Tree comparable
remove item tree =
    let
        tag : DeletionResult comparable -> DeletionResult comparable
        tag result =
            let
                logString : String
                logString =
                    case result of
                        NotFound ->
                            "Node not found, bubbling up"

                        Merged mergeTree ->
                            case mergeTree of
                                Empty ->
                                    "Merged leaf-node with single key into empty, leaving a hole for out parents to fill"

                                TwoNode _ self _ ->
                                    "Merged into node with key: " ++ toString self

                                ThreeNode _ left _ right _ ->
                                    "Merged into node with left: " ++ toString left ++ " and right: " ++ toString right

                        Replaced replacement ->
                            case replacement of
                                Empty ->
                                    "Replaced by empty, bubbling replacement up"

                                TwoNode _ self _ ->
                                    "Replaced by node with key: " ++ toString self

                                ThreeNode _ left _ right _ ->
                                    "Replaced by node with left: " ++ toString left ++ " and right: " ++ toString right
            in
                trace logString result

        findNextLarger : comparable -> Tree comparable -> comparable
        findNextLarger item tree =
            let
                tag : comparable -> comparable
                tag =
                    trace "Removing an internal node is hard, so substituting for the next larger value."
            in
                case tree of
                    Empty ->
                        Debug.crash "Empty leaf at this point means the invariants were not maintainted."

                    TwoNode Empty self _ ->
                        if self <= item then
                            Debug.crash "Found smaller or equal item in right hand branch. Invariants not maintained."
                        else
                            tag self

                    ThreeNode Empty left _ _ _ ->
                        if left <= item then
                            Debug.crash "Found smaller or equal item in right hand branch. Invariants not maintained."
                        else
                            tag left

                    TwoNode lower _ _ ->
                        findNextLarger item lower

                    ThreeNode lower _ _ _ _ ->
                        findNextLarger item lower

        taggedRemove : comparable -> Tree comparable -> DeletionResult comparable
        taggedRemove item tree =
            doRemove item tree |> tag

        doRemove : comparable -> Tree comparable -> DeletionResult comparable
        doRemove item tree =
            case tree of
                -- If its an empty tree, we can't possibly contain the value
                -- you're looking for
                Empty ->
                    NotFound

                -- Leafnode with 1 key: either it is us and we're "orphaning" an empty subtree (which means we're creating a hole in the tree that must somehow be filled), or it does not exists
                TwoNode Empty self Empty ->
                    if self == item then
                        Merged empty
                    else
                        NotFound

                -- Leafnode with 2 keys: Either punching a hole that can be filled by simply replacing with a singleton, or not found
                ThreeNode Empty left Empty right Empty ->
                    if left == item then
                        singleton right
                            |> Replaced
                    else if right == item then
                        singleton left
                            |> Replaced
                    else
                        NotFound

                -- internal node with 1 key: If our key is the key to be removed, look instead for the next larget node in the right-hand side of the tree, and "act" as if our node is located there
                TwoNode lower self greater ->
                    if item < self then
                        case taggedRemove item lower of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                -- This needs careful handling. Either our right hand side is a threenode and has a node we can borrow to restore the balance here, or it's a two node, which means we Merge with the right hand side, creating a 3 node and let our parents deal with it.
                                case greater of
                                    Empty ->
                                        Debug.crash "Invariant broken"

                                    ThreeNode sLower sLeft sBetween sRight sGreater ->
                                        TwoNode
                                            (TwoNode
                                                mergeTree
                                                self
                                                sLower
                                            )
                                            sLeft
                                            (TwoNode
                                                sBetween
                                                sRight
                                                sGreater
                                            )
                                            |> Replaced

                                    TwoNode sLower sSelf sGreater ->
                                        ThreeNode
                                            mergeTree
                                            self
                                            sLower
                                            sSelf
                                            sGreater
                                            |> Merged

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode replacement self greater
                                    |> Replaced
                    else if item == self then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item greater
                        in
                            -- We're an internal node, so we need to cheat a bit and remove the next larger (or lower, but we chose larger.) leafnode, taking care to re-insert that node and forget all about our self.
                            case taggedRemove nextLarger greater of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case lower of
                                        Empty ->
                                            Debug.crash "Invariant not maintained"

                                        TwoNode sLower sSelf sGreater ->
                                            ThreeNode
                                                sLower
                                                sSelf
                                                sGreater
                                                nextLarger
                                                mergeTree
                                                |> Merged

                                        ThreeNode sLower sLeft sBetween sRight sGreater ->
                                            TwoNode
                                                (TwoNode sLower sLeft sBetween)
                                                sRight
                                                (TwoNode sGreater nextLarger mergeTree)
                                                |> Replaced

                                Replaced replacement ->
                                    TwoNode
                                        lower
                                        nextLarger
                                        replacement
                                        |> Replaced
                    else
                        case taggedRemove item greater of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                case lower of
                                    Empty ->
                                        Debug.crash "not possible"

                                    TwoNode sLower sSelf sGreater ->
                                        ThreeNode
                                            sLower
                                            sSelf
                                            sGreater
                                            self
                                            mergeTree
                                            |> Merged

                                    ThreeNode sLower sLeft sBetween sRight sGreater ->
                                        TwoNode
                                            (TwoNode sLower sLeft sBetween)
                                            sRight
                                            (TwoNode sGreater self mergeTree)
                                            |> Replaced

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode lower self replacement
                                    |> Replaced

                ThreeNode lower left between right greater ->
                    if item < left then
                        case taggedRemove item lower of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                case between of
                                    Empty ->
                                        Debug.crash "Not possible"

                                    TwoNode bLower bSelf bGreater ->
                                        case greater of
                                            Empty ->
                                                Debug.crash "Not possible"

                                            TwoNode gLower gSelf gGreater ->
                                                TwoNode
                                                    (TwoNode mergeTree left bLower)
                                                    bSelf
                                                    (ThreeNode
                                                        bGreater
                                                        right
                                                        gLower
                                                        gSelf
                                                        gGreater
                                                    )
                                                    |> Replaced

                                            ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                ThreeNode
                                                    (TwoNode mergeTree left bLower)
                                                    bSelf
                                                    (TwoNode bGreater right gLower)
                                                    gLeft
                                                    (TwoNode gBetween gRight gGreater)
                                                    |> Replaced

                                    ThreeNode bLower bLeft bBetween bRight bGreater ->
                                        ThreeNode
                                            (TwoNode mergeTree left bLower)
                                            bLeft
                                            (TwoNode bBetween bRight bGreater)
                                            right
                                            greater
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    replacement
                                    left
                                    between
                                    right
                                    greater
                                    |> Replaced
                    else if item == left then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item between
                        in
                            case taggedRemove nextLarger between of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case lower of
                                        Empty ->
                                            Debug.crash "nein"

                                        TwoNode lLower lSelf lGreater ->
                                            case greater of
                                                Empty ->
                                                    Debug.crash "nein"

                                                TwoNode gLower gSelf gGreater ->
                                                    TwoNode
                                                        (ThreeNode
                                                            lLower
                                                            lSelf
                                                            lGreater
                                                            nextLarger
                                                            mergeTree
                                                        )
                                                        right
                                                        greater
                                                        |> Replaced

                                                ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                    ThreeNode
                                                        lower
                                                        nextLarger
                                                        (TwoNode mergeTree right gLower)
                                                        gLeft
                                                        (TwoNode gBetween gRight gGreater)
                                                        |> Replaced

                                        ThreeNode lLower lLeft lBetween lRight lGreater ->
                                            ThreeNode
                                                (TwoNode lLower lLeft lBetween)
                                                lRight
                                                (TwoNode lGreater left mergeTree)
                                                right
                                                greater
                                                |> Replaced

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        nextLarger
                                        replacement
                                        right
                                        greater
                                        |> Replaced
                    else if item < right then
                        case taggedRemove item between of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                case lower of
                                    Empty ->
                                        Debug.crash "nein"

                                    TwoNode lLower lSelf lGreater ->
                                        case greater of
                                            Empty ->
                                                Debug.crash "nein"

                                            TwoNode gLower gSelf gGreater ->
                                                TwoNode
                                                    (ThreeNode
                                                        lLower
                                                        lSelf
                                                        lGreater
                                                        left
                                                        mergeTree
                                                    )
                                                    right
                                                    greater
                                                    |> Replaced

                                            ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                ThreeNode
                                                    lower
                                                    left
                                                    (TwoNode mergeTree right gLower)
                                                    gLeft
                                                    (TwoNode gBetween gRight gGreater)
                                                    |> Replaced

                                    ThreeNode lLower lLeft lBetween lRight lGreater ->
                                        ThreeNode
                                            (TwoNode lLower lLeft lBetween)
                                            lRight
                                            (TwoNode lGreater left mergeTree)
                                            right
                                            greater
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    replacement
                                    right
                                    greater
                                    |> Replaced
                    else if item == right then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item greater
                        in
                            case taggedRemove nextLarger greater of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case between of
                                        Empty ->
                                            Debug.crash "nope"

                                        TwoNode bLower bSelf bGreater ->
                                            case lower of
                                                Empty ->
                                                    Debug.crash "still, nope"

                                                TwoNode lLower lSelf lGreater ->
                                                    TwoNode
                                                        (ThreeNode
                                                            lLower
                                                            lSelf
                                                            lGreater
                                                            left
                                                            bLower
                                                        )
                                                        bSelf
                                                        (TwoNode bGreater nextLarger mergeTree)
                                                        |> Replaced

                                                ThreeNode lLower lLeft lBetween lRight lGreater ->
                                                    ThreeNode
                                                        (TwoNode lLower lLeft lBetween)
                                                        lRight
                                                        (TwoNode lGreater left bLower)
                                                        bSelf
                                                        (TwoNode bGreater nextLarger mergeTree)
                                                        |> Replaced

                                        ThreeNode bLower bLeft bBetween bRight bGreater ->
                                            ThreeNode
                                                lower
                                                left
                                                (TwoNode bLower bLeft bBetween)
                                                bRight
                                                (TwoNode bGreater nextLarger mergeTree)
                                                |> Replaced

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        left
                                        between
                                        nextLarger
                                        replacement
                                        |> Replaced
                    else
                        case taggedRemove item greater of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                case between of
                                    Empty ->
                                        Debug.crash "nope"

                                    TwoNode bLower bSelf bGreater ->
                                        case lower of
                                            Empty ->
                                                Debug.crash "still, nope"

                                            TwoNode lLower lSelf lGreater ->
                                                TwoNode
                                                    (ThreeNode
                                                        lLower
                                                        lSelf
                                                        lGreater
                                                        left
                                                        bLower
                                                    )
                                                    bSelf
                                                    (TwoNode bGreater right mergeTree)
                                                    |> Replaced

                                            ThreeNode lLower lLeft lBetween lRight lGreater ->
                                                ThreeNode
                                                    (TwoNode lLower lLeft lBetween)
                                                    lRight
                                                    (TwoNode lGreater left bLower)
                                                    bSelf
                                                    (TwoNode bGreater right mergeTree)
                                                    |> Replaced

                                    ThreeNode bLower bLeft bBetween bRight bGreater ->
                                        ThreeNode
                                            lower
                                            left
                                            (TwoNode bLower bLeft bBetween)
                                            bRight
                                            (TwoNode bGreater right mergeTree)
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    between
                                    right
                                    replacement
                                    |> Replaced
    in
        case taggedRemove (trace "Attempting to remove " item) tree of
            NotFound ->
                tree

            Replaced newTree ->
                newTree

            Merged newTree ->
                newTree


{-| Similar to the DeletionResult ADT, the InsertionResult ADT is used to keep
track of operations down the tree, and handling them as required after
insertion, in order to maintain the invariant.
-}
type InsertionResult comparable
    = Consumed (Tree comparable)
    | Pushed (Tree comparable) comparable (Tree comparable)
    | AlreadyExists


{-| Insertion is, albeit conceptually simple, in practice fairly complex due to
the many variations that need to be considered.
-}
insert : comparable -> Tree comparable -> Tree comparable
insert item tree =
    let
        doInsertion : comparable -> Tree comparable -> InsertionResult comparable
        doInsertion item tree =
            case tree of
                -- Empty tree results in a singleton
                Empty ->
                    singleton item |> Consumed

                -- Twonode with empty branches -> fill in a branch
                TwoNode Empty self Empty ->
                    if item < self then
                        ThreeNode Empty item Empty self Empty
                            |> Consumed
                    else if item == self then
                        AlreadyExists
                    else
                        ThreeNode Empty self Empty item Empty
                            |> Consumed

                -- ThreeNode with empty branches
                ThreeNode Empty left Empty right Empty ->
                    if item < left then
                        -- This forces us into a 4-node, so we split and pass a new root to our parent, which will then consume if (or split, too)
                        Pushed (singleton item) left (singleton right)
                    else if item == left then
                        -- It's me, it's me!
                        AlreadyExists
                    else if item < right then
                        Pushed (singleton left) item (singleton right)
                    else if item == right then
                        AlreadyExists
                    else
                        Pushed (singleton left) right (singleton item)

                -- TwoNode with actual branches -> potentially need to split
                TwoNode lower self greater ->
                    if item < self then
                        case doInsertion item lower of
                            -- If the subtree needed no split, we consume, too
                            Consumed newLower ->
                                TwoNode
                                    newLower
                                    self
                                    greater
                                    |> Consumed

                            -- Subtree needed a split, so we consume that split
                            Pushed newLower newLeft newMiddle ->
                                ThreeNode
                                    newLower
                                    newLeft
                                    newMiddle
                                    self
                                    greater
                                    |> Consumed

                            AlreadyExists ->
                                AlreadyExists
                    else if item == self then
                        AlreadyExists
                    else
                        case doInsertion item greater of
                            -- If the subtree needed no split, we consume, too
                            Consumed newGreater ->
                                TwoNode
                                    lower
                                    self
                                    newGreater
                                    |> Consumed

                            -- Subtree split, so we consume the new value
                            Pushed newMiddle newRight newGreater ->
                                ThreeNode
                                    lower
                                    self
                                    newMiddle
                                    newRight
                                    newGreater
                                    |> Consumed

                            AlreadyExists ->
                                AlreadyExists

                ThreeNode lower left between right greater ->
                    if item < left then
                        case doInsertion item lower of
                            Consumed newLower ->
                                ThreeNode newLower left between right greater
                                    |> Consumed

                            Pushed newLower newSelf newGreater ->
                                Pushed
                                    (TwoNode newLower newSelf newGreater)
                                    left
                                    (TwoNode between right greater)

                            AlreadyExists ->
                                AlreadyExists
                    else if item == left then
                        AlreadyExists
                    else if item < right then
                        case doInsertion item between of
                            Consumed newBetween ->
                                ThreeNode lower left newBetween right greater
                                    |> Consumed

                            Pushed newLower newSelf newGreater ->
                                Pushed
                                    (TwoNode lower left newLower)
                                    newSelf
                                    (TwoNode newGreater right greater)

                            AlreadyExists ->
                                AlreadyExists
                    else if item == right then
                        AlreadyExists
                    else
                        case doInsertion item greater of
                            Consumed newGreater ->
                                ThreeNode lower left between right newGreater
                                    |> Consumed

                            Pushed newLower newSelf newGreater ->
                                Pushed
                                    (TwoNode lower left between)
                                    right
                                    (TwoNode newLower newSelf newGreater)

                            AlreadyExists ->
                                AlreadyExists
    in
        case doInsertion item tree of
            AlreadyExists ->
                tree

            Consumed byTree ->
                byTree

            Pushed smaller self higher ->
                TwoNode smaller self higher


{-| Recursively check if a given value is a member of the tree.
-}
member : comparable -> Tree comparable -> Bool
member item tree =
    case tree of
        Empty ->
            False

        TwoNode lower self greater ->
            if item < self then
                member item lower
            else if item == self then
                True
            else
                member item greater

        ThreeNode lower left between right greater ->
            if item < left then
                member item lower
            else if item == left then
                True
            else if item < right then
                member item between
            else if item == right then
                True
            else
                member item greater


{-| Recursively fold over values in the tree, depth first, left to right.
-}
foldl : (comparable -> a -> a) -> a -> Tree comparable -> a
foldl operation acc tree =
    case tree of
        Empty ->
            acc

        TwoNode lower self greater ->
            foldl operation acc lower
                |> operation self
                |> swirlr foldl greater operation

        ThreeNode lower left between right greater ->
            foldl operation acc lower
                |> operation left
                |> swirlr foldl between operation
                |> operation right
                |> swirlr foldl greater operation


{-| Recursively fold over values in the tree, depth first, right to left.
-}
foldr : (comparable -> a -> a) -> a -> Tree comparable -> a
foldr operation acc tree =
    case tree of
        Empty ->
            acc

        TwoNode lower self greater ->
            foldl operation acc greater
                |> operation self
                |> swirlr foldl lower operation

        ThreeNode lower left between right greater ->
            foldl operation acc greater
                |> operation left
                |> swirlr foldl between operation
                |> operation right
                |> swirlr foldl lower operation



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


{-| Fold over the tree, executing the specified operation on each value, and
accumulating these values into a new tree.
-}
map : (comparable -> comparable2) -> Tree comparable -> Tree comparable2
map operator =
    foldl
        (insert << operator)
        empty
