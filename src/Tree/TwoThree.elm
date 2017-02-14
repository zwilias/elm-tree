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
@docs filter, toList, fromList, union, remove, keys, values

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
type Tree k v
    = Empty
    | TwoNode (Tree k v) k v (Tree k v)
    | ThreeNode (Tree k v) k v (Tree k v) k v (Tree k v)


{-| Creates an empty tree.
-}
empty : Tree k v
empty =
    Empty


{-| Creates a singleton tree with the provided value.
-}
singleton : comparable -> v -> Tree comparable v
singleton key value =
    TwoNode empty key value empty


{-| A types used to handle the different cases that may arise during `remove`
operations, and that need specific handling while unwinding the stack.
-}
type
    DeletionResult k v
    -- NotFound means simply that the node could not be removed, for it does not exists
    = NotFound
      -- Orphaned means the subtree became one level less deep. This needs taking care of recursively, in order to restore the "all leaves at the same level" invariant
    | Merged (Tree k v)
      -- The simplest of cases - a subtree was replaced by something with an equivalent amount of levels, which means the break of invariants has been contained, and the replacement can simply bubble up
    | Replaced (Tree k v)


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
remove : comparable -> Tree comparable v -> Tree comparable v
remove item tree =
    let
        tag : DeletionResult comparable v -> DeletionResult comparable v
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

                                TwoNode _ self _ _ ->
                                    "Merged into node with key: " ++ toString self

                                ThreeNode _ left _ _ right _ _ ->
                                    "Merged into node with left: " ++ toString left ++ " and right: " ++ toString right

                        Replaced replacement ->
                            case replacement of
                                Empty ->
                                    "Replaced by empty, bubbling replacement up"

                                TwoNode _ self _ _ ->
                                    "Replaced by node with key: " ++ toString self

                                ThreeNode _ left _ _ right _ _ ->
                                    "Replaced by node with left: " ++ toString left ++ " and right: " ++ toString right
            in
                trace logString result

        findNextLarger : comparable -> Tree comparable v -> ( comparable, v )
        findNextLarger item tree =
            let
                tag : a -> a
                tag =
                    trace "Removing an internal node is hard, so substituting for the next larger value."
            in
                case tree of
                    Empty ->
                        Debug.crash "Empty leaf at this point means the invariants were not maintainted."

                    TwoNode Empty key value _ ->
                        if key <= item then
                            Debug.crash "Found smaller or equal item in right hand branch. Invariants not maintained."
                        else
                            tag ( key, value )

                    ThreeNode Empty key value _ _ _ _ ->
                        if key <= item then
                            Debug.crash "Found smaller or equal item in right hand branch. Invariants not maintained."
                        else
                            tag ( key, value )

                    TwoNode lower _ _ _ ->
                        findNextLarger item lower

                    ThreeNode lower _ _ _ _ _ _ ->
                        findNextLarger item lower

        taggedRemove : comparable -> Tree comparable v -> DeletionResult comparable v
        taggedRemove item tree =
            doRemove item tree |> tag

        doRemove : comparable -> Tree comparable v -> DeletionResult comparable v
        doRemove item tree =
            case tree of
                -- If its an empty tree, we can't possibly contain the value
                -- you're looking for
                Empty ->
                    NotFound

                -- Leafnode with 1 key: either it is us and we're "orphaning" an empty subtree (which means we're creating a hole in the tree that must somehow be filled), or it does not exists
                TwoNode Empty key value Empty ->
                    if key == item then
                        Merged empty
                    else
                        NotFound

                -- Leafnode with 2 keys: Either punching a hole that can be filled by simply replacing with a singleton, or not found
                ThreeNode Empty left lVal Empty right rVal Empty ->
                    if left == item then
                        singleton right rVal
                            |> Replaced
                    else if right == item then
                        singleton left lVal
                            |> Replaced
                    else
                        NotFound

                -- internal node with 1 key: If our key is the key to be removed, look instead for the next larget node in the right-hand side of the tree, and "act" as if our node is located there
                TwoNode lower key val greater ->
                    if item < key then
                        case taggedRemove item lower of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                -- This needs careful handling. Either our right hand side is a threenode and has a node we can borrow to restore the balance here, or it's a two node, which means we Merge with the right hand side, creating a 3 node and let our parents deal with it.
                                case greater of
                                    Empty ->
                                        Debug.crash "Invariant broken"

                                    ThreeNode sLower sLeft sLVal sBetween sRight sRVal sGreater ->
                                        TwoNode
                                            (TwoNode
                                                mergeTree
                                                key
                                                val
                                                sLower
                                            )
                                            sLeft
                                            sLVal
                                            (TwoNode
                                                sBetween
                                                sRight
                                                sRVal
                                                sGreater
                                            )
                                            |> Replaced

                                    TwoNode sLower sSelf sVal sGreater ->
                                        ThreeNode
                                            mergeTree
                                            key
                                            val
                                            sLower
                                            sSelf
                                            sVal
                                            sGreater
                                            |> Merged

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode replacement key val greater
                                    |> Replaced
                    else if item == key then
                        let
                            ( nextKey, nextVal ) =
                                findNextLarger item greater
                        in
                            -- We're an internal node, so we need to cheat a bit and remove the next larger (or lower, but we chose larger.) leafnode, taking care to re-insert that node and forget all about our self.
                            case taggedRemove nextKey greater of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case lower of
                                        Empty ->
                                            Debug.crash "Invariant not maintained"

                                        TwoNode sLower sSelf sVal sGreater ->
                                            ThreeNode
                                                sLower
                                                sSelf
                                                sVal
                                                sGreater
                                                nextKey
                                                nextVal
                                                mergeTree
                                                |> Merged

                                        ThreeNode sLower sLeft sLVal sBetween sRight sRVal sGreater ->
                                            TwoNode
                                                (TwoNode sLower sLeft sLVal sBetween)
                                                sRight
                                                sRVal
                                                (TwoNode sGreater nextKey nextVal mergeTree)
                                                |> Replaced

                                Replaced replacement ->
                                    TwoNode
                                        lower
                                        nextKey
                                        nextVal
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

                                    TwoNode sLower sSelf sVal sGreater ->
                                        ThreeNode
                                            sLower
                                            sSelf
                                            sVal
                                            sGreater
                                            key
                                            val
                                            mergeTree
                                            |> Merged

                                    ThreeNode sLower sLeft sLVal sBetween sRight sRVal sGreater ->
                                        TwoNode
                                            (TwoNode sLower sLeft sLVal sBetween)
                                            sRight
                                            sRVal
                                            (TwoNode sGreater key val mergeTree)
                                            |> Replaced

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode lower key val replacement
                                    |> Replaced

                ThreeNode lower left lVal between right rVal greater ->
                    if item < left then
                        case taggedRemove item lower of
                            NotFound ->
                                NotFound

                            Merged mergeTree ->
                                case between of
                                    Empty ->
                                        Debug.crash "Not possible"

                                    TwoNode bLower bSelf bVal bGreater ->
                                        case greater of
                                            Empty ->
                                                Debug.crash "Not possible"

                                            TwoNode gLower gSelf gVal gGreater ->
                                                TwoNode
                                                    (TwoNode mergeTree left lVal bLower)
                                                    bSelf
                                                    bVal
                                                    (ThreeNode
                                                        bGreater
                                                        right
                                                        rVal
                                                        gLower
                                                        gSelf
                                                        gVal
                                                        gGreater
                                                    )
                                                    |> Replaced

                                            ThreeNode gLower gLeft gLVal gBetween gRight gRVal gGreater ->
                                                ThreeNode
                                                    (TwoNode mergeTree left lVal bLower)
                                                    bSelf
                                                    bVal
                                                    (TwoNode bGreater right rVal gLower)
                                                    gLeft
                                                    gLVal
                                                    (TwoNode gBetween gRight gRVal gGreater)
                                                    |> Replaced

                                    ThreeNode bLower bLeft bLVal bBetween bRight bRVal bGreater ->
                                        ThreeNode
                                            (TwoNode mergeTree left lVal bLower)
                                            bLeft
                                            bLVal
                                            (TwoNode bBetween bRight bRVal bGreater)
                                            right
                                            rVal
                                            greater
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    replacement
                                    left
                                    lVal
                                    between
                                    right
                                    rVal
                                    greater
                                    |> Replaced
                    else if item == left then
                        let
                            ( nextKey, nextVal ) =
                                findNextLarger item between
                        in
                            case taggedRemove nextKey between of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case lower of
                                        Empty ->
                                            Debug.crash "nein"

                                        TwoNode lLower lKey lVak lGreater ->
                                            case greater of
                                                Empty ->
                                                    Debug.crash "nein"

                                                TwoNode gLower gKey gVal gGreater ->
                                                    TwoNode
                                                        (ThreeNode
                                                            lLower
                                                            lKey
                                                            lVal
                                                            lGreater
                                                            nextKey
                                                            nextVal
                                                            mergeTree
                                                        )
                                                        right
                                                        rVal
                                                        greater
                                                        |> Replaced

                                                ThreeNode gLower gLeft gLVal gBetween gRight gRVal gGreater ->
                                                    ThreeNode
                                                        lower
                                                        nextKey
                                                        nextVal
                                                        (TwoNode mergeTree right rVal gLower)
                                                        gLeft
                                                        gLVal
                                                        (TwoNode gBetween gRight gRVal gGreater)
                                                        |> Replaced

                                        ThreeNode lLower lLeft lLVal lBetween lRight lRVal lGreater ->
                                            ThreeNode
                                                (TwoNode lLower lLeft lLVal lBetween)
                                                lRight
                                                lRVal
                                                (TwoNode lGreater left lVal mergeTree)
                                                right
                                                rVal
                                                greater
                                                |> Replaced

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        nextKey
                                        nextVal
                                        replacement
                                        right
                                        rVal
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

                                    TwoNode lLower lSelf lSVal lGreater ->
                                        case greater of
                                            Empty ->
                                                Debug.crash "nein"

                                            TwoNode _ _ _ _ ->
                                                TwoNode
                                                    (ThreeNode
                                                        lLower
                                                        lSelf
                                                        lSVal
                                                        lGreater
                                                        left
                                                        lVal
                                                        mergeTree
                                                    )
                                                    right
                                                    rVal
                                                    greater
                                                    |> Replaced

                                            ThreeNode gLower gLeft gLVal gBetween gRight gRVal gGreater ->
                                                ThreeNode
                                                    lower
                                                    left
                                                    lVal
                                                    (TwoNode mergeTree right rVal gLower)
                                                    gLeft
                                                    gLVal
                                                    (TwoNode gBetween gRight gRVal gGreater)
                                                    |> Replaced

                                    ThreeNode lLower lLeft lLVal lBetween lRight lRVal lGreater ->
                                        ThreeNode
                                            (TwoNode lLower lLeft lLVal lBetween)
                                            lRight
                                            lRVal
                                            (TwoNode lGreater left lVal mergeTree)
                                            right
                                            rVal
                                            greater
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    lVal
                                    replacement
                                    right
                                    rVal
                                    greater
                                    |> Replaced
                    else if item == right then
                        let
                            ( nextKey, nextVal ) =
                                findNextLarger item greater
                        in
                            case taggedRemove nextKey greater of
                                NotFound ->
                                    Debug.crash "We know it's there, just remove it, you can't tell me it wasn't found."

                                Merged mergeTree ->
                                    case between of
                                        Empty ->
                                            Debug.crash "nope"

                                        TwoNode bLower bSelf bSVal bGreater ->
                                            case lower of
                                                Empty ->
                                                    Debug.crash "still, nope"

                                                TwoNode lLower lSelf lSVal lGreater ->
                                                    TwoNode
                                                        (ThreeNode
                                                            lLower
                                                            lSelf
                                                            lSVal
                                                            lGreater
                                                            left
                                                            lVal
                                                            bLower
                                                        )
                                                        bSelf
                                                        bSVal
                                                        (TwoNode bGreater nextKey nextVal mergeTree)
                                                        |> Replaced

                                                ThreeNode lLower lLeft lLVal lBetween lRight lRVal lGreater ->
                                                    ThreeNode
                                                        (TwoNode lLower lLeft lLVal lBetween)
                                                        lRight
                                                        lRVal
                                                        (TwoNode lGreater left lVal bLower)
                                                        bSelf
                                                        bSVal
                                                        (TwoNode bGreater nextKey nextVal mergeTree)
                                                        |> Replaced

                                        ThreeNode bLower bLeft bLVal bBetween bRight bRVal bGreater ->
                                            ThreeNode
                                                lower
                                                left
                                                lVal
                                                (TwoNode bLower bLeft bLVal bBetween)
                                                bRight
                                                bRVal
                                                (TwoNode bGreater nextKey nextVal mergeTree)
                                                |> Replaced

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        left
                                        lVal
                                        between
                                        nextKey
                                        nextVal
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

                                    TwoNode bLower bSelf bSVal bGreater ->
                                        case lower of
                                            Empty ->
                                                Debug.crash "still, nope"

                                            TwoNode lLower lSelf lSVal lGreater ->
                                                TwoNode
                                                    (ThreeNode
                                                        lLower
                                                        lSelf
                                                        lSVal
                                                        lGreater
                                                        left
                                                        lVal
                                                        bLower
                                                    )
                                                    bSelf
                                                    bSVal
                                                    (TwoNode bGreater right rVal mergeTree)
                                                    |> Replaced

                                            ThreeNode lLower lLeft lLVal lBetween lRight lRVal lGreater ->
                                                ThreeNode
                                                    (TwoNode lLower lLeft lLVal lBetween)
                                                    lRight
                                                    lRVal
                                                    (TwoNode lGreater left lVal bLower)
                                                    bSelf
                                                    bSVal
                                                    (TwoNode bGreater right rVal mergeTree)
                                                    |> Replaced

                                    ThreeNode bLower bLeft bLVal bBetween bRight bRVal bGreater ->
                                        ThreeNode
                                            lower
                                            left
                                            lVal
                                            (TwoNode bLower bLeft bLVal bBetween)
                                            bRight
                                            bRVal
                                            (TwoNode bGreater right rVal mergeTree)
                                            |> Replaced

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    lVal
                                    between
                                    right
                                    rVal
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
type InsertionResult k v
    = Consumed (Tree k v)
    | Pushed (Tree k v) k v (Tree k v)
    | AlreadyExists


{-| Insertion is, albeit conceptually simple, in practice fairly complex due to
the many variations that need to be considered.
-}
insert : comparable -> v -> Tree comparable v -> Tree comparable v
insert key val tree =
    let
        doInsertion : comparable -> v -> Tree comparable v -> InsertionResult comparable v
        doInsertion key val tree =
            case tree of
                -- Empty tree results in a singleton
                Empty ->
                    singleton key val |> Consumed

                -- Twonode with empty branches -> fill in a branch
                TwoNode Empty self sVal Empty ->
                    if key < self then
                        ThreeNode Empty key val Empty self sVal Empty
                            |> Consumed
                    else if key == self then
                        AlreadyExists
                    else
                        ThreeNode Empty self sVal Empty key val Empty
                            |> Consumed

                -- ThreeNode with empty branches
                ThreeNode Empty left lVal Empty right rVal Empty ->
                    if key < left then
                        -- This forces us into a 4-node, so we split and pass a new root to our parent, which will then consume if (or split, too)
                        Pushed (singleton key val) left lVal (singleton right rVal)
                    else if key == left then
                        -- It's me, it's me!
                        AlreadyExists
                    else if key < right then
                        Pushed (singleton left lVal) key val (singleton right rVal)
                    else if key == right then
                        AlreadyExists
                    else
                        Pushed (singleton left lVal) right rVal (singleton key val)

                -- TwoNode with actual branches -> potentially need to split
                TwoNode lower self sVal greater ->
                    if key < self then
                        case doInsertion key val lower of
                            -- If the subtree needed no split, we consume, too
                            Consumed newLower ->
                                TwoNode
                                    newLower
                                    self
                                    sVal
                                    greater
                                    |> Consumed

                            -- Subtree needed a split, so we consume that split
                            Pushed newLower newLeft newLeftVal newMiddle ->
                                ThreeNode
                                    newLower
                                    newLeft
                                    newLeftVal
                                    newMiddle
                                    self
                                    sVal
                                    greater
                                    |> Consumed

                            AlreadyExists ->
                                AlreadyExists
                    else if key == self then
                        AlreadyExists
                    else
                        case doInsertion key val greater of
                            -- If the subtree needed no split, we consume, too
                            Consumed newGreater ->
                                TwoNode
                                    lower
                                    self
                                    sVal
                                    newGreater
                                    |> Consumed

                            -- Subtree split, so we consume the new value
                            Pushed newMiddle newRight newRightVal newGreater ->
                                ThreeNode
                                    lower
                                    self
                                    sVal
                                    newMiddle
                                    newRight
                                    newRightVal
                                    newGreater
                                    |> Consumed

                            AlreadyExists ->
                                AlreadyExists

                ThreeNode lower left lVal between right rVal greater ->
                    if key < left then
                        case doInsertion key val lower of
                            Consumed newLower ->
                                ThreeNode newLower left lVal between right rVal greater
                                    |> Consumed

                            Pushed newLower newSelf newSelfVal newGreater ->
                                Pushed
                                    (TwoNode newLower newSelf newSelfVal newGreater)
                                    left
                                    lVal
                                    (TwoNode between right rVal greater)

                            AlreadyExists ->
                                AlreadyExists
                    else if key == left then
                        AlreadyExists
                    else if key < right then
                        case doInsertion key val between of
                            Consumed newBetween ->
                                ThreeNode lower left lVal newBetween right rVal greater
                                    |> Consumed

                            Pushed newLower newSelf newSelfVal newGreater ->
                                Pushed
                                    (TwoNode lower left lVal newLower)
                                    newSelf
                                    newSelfVal
                                    (TwoNode newGreater right rVal greater)

                            AlreadyExists ->
                                AlreadyExists
                    else if key == right then
                        AlreadyExists
                    else
                        case doInsertion key val greater of
                            Consumed newGreater ->
                                ThreeNode lower left lVal between right rVal newGreater
                                    |> Consumed

                            Pushed newLower newSelf newSelfVal newGreater ->
                                Pushed
                                    (TwoNode lower left lVal between)
                                    right
                                    rVal
                                    (TwoNode newLower newSelf newSelfVal newGreater)

                            AlreadyExists ->
                                AlreadyExists
    in
        case doInsertion key val tree of
            AlreadyExists ->
                tree

            Consumed byTree ->
                byTree

            Pushed smaller self selfVal higher ->
                TwoNode smaller self selfVal higher


{-| Recursively check if a given value is a member of the tree.
-}
member : comparable -> Tree comparable v -> Bool
member item tree =
    case tree of
        Empty ->
            False

        TwoNode lower self _ greater ->
            if item < self then
                member item lower
            else if item == self then
                True
            else
                member item greater

        ThreeNode lower left _ between right _ greater ->
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
foldl : (k -> v -> a -> a) -> a -> Tree k v -> a
foldl operation acc tree =
    case tree of
        Empty ->
            acc

        TwoNode lower self sVal greater ->
            foldl operation acc lower
                |> operation self sVal
                |> swirlr foldl greater operation

        ThreeNode lower left lVal between right rVal greater ->
            foldl operation acc lower
                |> operation left lVal
                |> swirlr foldl between operation
                |> operation right rVal
                |> swirlr foldl greater operation


{-| Recursively fold over values in the tree, depth first, right to left.
-}
foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr operation acc tree =
    case tree of
        Empty ->
            acc

        TwoNode lower self sVal greater ->
            foldl operation acc greater
                |> operation self sVal
                |> swirlr foldl lower operation

        ThreeNode lower left lVal between right rVal greater ->
            foldl operation acc greater
                |> operation right rVal
                |> swirlr foldl between operation
                |> operation left lVal
                |> swirlr foldl lower operation



-- Fold-based operations


{-| Convert tree to list in ascending order, using foldl.
-}
keys : Tree k v -> List k
keys =
    foldr (\key val -> (::) key) []


{-|
-}
values : Tree k v -> List v
values =
    foldr (\key val -> (::) val) []


{-| Create tree from list by folding over the list and inserting into an
initially empty tree.
-}
fromList : List ( comparable, v ) -> Tree comparable v
fromList =
    List.foldl (uncurry insert) empty


{-| Tree to list of key-value pairs
-}
toList : Tree k v -> List ( k, v )
toList =
    foldr ((\k v -> (::) ( k, v ))) []


{-| Foldl over the list and incrementing an accumulator by one for each value
that passes through the accumulator operation.
-}
size : Tree k v -> Int
size =
    foldl (\_ _ acc -> acc + 1) 0


{-| Union is implemented by folding over the second list and inserting it into
the first list.
-}
union : Tree comparable v -> Tree comparable v -> Tree comparable v
union =
    foldl insert


{-|
-}
filter : (comparable -> v -> Bool) -> Tree comparable v -> Tree comparable v
filter predicate =
    foldl
        (\key val ->
            if predicate key val then
                insert key val
            else
                identity
        )
        empty
