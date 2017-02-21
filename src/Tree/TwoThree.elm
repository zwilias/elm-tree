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
@docs InsertionResult, insert, DeletionResult, remove, member, size, foldl, foldr, get

# Fold-based operations
@docs filter, fromList, remove

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

        TwoNode left self value right ->
            if key < self then
                get key left
            else if key > self then
                get key right
            else
                Just value

        ThreeNode lower lKey lVal between rKey rVal greater ->
            if key < lKey then
                get key lower
            else if key == lKey then
                Just lVal
            else if key < rKey then
                get key between
            else if key == rKey then
                Just rVal
            else
                get key greater


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
                        TwoNode Empty self val Empty
                            |> Consumed
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
                        ThreeNode Empty left val Empty right rVal Empty
                            |> Consumed
                    else if key < right then
                        Pushed (singleton left lVal) key val (singleton right rVal)
                    else if key == right then
                        ThreeNode Empty left lVal Empty right val Empty
                            |> Consumed
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
                    else if key == self then
                        TwoNode lower self val greater
                            |> Consumed
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
                    else if key == left then
                        ThreeNode lower key val between right rVal greater
                            |> Consumed
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
                    else if key == right then
                        ThreeNode lower left lVal between key val greater
                            |> Consumed
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
    in
        case doInsertion key val tree of
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


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
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


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> a -> a) -> a -> Tree k v -> a
foldr operation acc tree =
    case tree of
        Empty ->
            acc

        TwoNode lower self sVal greater ->
            foldr operation acc greater
                |> operation self sVal
                |> swirlr foldr lower operation

        ThreeNode lower left lVal between right rVal greater ->
            foldr operation acc greater
                |> operation right rVal
                |> swirlr foldr between operation
                |> operation left lVal
                |> swirlr foldr lower operation



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
