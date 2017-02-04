module Tree.TwoThree exposing (..)

{-| https://en.wikipedia.org/wiki/2%E2%80%933_tree
-}

import Debug


type Tree comparable
    = Empty
    | TwoNode (Tree comparable) comparable (Tree comparable)
    | ThreeNode (Tree comparable) comparable (Tree comparable) comparable (Tree comparable)


empty : Tree comparable
empty =
    Empty


isEmpty : Tree comparable -> Bool
isEmpty tree =
    tree == Empty


singleton : comparable -> Tree comparable
singleton item =
    TwoNode empty item empty


type
    DeletionResult comparable
    -- NotFound means simply that the node could not be removed, for it does not exists
    = NotFound
      -- Orphaned means the subtree became one level less deep. This needs taking care of recursively, in order to restore the "all leaves at the same level" invariant
    | Merged (Tree comparable)
      -- The simplest of cases - a subtree was replaced by something with an equivalent amount of levels, which means the break of invariants has been contained, and the replacement can simply bubble up
    | Replaced (Tree comparable)


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
                Debug.log logString result

        findNextLarger : comparable -> Tree comparable -> comparable
        findNextLarger item tree =
            let
                tag : comparable -> comparable
                tag =
                    Debug.log "Removing an internal node is hard, so substituting for the next larger value."
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

        doRemove : comparable -> Tree comparable -> DeletionResult comparable
        doRemove item tree =
            case tree of
                -- If its an empty tree, we can't possibly contain the value
                -- you're looking for
                Empty ->
                    tag NotFound

                -- Leafnode with 1 key: either it is us and we're "orphaning" an empty subtree (which means we're creating a hole in the tree that must somehow be filled), or it does not exists
                TwoNode Empty self Empty ->
                    if self == item then
                        tag <| Merged empty
                    else
                        tag NotFound

                -- Leafnode with 2 keys: Either punching a hole that can be filled by simply replacing with a singleton, or not found
                ThreeNode Empty left Empty right Empty ->
                    if left == item then
                        singleton right
                            |> Replaced
                            |> tag
                    else if right == item then
                        singleton left
                            |> Replaced
                            |> tag
                    else
                        tag NotFound

                -- internal node with 1 key: If our key is the key to be removed, look instead for the next larget node in the right-hand side of the tree, and "act" as if our node is located there
                TwoNode lower self greater ->
                    if item < self then
                        case doRemove item lower of
                            NotFound ->
                                tag NotFound

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
                                            |> tag

                                    TwoNode sLower sSelf sGreater ->
                                        ThreeNode
                                            mergeTree
                                            self
                                            sLower
                                            sSelf
                                            sGreater
                                            |> Merged
                                            |> tag

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode replacement self greater
                                    |> Replaced
                                    |> tag
                    else if item == self then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item greater
                        in
                            -- We're an internal node, so we need to cheat a bit and remove the next larger (or lower, but we chose larger.) leafnode, taking care to re-insert that node and forget all about our self.
                            case doRemove nextLarger greater of
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
                                                |> tag

                                        ThreeNode sLower sLeft sBetween sRight sGreater ->
                                            TwoNode
                                                (TwoNode sLower sLeft sBetween)
                                                sRight
                                                (TwoNode sGreater nextLarger mergeTree)
                                                |> Replaced
                                                |> tag

                                Replaced replacement ->
                                    TwoNode
                                        lower
                                        nextLarger
                                        replacement
                                        |> Replaced
                                        |> tag
                    else
                        case doRemove item greater of
                            NotFound ->
                                tag NotFound

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
                                            |> tag

                                    ThreeNode sLower sLeft sBetween sRight sGreater ->
                                        TwoNode
                                            (TwoNode sLower sLeft sBetween)
                                            sRight
                                            (TwoNode sGreater self mergeTree)
                                            |> Replaced
                                            |> tag

                            -- Finally, a SIMPLE CASE YAY.
                            Replaced replacement ->
                                TwoNode lower self replacement
                                    |> Replaced
                                    |> tag

                ThreeNode lower left between right greater ->
                    if item < left then
                        case doRemove item lower of
                            NotFound ->
                                tag NotFound

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
                                                    |> tag

                                            ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                ThreeNode
                                                    (TwoNode mergeTree left bLower)
                                                    bSelf
                                                    (TwoNode bGreater right gLower)
                                                    gLeft
                                                    (TwoNode gBetween gRight gGreater)
                                                    |> Replaced
                                                    |> tag

                                    ThreeNode bLower bLeft bBetween bRight bGreater ->
                                        ThreeNode
                                            (TwoNode mergeTree left bLower)
                                            bLeft
                                            (TwoNode bBetween bRight bGreater)
                                            right
                                            greater
                                            |> Replaced
                                            |> tag

                            Replaced replacement ->
                                ThreeNode
                                    replacement
                                    left
                                    between
                                    right
                                    greater
                                    |> Replaced
                                    |> tag
                    else if item == left then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item between
                        in
                            case doRemove nextLarger between of
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
                                                        |> tag

                                                ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                    ThreeNode
                                                        lower
                                                        nextLarger
                                                        (TwoNode mergeTree right gLower)
                                                        gLeft
                                                        (TwoNode gBetween gRight gGreater)
                                                        |> Replaced
                                                        |> tag

                                        ThreeNode lLower lLeft lBetween lRight lGreater ->
                                            ThreeNode
                                                (TwoNode lLower lLeft lBetween)
                                                lRight
                                                (TwoNode lGreater left mergeTree)
                                                right
                                                greater
                                                |> Replaced
                                                |> tag

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        nextLarger
                                        replacement
                                        right
                                        greater
                                        |> Replaced
                                        |> tag
                    else if item < right then
                        case doRemove item between of
                            NotFound ->
                                tag NotFound

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
                                                    |> tag

                                            ThreeNode gLower gLeft gBetween gRight gGreater ->
                                                ThreeNode
                                                    lower
                                                    left
                                                    (TwoNode mergeTree right gLower)
                                                    gLeft
                                                    (TwoNode gBetween gRight gGreater)
                                                    |> Replaced
                                                    |> tag

                                    ThreeNode lLower lLeft lBetween lRight lGreater ->
                                        ThreeNode
                                            (TwoNode lLower lLeft lBetween)
                                            lRight
                                            (TwoNode lGreater left mergeTree)
                                            right
                                            greater
                                            |> Replaced
                                            |> tag

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    replacement
                                    right
                                    greater
                                    |> Replaced
                                    |> tag
                    else if item == right then
                        let
                            nextLarger : comparable
                            nextLarger =
                                findNextLarger item greater
                        in
                            case doRemove nextLarger greater of
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
                                                        |> tag

                                                ThreeNode lLower lLeft lBetween lRight lGreater ->
                                                    ThreeNode
                                                        (TwoNode lLower lLeft lBetween)
                                                        lRight
                                                        (TwoNode lGreater left bLower)
                                                        bSelf
                                                        (TwoNode bGreater nextLarger mergeTree)
                                                        |> Replaced
                                                        |> tag

                                        ThreeNode bLower bLeft bBetween bRight bGreater ->
                                            ThreeNode
                                                lower
                                                left
                                                (TwoNode bLower bLeft bBetween)
                                                bRight
                                                (TwoNode bGreater nextLarger mergeTree)
                                                |> Replaced
                                                |> tag

                                Replaced replacement ->
                                    ThreeNode
                                        lower
                                        left
                                        between
                                        nextLarger
                                        replacement
                                        |> Replaced
                                        |> tag
                    else
                        case doRemove item greater of
                            NotFound ->
                                tag NotFound

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
                                                    |> tag

                                            ThreeNode lLower lLeft lBetween lRight lGreater ->
                                                ThreeNode
                                                    (TwoNode lLower lLeft lBetween)
                                                    lRight
                                                    (TwoNode lGreater left bLower)
                                                    bSelf
                                                    (TwoNode bGreater right mergeTree)
                                                    |> Replaced
                                                    |> tag

                                    ThreeNode bLower bLeft bBetween bRight bGreater ->
                                        ThreeNode
                                            lower
                                            left
                                            (TwoNode bLower bLeft bBetween)
                                            bRight
                                            (TwoNode bGreater right mergeTree)
                                            |> Replaced
                                            |> tag

                            Replaced replacement ->
                                ThreeNode
                                    lower
                                    left
                                    between
                                    right
                                    replacement
                                    |> Replaced
                                    |> tag
    in
        case doRemove (Debug.log "Attempting to remove " item) tree |> tag of
            NotFound ->
                tree

            Replaced newTree ->
                newTree

            Merged newTree ->
                newTree


type InsertionResult comparable
    = Consumed (Tree comparable)
    | Pushed (Tree comparable) comparable (Tree comparable)
    | AlreadyExists


fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert empty


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
