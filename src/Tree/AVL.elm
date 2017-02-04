module Tree.AVL exposing (..)

{-| Let's make a set as a self-balancing AVL tree. For reasons.

To make things a tiny bit easier, here some funky syntax for representing trees:

```
-- This is a tree with a single member, and a an empty left-
--  and righthand side
< 1 . . >

-- An empty tree, hence, is visualised as follows:
.

-- A more complicated tree with 3 members, would look like
-- this:
< 2
  < 1 . . >
  < 3 . . >
>

```

# Types
@docs Node

# Creation
@docs empty, singleton

# Inspection
@docs member, size

# Manipulation
@docs insert, map, union, remove, filter, intersect, diff, partition

# Transformation
@docs toList, fromList, foldl, foldr

# Internals
@docs tree, rotateLeft, rotateRight, height, heightDiff, balance
-}


{-| A Tree must contain comparable types (because we use a balanced AVL tree
internally. It can, of course, also be `Empty`.
-}
type Tree comparable
    = Node Int comparable (Tree comparable) (Tree comparable)
    | Empty


{-| "Pure" application of our Tree: a singleton.

```
import Tree.Self as Tree exposing (Tree)


setWithOneElement : Tree Int
setWithOneElement
    = Tree.singleton 1


setWithOneElement == < 1 . . >
```
-}
singleton : comparable -> Tree comparable
singleton item =
    Node 1 item empty empty


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


{-| Create an empty Tree. Useful to hide implementation details and whatnot

```
import Tree.Self as Tree exposing (Tree)

Tree.empty == .
```
-}
empty : Tree comparable
empty =
    Empty


{-| Insert an element into the appropriate location in our Tree. If the element
already exists, this is of no use and no elements are added.

After an element is inserted, the tree is rebalanced, so the AVL invariant
holds.

```
import Tree.Self as Tree exposing (Tree)

Tree.singleton 1
    |> Tree.insert 2
    == < 1
         .
         < 2 . . >
       >
```
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


{-| A simple call to fold: `foldl (::) []`.

Yay!

```
import Tree.Self as Tree exposing (Tree)


Tree.empty
    |> Tree.toList == []


Tree.singleton 1
    |> Tree.toList == [1]
```
-}
toList : Tree comparable -> List comparable
toList =
    foldl (::) []


{-| Create a Tree *from* a List. This should actually be the final incantation.

```
import Tree.Self as Tree exposing (Tree)


aList : List Int
aList =
    [ 1, 2, 3 ]


aList |> Tree.fromList ==
    < 2
      < 1 . . >
      < 3 . . >
    >
```
-}
fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert empty


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


{-| Checking if an item is present in a tree - an important operation - can
happen in `O (log n)`. That's what using an AVL tree gets you. Essentially, this
uses the same inner working as the insert operation: recurse down the tree and
check if the element is there, somewhere.
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


{-| Figure out how many items are in this set.

This is implemented in terms of a fold. Because that's just neat!
-}
size : Tree comparable -> Int
size =
    foldl (\_ acc -> acc + 1) 0


{-| Fold-left over our set.

This works by folding over the left hand, then the head, then the right; all
recursively (except the head).

Coolbeans!
-}
foldl : (comparable -> a -> a) -> a -> Tree comparable -> a
foldl operation acc set =
    case set of
        Empty ->
            acc

        Node _ head left right ->
            foldl operation acc left
                |> (\acc -> operation head acc)
                |> (\acc -> foldl operation acc right)


{-| Fold-right over our set.

Similar to foldl, but in reversed order of operation - fold over the right hand,
*then* the head, and *finally* the left hand.
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


{-| Folds on neat data structures are so... neat.

This doccomment is already longer than the code.

Neat. NEAT. **NEAT**.
-}
union : Tree comparable -> Tree comparable -> Tree comparable
union =
    foldl insert


{-| Remove `item` from the set, if it is in there.
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


{-| TODO
-}
intersect : Tree comparable -> Tree comparable -> Tree comparable
intersect left =
    filter (flip member left)


{-| TODO
-}
diff : Tree comparable -> Tree comparable -> Tree comparable
diff left right =
    filter (not << flip member right) left


{-| TODO
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


{-| Anothing fold. I love folds. Yay, folds!
-}
map : (comparable -> comparable2) -> Tree comparable -> Tree comparable2
map operator =
    foldl
        (insert << operator)
        empty
