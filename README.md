# Self-balancing binary search trees, in Elm.

Self-balancing binary search trees are a node-based data structure that allow amortized worst case asymptotic `O (log n)` lookup, insertion and removal, as well as `O (n)` enumeration.

## AVL trees

## Two Three trees

## Fold-based operations

### toList

Convert tree to list in ascending order, using foldl.

*Example implementation:*

```elm
toList : Tree comparable -> List comparable
toList =
    foldl (::) []
```

### fromList

Create tree from list by folding over the list and inserting into an initially empty tree. Folds over the list, rather than the tree.

*Example implementation:*

```elm
fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert empty
```

### size

Foldl over the list and incrementing an accumulator by one for each value that passes through the accumulator operation.

*Example implementation:*

```elm
size : Tree comparable -> Int
size =
    foldl (\_ acc -> acc + 1) 0
```

### map

Fold over the tree, executing the specified operation on each value, and
accumulating these values into a new tree.

*Example implementation:*

```elm
map : (comparable -> comparable2) -> Tree comparable -> Tree comparable2
map operator =
    foldl
        (insert << operator)
        empty
```

### filter

Create a new set with elements that match the predicate.

*Example implementation:*

```elm
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
```

### union

Union is implemented by folding over the second tree and inserting it into the
first tree.

*Example implementation:*

```elm
union : Tree comparable -> Tree comparable -> Tree comparable
union =
    foldl insert
```

### intersect

Tree intersection creates a new Tree containing only those values found in both
trees. This is implemented by filtering the right-hand set, only keeping values
found in the left-hand set.

*Example implementation:*

```elm
intersect : Tree comparable -> Tree comparable -> Tree comparable
intersect left =
    filter (flip member left)
```

### difference

The differences between two trees is, in Elm land, defined as the elements of
the left tree that do not exists in the right tree. As such, this is
implemented by filtering the left tree for values that do not exist in the
right set.

```elm
diff : Tree comparable -> Tree comparable -> Tree comparable
diff left right =
    filter (not << flip member right) left
```

### partition

Similar to filtering, this does not throw away the values that do not match the
predicate, but creating a second tree from those values. The resulting trees
are then returned as a tuple.

*Example implementation:*

```elm
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
```
