# Iterators for ELM 

`elm-iter` implements lazy iterators in ELM. More later :)


# Examples

We can convert iterators from/to lists:

    it = Iter.fromList [1, 2, 3]

    lst = Iter.fromIter it

But iterators can also be infinite:

    Iter.numbers  --- generate all natural numbers starting at 0

Of course, this is only useful because we have functions to transform, slice,
and join different iterators

    String.concat << Iter.toList
        (Iter.take 11 <| 
            (Iter.intersperse ", " (Iter.map String.fromInt numbers))
        )
    === "0, 1, 2, 3, 4, 5" 
