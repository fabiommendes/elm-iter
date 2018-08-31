module Tools.Iter exposing
    ( Iter(..), Next, Count
    , next, step, iterator
    , numbers, numbersFrom, numbersBy, countTo, range, linspace, stepsFrom
    , repeat, repeatN, cycle, power, powerN, empty, singleton, indexed
    , filter, take, drop, takeWithDefault, takeWhile
    , map, map2
    , zip, enumerate
    , concat, intersperse
    , toList, fromList
    , reduce, accumulate, sum, all, any, maximum, minimum
    , nth, last, lastWithDefault
    )

{-| Iterator interface inspired on Python's iterator protocol.


# Important types

@docs Iter, Next, Count


# Iterator protocol

@docs next, step, iterator


# Creating new iterators


## Numeric sequences

@docs numbers, numbersFrom, numbersBy, countTo, range, linspace, stepsFrom


## Other iterators

@docs repeat, repeatN, cycle, power, powerN, empty, singleton, indexed


# Transformations


## Sub-iterators

@docs filter, take, drop, takeWithDefault, takeWhile


## Mapping

@docs map, map2


## Zipping

@docs zip, enumerate


## Building iterators from sub-iterators

@docs concat, intersperse


## Conversion to/from lists and other data structures

@docs toList, fromList


# Reductions and folds

@docs reduce, accumulate, sum, all, any, maximum, minimum


# Retrieving iterators

@docs nth, last, lastWithDefault

-}

import Debug



--------------------------------------------------------------------------------
-- ITERATOR TYPES
--------------------------------------------------------------------------------


{-| Base iterator type
-}
type Iter a b
    = It
        { state : b
        , next : Next a b
        }


{-| A function that maybe compute the next element of an iterator
-}
type alias Next a b =
    b -> Maybe ( b, a )


{-| Toggle between two states
-}
type Toggle a b
    = First a
    | Second b


{-| A state with a auxiliary numeric counter
-}
type alias Count a =
    ( Int, a )



--------------------------------------------------------------------------------
-- ITERATOR PROTOCOL
--------------------------------------------------------------------------------


{-| Return Just the next value if iterator is not exausted or Nothing otherwise:

    next numbers === Just 0

    next empty === Nothing

-}
next : Iter a b -> Maybe a
next (It iter) =
    case iter.next iter.state of
        Just ( state_, value ) ->
            Just value

        Nothing ->
            Nothing


{-| Advance iterator by one and return a tuple of ( iterator, value ):

    (it, _) = step numbers
    next it === Just 1

-}
step : Iter a b -> ( Iter a b, Maybe a )
step (It it) =
    case it.next it.state of
        Just ( st, value ) ->
            ( It { it | state = st }, Just value )

        Nothing ->
            ( It it, Nothing )


{-| Create a new iterator from a next function and an initial state.

The next function is called successfuly to advance the iterator and produce new
values

-}
iterator : Next a b -> b -> Iter a b
iterator func init =
    makeIter init func



--------------------------------------------------------------------------------
-- CREATING NEW ITERATORS
--------------------------------------------------------------------------------


{-| Infinite iterator over the sequence of all natural numbers
-}
numbers : Iter number number
numbers =
    numbersFrom 0


{-| Infinite iterator over the sequence of all numbers starting with `a` with
increments of 1.
-}
numbersFrom : number -> Iter number number
numbersFrom a =
    makeIter a (\x -> Just ( x + 1, x ))


{-| Infinite iterator over the sequence of all numbers starting with `a`
increments of `delta`.
-}
numbersBy : number -> number -> Iter number number
numbersBy delta a =
    makeIter a (\x -> Just ( x + delta, a ))


{-| Count from 0 to n:

    countTo 5 --> 0, 1, 2, 3, 4, 5

-}
countTo : Int -> Iter Int Int
countTo n =
    stepsFrom 0 1 n


{-| Iterate from `a` to `b` (inclusive):

    range 1 3 --> 1, 2, 3

-}
range : number -> number -> Iter number number
range a b =
    stepsFrom a (sign (b - a)) (abs (b - a) + 1)


{-| Create `n` numbers from `a` to `b`:

    linspace 0 5 5 --> 0.0, 1.25, 2.5, 3.75, 5.0

-}
linspace a b n =
    stepsFrom a ((b - a) / (n - 1)) n


{-| Make `n` steps of `delta` starting from `a`

    stepsFrom 0 0.5 4 -->  0.0, 0.5, 1.0, 1.5

This function is the basis for implementing `countTo`, `range` and `linspace`.

-}
stepsFrom : number -> number -> number -> Iter number number
stepsFrom a delta n =
    let
        next_ i =
            if i < n then
                Just ( i + 1, a + delta * i )

            else
                Nothing
    in
    makeIter 0 next_



-- OTHER OPERATOR FACTORIES ----------------------------------------------------


{-| Construct iterator from index function that takes an integer and return
the value of the corresponding position.

    indexed f -->  f(0), f(1), f(2), ...

-}
indexed : (Int -> a) -> Iter a Int
indexed f =
    map f numbers


{-| Make infinite repetitions of `a`:

    repeat "foo" -->  "foo", "foo", "foo", ...

-}
repeat : a -> Iter a ()
repeat a =
    makeIter () (\x -> Just ( x, a ))


{-| Make `n` repetitions of `a`:

    repeatN 3 "foo" -->  "foo", "foo", "foo"

-}
repeatN n a =
    makeIter n
        (\m ->
            if m > 0 then
                Just ( m - 1, a )

            else
                Nothing
        )


{-| Infinite iterator that cycles among members of input list:

    cycle [ 1, 2, 3 ] -->  1, 2, 3, 1, 2, 3, 1, 2, ...

-}
cycle : List a -> Iter a (List a)
cycle lst =
    if lst == [] then
        empty (It { state = [], next = \_ -> Nothing })

    else
        let
            next_ x =
                case x of
                    [] ->
                        next_ lst

                    head :: tail ->
                        Just ( tail, head )
        in
        makeIter lst next_


{-| Successively apply function to initial argument:

    power f x -->  x, f x, f (f x), f (f (f x)), ...

-}
power : (a -> a) -> a -> Iter a a
power f a =
    makeIter a (\x -> Just ( f a, a ))


{-| Successively apply function to initial argument. Repeat `n` times.

    power 2 f x -->  x, f x, f(f x)

-}
powerN n f a =
    take n (power f a)


{-| Empty (exausted) iterator:

    next empty iter === Nothing

-}
empty : Iter a b -> Iter a b
empty it =
    makeIter (curr it) (\_ -> Nothing)


{-| Creates an iterator with a single value

    singleton a -->  a

-}
singleton : a -> Iter a (Maybe a)
singleton a =
    makeIter (Just a)
        (\s ->
            case s of
                Just x ->
                    Just ( Nothing, x )

                Nothing ->
                    Nothing
        )



--------------------------------------------------------------------------------
-- TRANSFORMATIONS
--------------------------------------------------------------------------------
-- SUB ITERATORS ---------------------------------------------------------------


{-| Return an iterator that only yields elements that agree with predicate.
-}
filter : (a -> Bool) -> Iter a b -> Iter a b
filter pred iter =
    let
        next_ : (a -> Bool) -> Next a b -> Next a b
        next_ pfunc nfunc st =
            case nfunc st of
                Just ( st_, a ) ->
                    if pfunc a then
                        Just ( st_, a )

                    else
                        next_ pred nfunc st

                Nothing ->
                    Nothing
    in
    makeIter (curr iter) (next_ pred (nextf iter))


{-| Limit the iterator to `n` elements.
-}
take : Int -> Iter a b -> Iter a (Count b)
take n (It it) =
    makeIter ( n, it.state )
        (\( m, st ) ->
            if m > 0 then
                case it.next st of
                    Just ( s, x ) ->
                        Just ( ( m - 1, s ), x )

                    Nothing ->
                        Nothing

            else
                Nothing
        )


{-| Limit the iterator to `n` elements. If the original iterator has less than
`n` elements, fill the iterator with copies of `a`.
-}
takeWithDefault : a -> Int -> Iter a b -> Iter a (Count b)
takeWithDefault a n (It it) =
    makeIter ( n, it.state )
        (\( m, st ) ->
            if m > 0 then
                case it.next st of
                    Just ( s, x ) ->
                        Just ( ( m - 1, s ), x )

                    Nothing ->
                        Just ( ( m - 1, st ), a )

            else
                Nothing
        )


{-| Take elements of iterator while predicate is True.
-}
takeWhile : (a -> Bool) -> Iter a b -> Iter a b
takeWhile pred (It it) =
    makeIter it.state
        (\s ->
            case it.next s of
                Just ( s_, x ) ->
                    if pred x then
                        Just ( s_, x )

                    else
                        Nothing

                Nothing ->
                    Nothing
        )


{-| Drop the `n` first elements of iterator.
-}
drop : Int -> Iter a b -> Iter a b
drop n (It it) =
    if n <= 0 then
        It it

    else
        case it.next it.state of
            Just ( s, _ ) ->
                drop (n - 1) (It { it | state = s })

            Nothing ->
                It it



-- MAPPING ---------------------------------------------------------------------


{-| Creates a new iterator that maps `f` into each element of the iterator.
-}
map : (a -> b) -> Iter a st -> Iter b st
map func (It iter) =
    makeIter iter.state <|
        (iter.next >> Maybe.map (Tuple.mapSecond func))


{-| Creates a new iterator that maps `f` successively into elements of iterA and
iterB.
-}
map2 : (a -> b -> c) -> Iter a st -> Iter b st_ -> Iter c ( st, st_ )
map2 func (It iterA) (It iterB) =
    makeIter ( iterA.state, iterB.state )
        (\( stA, stB ) ->
            case iterA.next stA of
                Nothing ->
                    Nothing

                Just ( stA_, xA ) ->
                    case iterB.next stB of
                        Nothing ->
                            Nothing

                        Just ( stB_, xB ) ->
                            Just ( ( stA_, stB_ ), func xA xB )
        )



-- ZIPPING ---------------------------------------------------------------------


{-| Zip two iterators (i.e., iterate over the tuples of (a, b) taken from
iterA and iterB respectively.

    zip itA itB -->  ( a0, b0 ), ( a1, b1 ), ...

-}
zip : Iter a b -> Iter c d -> Iter ( a, c ) ( b, d )
zip (It iterA) (It iterB) =
    let
        next_ ( sA, sB ) =
            case iterA.next sA of
                Nothing ->
                    Nothing

                Just ( sA_, xA ) ->
                    case iterB.next sB of
                        Nothing ->
                            Nothing

                        Just ( sB_, xB ) ->
                            Just ( ( sA_, sB_ ), ( xA, xB ) )
    in
    makeIter ( iterA.state, iterB.state ) next_


{-| Enumerate iterator counting from `a`:

    enumerate n it --> (n, x0), (n + 1, x1), (n + 2, x2), ...

-}
enumerate : Int -> Iter a b -> Iter (Count a) (Count b)
enumerate n it =
    zip (numbersFrom n) it



--------------------------------------------------------------------------------
-- BUILDING ITERATORS
--------------------------------------------------------------------------------


{-| Concatenate two iterators together:

    concat itA itB -->  a0, a1, ..., aN, b0, b1, ..., bN

-}
concat : Iter a b -> Iter a c -> Iter a (Toggle b c)
concat iterA iterB =
    makeIter (First (curr iterA)) (nextInConcatenation iterA iterB)


nextInConcatenation : Iter a b -> Iter a c -> Toggle b c -> Maybe ( Toggle b c, a )
nextInConcatenation iterA iterB state =
    case state of
        First st ->
            case nextf iterA st of
                Just ( stA, x ) ->
                    Just ( First stA, x )

                Nothing ->
                    nextInConcatenation iterA iterB (Second <| curr iterB)

        Second st ->
            nextf iterB st
                |> Maybe.map (Tuple.mapFirst Second)


{-| Interspace each element of iterator with the value of `x`:

    interspace a it --> x0, a, x1, a, x2, a, ..., xN

-}
intersperse : a -> Iter a b -> Iter a (Toggle b b)
intersperse x (It it) =
    let
        next_ : Toggle b b -> Maybe ( Toggle b b, a )
        next_ st =
            case st of
                First s ->
                    case it.next s of
                        Just ( s_, a ) ->
                            Just ( Second s_, a )

                        Nothing ->
                            Nothing

                Second s ->
                    Just ( First s, x )
    in
    makeIter (Second it.state) next_



--------------------------------------------------------------------------------
-- CONVERSION TO LISTS AND OTHER DATA STRUCTURES
--------------------------------------------------------------------------------


{-| Convert list to iterator:

    fromList [ a, b, c ] -->  a, b, c

-}
fromList : List a -> Iter a (List a)
fromList lst =
    makeIter lst (splitListHeadAndTail >> Maybe.map flip)


splitListHeadAndTail : List a -> Maybe ( a, List a )
splitListHeadAndTail lst =
    case lst of
        [] ->
            Nothing

        x :: tail ->
            Just ( x, tail )


{-| Extract list from iterator:

    toList it  ===  [ x0, x1, x2, ..., xN ]

WARNING: This function should obviously never be used on infinite iterators.

-}
toList : Iter a b -> List a
toList it =
    List.reverse <| toListAcc it []


toListAcc : Iter a b -> List a -> List a
toListAcc (It it) acc =
    case it.next it.state of
        Nothing ->
            acc

        Just ( it_, a ) ->
            toListAcc (It { it | state = it_ }) (a :: acc)



--------------------------------------------------------------------------------
-- REDUCERS
--------------------------------------------------------------------------------


{-| Reduce iterator using a left fold
-}
reduce : (a -> b -> b) -> b -> Iter a s -> b
reduce f x0 (It it) =
    let
        reducer : s -> b -> b
        reducer s x =
            case it.next s of
                Just ( t, y ) ->
                    reducer t (f y x)

                Nothing ->
                    x
    in
    reducer it.state x0


{-| Accumulative reduce: return an iterator over partial reductions of the
input iterator.
-}
accumulate : (a -> b -> b) -> b -> Iter a s -> Iter b ( s, b )
accumulate f x0 (It it) =
    makeIter ( it.state, x0 )
        (\( st, x ) ->
            case it.next st of
                Just ( st_, a ) ->
                    let
                        x_ =
                            f a x
                    in
                    Just ( ( st_, x_ ), x_ )

                Nothing ->
                    Nothing
        )


{-| Sum all elements of iterator
-}
sum : Iter number a -> number
sum =
    reduce (+) 0


{-| Return true if all elements of iterator satisfy the given predicate
-}
all : (a -> Bool) -> Iter a b -> Bool
all pred =
    map pred >> reduce (&&) True


{-| Return true if any element of iterator satisfy the given predicate
-}
any : (a -> Bool) -> Iter a b -> Bool
any pred =
    map pred >> reduce (||) False


{-| Return Just the largest element of iterator or Nothing if iterator is empty.
-}
maximum : Iter comparable b -> Maybe comparable
maximum it =
    case step it of
        ( it_, Just x ) ->
            Just (reduce max x it_)

        ( it_, Nothing ) ->
            Nothing


{-| Return Just the lowest element of iterator or Nothing if iterator is empty.
-}
minimum : Iter comparable b -> Maybe comparable
minimum it =
    case step it of
        ( it_, Just x ) ->
            Just (reduce min x it_)

        ( it_, Nothing ) ->
            Nothing



--------------------------------------------------------------------------------
-- FETCHING ELEMENTS
--------------------------------------------------------------------------------


{-| Return the n-th element of iterator.
-}
nth : Int -> Iter a b -> Maybe a
nth n it =
    next (drop n it)


{-| Return the last element of iterator.
-}
last : Iter a b -> Maybe a
last it =
    case step it of
        ( it_, Just x ) ->
            Just (lastWithDefault x it_)

        ( it_, Nothing ) ->
            Nothing


{-| Return the last element of or `x` if iterator is empty.
-}
lastWithDefault : a -> Iter a b -> a
lastWithDefault x it =
    case step it of
        ( it_, Just x_ ) ->
            lastWithDefault x_ it_

        ( it_, Nothing ) ->
            x



-- AUXILIARY FUNCTIONS


makeIter : b -> Next a b -> Iter a b
makeIter st f =
    It { state = st, next = f }


curr : Iter a b -> b
curr (It iter) =
    iter.state


nextf : Iter a b -> (b -> Maybe ( b, a ))
nextf (It iter) =
    iter.next


sign : number -> number
sign n =
    if n > 0 then
        1

    else if n < 0 then
        -1

    else
        0


double : a -> ( a, a )
double x =
    ( x, x )


flip : ( a, b ) -> ( b, a )
flip ( a, b ) =
    ( b, a )
