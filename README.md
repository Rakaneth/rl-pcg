# What this is

This is a random number generator based on [this](https://github.com/imneme/pcg-c-basic) implementation of the PCG family of random number generators. Its API is designed to be used for roguelike games.

# How to use

Clone this project into a place where it can be loaded by [Quicklisp](http://quicklisp.org); it can then be loaded standalone or pulled in as a dependency for any other Quicklisp project.

The package provides the nickname `pcg` that can be used with exported functions.

# API

## RNG functions

### `(new-rng &key seed)`

Creates a new RNG with an integer seed of `seed`. This can be passed to any function or method that requires an RNG.

### `*global-rng*`

A global RNG instance created at runtime, seeded with the system time. Any function that requires an RNG will use this if the `:rng` parameter is omitted.

### `(get-int &key (min-num 0) max-num rng)`

Returns a uniformly random integer in the interval `[min-num, max-num)`.

### `(get-bool &key rng)`

Returns `T` or `NIL` at random.

### `(get-float &key rng)`

Returns a uniformly random float in the interval `[0, 1)`.

### `(get-weighted table &key rng)`

Returns a random item from `table`, which should be an alist with the items to be returned as keys and weights as values.

Example: 
```lisp
(defvar *table* '((:orc . 5)
                  (:troll . 3)
                  (:dragon . 1))) ;; => *TABLE*

(get-weighted *table*) ;; => one of :orc, :troll, or :dragon
```

### `(get-random-element s &key rng)`

Returns a random element from `s`.

## Dice functions and methods

### Rolling methods

#### `(roll ((sides integer) &key (dice 1) (bonus 0) (diff 0) (target 0) keep rng))`

The heart of the roll methods, this simulates a roll of `dice` `sides`-sided dice. 

`bonus` is added to the total of the roll.
`diff` checks each die to see if it rolled at or above `diff`.
`target` checks the total to see if it rolled at or above `target`.
`keep` keeps the highest `keep` dice. It will keep all dice by default or if this value is greater than `dice`.

This function summarizes the results of the roll with a plist in the form:
`(:total (sum of kept dice) :roll (list of dice rolled) :success (T if total >= target) :hits (number of kept dice >= diff)`

#### `(roll ((dp dice-parser) &key))`

Rolls a dice-parser object (see [below](#dice-string-parsing)).

#### `(roll ((dice-string string) &key rng)`

Parses `dice-string` and rolls the resulting dice parser object using `rng`.

### Dice String Parsing

#### `(parse dice-string &key rng)`

Parses a valid `dice-string`, returning a dice-parser object, which can be `roll`ed (see [above](#rolling-methods))

#### Dice Notation

* Basic Notation
    * `XdY(+|-Z)` rolls `X` dice of `Y` sides with a modifier of `Z`
    * Examples:
        * `4d6+3` rolls 4 6-sided dice and adds 3 to the total
        * `2d4-1` rolls 2 4-sided dice and subtracts 1 from the total

* Target Numbers
    * `XdY(+|-Z)(tT)` rolls `XdY(+|-Z)` as above, testing against a target number of `T` (reported in the `:success` key of the roll)
    * Examples:
        * `1d20t15` rolls a d20 against a target number of 15.
        * `2d10-4t12` rolls 2d10, subtracts 4, then compares against the target number of 12.

* Hits and Difficulty
    * `XdY(fD)` rolls `XdY` as above, comparing each roll against the difficulty of `D`, reporting the number of successes in `:hits`
    * Not compatible with the keep options
    * Examples:
        * `7d10f6` rolls 7 10-sided dice, counting the number of dice that are 6 or above
        * `6d6f4` rolls 6 6-sided dice, counting the number of dice that are 4 or above

* Keep Highest
    * `XdY(kK)(+|-Z)` rolls `XdY` as above, keeping `K` highest dice, modifying the result by `Z`.
    * May be used with target numbers, bonuses, and penalties freely.
    * Examples:
        * `4d6k3` rolls 4 6-sided dice, keeping the highest 3.
        * `4d6k3t11` rolls 4 6-sided dice, keeping the highest 3, comparing the result to a target of 11.
        * `4d6k3+10` rolls 4 6-sided dice, keeping the highest 3, adding 10 to the result.
        * `4d6k3+10t11` rolls 4 6-sided dice, keeping the highest 3, adding 10 to the result, comparing the result to a target of 11.
            * `4d6+10k3t11` is a valid variation of the previous roll.
            * `4d6t11k3+10` is also a valid variation.

## UUID

### `(get-uuid (&key rng))`

This function returns a v4 UUID as a string value.

## Shuffling

### `(shuffle! sequence (&key rng))

This function shuffles `sequence` in place. Modifies and returns the sequence.

## Changelog

### v0.2.4

* Added `shuffle!` function.

### v0.2.3

* Added `get-uuid` function.

### v0.2.2

* General release.


