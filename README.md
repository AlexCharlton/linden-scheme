# linden-scheme
Parametric 2L-systems integrated with Scheme. The L-systems defined with this library take a significantly different form from the string-based L-systems described by Lindenmayer. linden-scheme uses a class-based system for defining L-systems.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install linden-scheme`.

## Requirements
None

## Documentation
### Defining L-systems
L-systems in linden-scheme are comprised of two parts: L-systems and rules, which are conceptually analogous to classes and methods, respectively.

    [macro] (define-rule [CLASS] (RULE-NAME [ARGS] ...) BODY ...)

Defines a rule similar to defining a function, but with an optional `CLASS`. If `CLASS` is omitted, the rule will be used if no more specific rules are defined – i.e. as a fallback rule. Multiple rules with the same `RULE-NAME` may be defined, provided they have different classes (otherwise the previous rule with the same name will be over-written).

Rules should return a list of rules in `(RULE [ARGS] ...)` form. Any non-list value, when returned, will be treated as a no-op. In other words, when the rule is evaluated and a list is returned, the rule being evaluated in the current L-systems (by `step-l-system`) will be replaced by the given list of rules. If no list is provided, the rule being evaluated will remain the same.

The special form `branch` may be included in the list of rules returned by a rule. `branch` should contain one or more rules. When `branch` is used, the state of the rules contained in the `branch` is split off from that of the parent of the `branch`.

For example, a list returned by a rule may look like:

    '((leaf 1)
      (branch (leaf 1) (stem 1) (apex))
      (stem 1) 
      (apex))

which describes a leaf, a branch (containing a leaf, stem, and apex), a stem, and an apex.

When context-dependant or probabilistic rules are desired, see the macros `context` and `probability` in the section [Macros](#macros)

    [macro] (define-render-rule [CLASS] (RULE-NAME [ARGS] ...) BODY ...)

`define-render-rule` behaves much like `define-rule`, to be used for the rendering of an L-system. Render rules operate by side-effect only. The state variable `render-target` is provided to track the object that the L-system is rendering to. See `render-l-system` and [Manipulating state](#manipulating-state).

    [macro] (define-l-system CLASS (SUPER-CLASSES ...) (RULE [ARGS] ...))

Defines the class of L-systems named `CLASS`. These L-systems will use rules defined for `CLASS`, and if none are available, they will inherit those from the classes `SUPER-CLASSES`. The rules defined for the super-classes are chosen in order of appearance in the super-classes list.

Additionally a function named `CLASS` is defined that, when called, returns an L-system of `CLASS` with the initial rule `RULE`.

### Stepping and rendering L-systems
    [procedure] (step-l-system SYSTEM)

Returns a new L-system, created by evaluating each rule in `SYSTEM` according to the rules defined by `define-rule` corresponding to the class of the L-system. Any rule that has not been defined by `define-rule` for the class or super-classes of the system is ignored. When rules are being evaluated, any state is branched according to `branch` statements in the system (see [Manipulating state](#manipulating-state)).

    [procedure] (step-l-system-times N SYSTEM)

Performs `step-l-system` on the given `SYSTEM`, `N` times.

    [procedure] (render-l-system SYSTEM RENDER-TARGET)

Evaluates each rule in the `SYSTEM`, in order, given their meanings defined by `define-render-rule`. Any rule that has not been defined by `define-render-rule` for the class or super-classes of the system is ignored. As with `step-l-system`, state is branched according to the `branch` statements in the system. The state variable `render-target` is set to `RENDER-TARGET` at the start of evaluation (see [Manipulating state](#manipulating-state)). `RENDER-TARGET` is returned.

### Macros
    [macro] (context (TEST BODY ...) ...)

Similar to a `cond` form, but conditional to the context of a given rule. Evaluates `BODY` when the context given in `TEST` matches the current context of the rule being evaluated. Each `TEST` should be of the form:

     ((PREVIOUS-RULE [ARGS] ...) (NEXT-RULE [ARGS] ...) [: GUARD])

When `PREVIOUS-RULE` and `NEXT-RULE` match the names of the previous and next rules to the rule being currently evaluated, the associated `BODY` is evaluated at the exclusion of all other `BODY`s. The supplied `ARGS` are symbols that are bound to the values of the arguments of their respective rules. Either `(PREVIOUS-RULE [ARGS] ...)` or `(NEXT-RULE [ARGS] ...)` may be replaced with a `*`, indicating that any rule (including none) may match.

If desired, a `GUARD` form may be supplied, preceded by a `:`. This guard acts as an additional test before a `BODY` is evaluated. The guard may use any variables given as `ARGS` (as well as any other variables in scope).

The last `TEST` may consist of the symbol `else`, which is unconditionally evaluated.

For example:

    (define-rule (apex)
      (context
       (((stem len) * : (> len 2))
        '((leaf 1) (branch (leaf 1) (stem 1) (apex)) (stem 1) (apex)))
       (((stem len) *)
        #f)
       (else '((leaf 1) (stem 1) (apex)))))

defines a rule (`apex`) that creates a new branch when preceded by a `stem` whose first parameter (`len`) greater than 2, does nothing when preceded by a `stem` of length less than or equal to 2, and otherwise creates a new `leaf` and `stem`.

    [macro] (probability (PROBABILITY BODY ...) ...)

Similar to a `cond` form, but evaluates a clause based on a given probability. `PROBABILITY` is expected to be a number less than 1.0, or the symbol `else`. The sum of all probabilities should add up to 1.0, or less than 1.0 if an `else` clause is used. Otherwise there is a chance that no clauses will be evaluated (in the case that the probabilities add to less than 1.0 and no `else` is used) or a clause may never be evaluated (in the case that the probabilities add to more than 1.0).

For example:

    (probability
      (0.1 '((branch (flower)) (apex)))
      (0.7 #f)
      (else '((branch (flower)) (branch (flower)) (apex))) 

describes a 10% chance of creating a branch with a flower, a 70% chance of doing nothing, and a 20% chance of creating two branches with flowers.

### Manipulating state
While rendering L-systems, it is often desirable to track the state of a number of variables, following the branches in the L-system. In this manner, one can implement a turtle graphics system. linden-schemes provides this mechanism through the following three functions:

    [procedure] (define-state VAR DEFAULT)

Creates a new state variable named `VAR` with the default value `DEFAULT`.

    [procedure] (get-state VAR)

Returns the value of the state variable `VAR`. This is only useful when called within a rule.

    [procedure] (set-state VAR VALUE)

Sets the value of the state variable `VAR` to `VALUE`. This is only useful when called within a rule.


## Examples
The following is an example of a L-system of a crocus taken from The Algorithmic Beauty of Plants [(Prusinkiewicz, Lindermayer. 1990)](http://algorithmicbotany.org/papers/abop/abop.pdf), section 3.1.3.

Note that three different classes (`crocus`, `plant`, and none) are used in this example. If, for example, a new rule `stem` were defined for the class `crocus`, it would be used over the `stem` rule that is defined for `plant`.

``` scheme
(use linden-scheme srfi-1)

(define developmental-switch-time 3)
(define leaf-growth-limit 4)
(define flower-growth-limit 2)

(define-rule crocus (apex time)
  (cond
     ((< time developmental-switch-time)
      `((stem 1)
        (branch (pitch -30) (leaf 0))
        (roll 138)
        (apex ,(add1 time))))
     (else '((stem 20) (flower 0)))))

(define-rule crocus (flower size)
    (cond
     ((< size flower-growth-limit)
      `((flower ,(add1 size))))))

(define-rule plant (stem length)
  (cond
   ((< length 4) `((stem ,(add1 length))))))

(define-rule (leaf size)
    (cond
     ((< size leaf-growth-limit)
      `((leaf ,(add1 size))))))

(define-l-system crocus (plant)
  (apex 1))

(for-each (lambda (i)
            (print (step-l-system-times i (crocus))))
          (iota 5 1))
```

## Version history
### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/linden-scheme).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD
