# linden-scheme
Parametric 2L-systems integrated with Scheme. 

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install linden-scheme`.

## Requirements
None

## Documentation
### Defining L-systems
    [macro] (define-rule [CLASS] (RULE-NAME [ARGS] ...) BODY ...)


    [macro] (define-render-rule [CLASS] (RULE-NAME [ARGS] ...) BODY ...)


    [macro] (define-production CLASS (SUPER-CLASSES ...) (RULE [ARGS] ...))


### Stepping and rendering productions
    [procedure] (step-production PRODUCTION)


    [procedure] (step-production-times N PRODUCTION)


    [procedure] (render-production PRODUCTION)


### Macros
    [macro] (context (TEST BODY ...) ...)


    [macro] (probability (PROBABILITY BODY ...) ...)


### Manipulating state
While rendering L-systems, it is often desirable to track the state of a number of variables, following the branches in the production. Through this, one can implement any sort of turtle graphics system. linden-schemes provides this mechanism through the following three functions:

    [procedure] (define-state VAR DEFAULT)

Creates a new state variable named `VAR` with the default value `DEFAULT`.

    [procedure] (get-state VAR)

Returns the value of the state variable `VAR`. This is only useful when called within a rule.

    [procedure] (set-state VAR VALUE)

Sets the value of the state variable `VAR` to `VALUE`. This is only useful when called within a rule.


## Examples
The following is an example of a production of a crocus taken from The Algorithmic Beauty of Plants [(Prusinkiewicz, Lindermayer. 1990)](http://algorithmicbotany.org/papers/abop/abop.pdf), section 3.1.3.

``` scheme
(use linden-scheme)

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

(define-production crocus (plant)
  (apex 1))

(print (step-production (crocus)))
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
