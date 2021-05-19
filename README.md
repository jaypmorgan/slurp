# SluRp

*Statistical Lisp in R*


## Examples

```lisp
(+ 2 (* 2 3))

# [1] 8

(defparam x 5)

# [1] 5

(+ x 5)

# [1] 10

(rnorm 10 10 10)

# [1]  14.8692128   3.4932029  18.6337963  15.5319696  -2.9426000 -11.6523209
# [7]  -0.9475137   6.1512033   4.6517305   8.0993424

(print "testing")

# "testing"
# "testing" <- returns "testing" as well

# create vectors with [ ]
(mean [1 2 3 4 5])

# [1] 3
```

## Things to do:

- [X] Lisp tokenisation
- [X] Basic function application
- [X] Vector notation
- [X] Nested function application
- [ ] Function definitions
- [ ] Lambdas
- [X] Variable assignments
- [ ] Sets and Lists
- [ ] Keyword arguments
- [ ] Macros
- [ ] Pre-installed macros such as threading

### Functions to implement

- [ ] Cons
- [ ] Listp
- [ ] Atomp
- [ ] Better evaluation - special symbols evaluate to themselves
- [ ] Better tokenisation -- should take into account the end of the list to allow scripts to run
- [ ] Quote/Eval
