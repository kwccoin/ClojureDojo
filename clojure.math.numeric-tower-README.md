clojure.math.numeric-tower
========================================

Formerly clojure.contrib.math

Math functions that deal intelligently with the various
types in Clojure's numeric tower, as well as math functions
commonly found in Scheme implementations.


Releases and Dependency Information
========================================

Latest stable release: 0.0.1

* [All Released Versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22math.numeric-tower%22)

* [Development Snapshot Versions](https://oss.sonatype.org/index.html#nexus-search;gav~org.clojure~math.numeric-tower~~~)

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[org.clojure/math.numeric-tower "0.0.1"]
```

[Maven](http://maven.apache.org/) dependency information:

```xml
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>math.numeric-tower</artifactId>
  <version>0.0.1</version>
</dependency>
```

Example Usage
========================================

```clojure
(ns example.core
  (:require [clojure.math.numeric-tower :as math]))

(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (math/expt x 2))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (math/sqrt (euclidean-squared-distance a b)))

(let [a [1 2 3 5 8 13 21]
      b [0 2 4 6 8 10 12]
  (euclidean-distance a b))

;;=> 9.643650760992955
```

Refer to docstrings in the `clojure.math.numeric-tower` namespace for
additional documentation.

[API Documentation](http://clojure.github.com/math.numeric-tower/)

Developer Information
========================================

* [GitHub project](https://github.com/clojure/math.numeric-tower)

* [Bug Tracker](http://dev.clojure.org/jira/browse/MTOWER)

* [Continuous Integration](http://build.clojure.org/job/math.numeric-tower/)

* [Compatibility Test Matrix](http://build.clojure.org/job/math.numeric-tower-test-matrix/)


Changelog
========================================

* Release 0.0.1 on 2011-10-15
  * Initial release.
  * Source-compatible with clojure.contrib.math, except for the name change.

License
========================================

Distributed under the Eclipse Public License, the same as Clojure.


========================================

API for clojure.math.numeric-tower - Math functions 0.0.2 (in development)
by Mark Engelberg

Full namespace name: clojure.math.numeric-tower

Overview
Project home page is http://github.com/clojure/math.numeric-tower/

Math functions that deal intelligently with the various
types in Clojure's numeric tower, as well as math functions
commonly found in Scheme implementations.

expt - (expt x y) is x to the yth power, returns an exact number
  if the base is an exact number, and the power is an integer,
  otherwise returns a double.
abs - (abs n) is the absolute value of n
gcd - (gcd m n) returns the greatest common divisor of m and n
lcm - (lcm m n) returns the least common multiple of m and n

When floor, ceil, and round are passed doubles, we just defer to
the corresponding functions in Java's Math library.  Java's
behavior is somewhat strange (floor and ceil return doubles rather
than integers, and round on large doubles yields spurious results)
but it seems best to match Java's semantics.  On exact numbers
(ratios and decimals), we can have cleaner semantics.

floor - (floor n) returns the greatest integer less than or equal to n.
  If n is an exact number, floor returns an integer,
  otherwise a double.
ceil - (ceil n) returns the least integer greater than or equal to n.
  If n is an exact number, ceil returns an integer,
  otherwise a double.
round - (round n) rounds to the nearest integer.
  round always returns an integer.  round rounds up for values
  exactly in between two integers.


sqrt - Implements the sqrt behavior I'm accustomed to from PLT Scheme,
  specifically, if the input is an exact number, and is a square
  of an exact number, the output will be exact.  The downside
  is that for the common case (inexact square root), some extra
  computation is done to look for an exact square root first.
  So if you need blazingly fast square root performance, and you
  know you're just going to need a double result, you're better
  off calling java's Math/sqrt, or alternatively, you could just
  convert your input to a double before calling this sqrt function.
  If Clojure ever gets complex numbers, then this function will
  need to be updated (so negative inputs yield complex outputs).
exact-integer-sqrt - Implements a math function from the R6RS Scheme
  standard.  (exact-integer-sqrt k) where k is a non-negative integer,
  returns [s r] where k = s^2+r and k < (s+1)^2.  In other words, it
  returns the floor of the square root and the "remainder".


Protocols

MathFunctions
Protocol

Known implementations: clojure.lang.BigInt, clojure.lang.Ratio, java.lang.Double, java.lang.Float, java.lang.Integer, java.lang.Long, java.math.BigDecimal, java.math.BigInteger

ceil
function

Usage: (ceil n)

(ceil n) returns the least integer greater than or equal to n.
If n is an exact number, ceil returns an integer, otherwise a double.


floor
function

Usage: (floor n)

(floor n) returns the greatest integer less than or equal to n.
If n is an exact number, floor returns an integer, otherwise a double.


integer-length
function

Usage: (integer-length n)

Length of integer in binary


round
function

Usage: (round n)

(round n) rounds to the nearest integer.
round always returns an integer.  Rounds up for values exactly in between two integers.


sqrt
function

Usage: (sqrt n)

Square root, but returns exact number if possible.

Source

Public Variables and Functions

abs
function

Usage: (abs n)

(abs n) is the absolute value of n

Source

exact-integer-sqrt
function

Usage: (exact-integer-sqrt n)

(exact-integer-sqrt n) expects a non-negative integer n, and returns [s r] where n = s^2+r and n < (s+1)^2.  In other words, it returns the floor of the square root and the 'remainder'.
For example, (exact-integer-sqrt 15) is [3 6] because 15 = 3^2+6.

Source

expt
function

Usage: (expt base pow)

(expt base pow) is base to the pow power.
Returns an exact number if the base is an exact number and the power is an integer, otherwise returns a double.

Source

gcd
function

Usage: (gcd a b)

(gcd a b) returns the greatest common divisor of a and b

Source

lcm
function

Usage: (lcm a b)

(lcm a b) returns the least common multiple of a and b

Source
Copyright 2007-2012 by Rich Hickey and the various contributors

