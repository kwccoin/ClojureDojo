; - clojure Macro
; - R. Mark Volkmann - Clojure - Functional Programming for the JVM
; http://java.ociweb.com/mark/clojure/article.html#Macros

;; Macros

#_(
Macros are used to add new constructs to the language. They are code
that generates code at read-time.

While functions always evaluate all their arguments, macros can decide
which of their arguments will be evaluated. This is important for
implementing forms like (if condition then-expr else-expr). If the
condition is true, only the "then" expression should be evaluated. If
the condition is false, only the "else" expression should be
evaluated. This means that if cannot be implemented as a function (it
is in fact a special form, not a macro). Other forms that must be
implemented as macros for this reason include and and or because they
need to "short-circuit".

To determine whether a given operation is implemented as a function or
a macro, either enter (doc name) in a REPL or examine its metadata. If
it is a macro then the metadata will contain a :macro key with a value
of true. For example, to determine this for and, enter the following
code in a REPL:
)

((meta (var and)) :macro) ; long way -> true
((meta #'and) :macro) ; slightly shorter way -> true

#_(
Let's walk through some examples of writing and using macros. Suppose
there are many places in our code that need to take different actions
based on whether a number is really close to zero, negative or
positive. We want to avoid code duplication. This must be implemented
as a macro instead of a function because only one of the three actions
should be evaluated. The defmacro macro defines a macro.

(defmacro around-zero [number negative-expr zero-expr positive-expr]
  `(let [number# ~number] ; so number is only evaluated once
    (cond
      (< (Math/abs number#) 1e-15) ~zero-expr
      (pos? number#) ~positive-expr
      true ~negative-expr)))

The Reader expands calls to the around-zero macro into a call to the
let special form. That contains a call to the cond function whose
arguments are its conditions and return values. The let special form
is used here for efficiency in the event that the first parameter,
number, is passed as an expression instead of a simple value. It
evaluates number once and then uses its value in two places within the
cond. The auto-gensym number# is used to generate a unique symbol name
so there is no chance the binding name can conflict with that of
another symbol. This enables the creation of hygienic macros.

The back-quote (a.k.a. syntax quote) at the beginning of the macro
definition prevents everything inside from being evaluated unless it
is unquoted. This means the contents will appear literally in the
expansion, except items preceded by a tilde (in this case, number,
zero-expr, positive-expr and negative-expr). When a symbol name is
preceded by a tilde inside a syntax quoted list, its value is
substituted. Bindings in syntax quoted lists whose values are
sequences can be preceded by ~@ to substitute their individual items.

Here are two example uses of this macro where the expected output is "+".

(around-zero 0.1 (println "-") (println "0") (println "+"))
(println (around-zero 0.1 "-" "0" "+")) ; same thing

To execute more than one form for one of the cases, wrap them in a do
form. For example, if the number represented a temperature and we had
a log function to write to a log file, we might write this:

(around-zero 0.1
  (do (log "really cold!") (println "-"))
  (println "0")
  (println "+"))

To verify that this macro is expanded properly, enter the following in a REPL:

(macroexpand-1
  '(around-zero 0.1 (println "-") (println "0") (println "+")))

This will output the following on a single line without the indentation:

(clojure.core/let [number__3382__auto__ 0.1]
  (clojure.core/cond
    (clojure.core/< (Math/abs number__3382__auto__) 1.0E-15) (println "0")
    (clojure.core/pos? number__3382__auto__) (println "+")
    true (println "-")))

Here's a function that uses the macro to return a word describing a number.

(defn number-category [number]
  (around-zero number "negative" "zero" "positive"))

Here are some example uses.

(println (number-category -0.1)) ; -> negative
(println (number-category 0)) ; -> zero
(println (number-category 0.1)) ; -> positive

Since macros don't evaluate their arguments, unquoted function names
can be passed to them and calls to the functions with arguments can be
constructed. Function definitions cannot do this and instead must be
passed anonymous functions that wrap calls to functions.

Here's a macro that takes two arguments. The first is a function that
expects one argument that is a number of radians, such as a
trigonometry function. The second is a number of degrees. If this were
written as a function instead of a macro, we would have to pass
#(Math/sin %) instead of simply Math/sin for the function. Note the
use of the # suffix to generate unique, local binding names. This is
sometimes necessary to avoid collisions with other bindings. Both #
and ~ must be used inside a syntax quoted list.

(defmacro trig-y-category [fn degrees]
  `(let [radians# (Math/toRadians ~degrees)
         result# (~fn radians#)]
     (number-category result#)))

Let's try it. The expected output from the code below is "zero",
"positive", "zero" and "negative".

(doseq [angle (range 0 360 90)] ; 0, 90, 180 and 270
  (println (trig-y-category Math/sin angle)))

Macro names cannot be passed as arguments to functions. For example, a
macro name such as and cannot be passed to the reduce function. A
workaround is to define an anonymous function that calls the macro.
For example, (fn [x y] (and x y)) or #(and %1 %2). The macro is
expanded inside the anonymous function body at read-time. When this
function is passed to another function such as reduce, a function
object rather than a macro name is passed.

Macro calls are processed at read-time.
)

; ---

#_(
The 'gotcha! moment comes when your read this:

"Since macros don't evaluate their arguments, unquoted function names
can be passed to them and calls to the functions with arguments can be
constructed. Function definitions cannot do this and instead must be
passed anonymous functions that wrap calls to functions."

(defmacro around-zero [number negative-expr zero-expr positive-expr]
  `(let [number# ~number] ; so number is only evaluated once
    (cond
      (< (Math/abs number#) 1e-15) ~zero-expr
      (pos? number#) ~positive-expr
      true ~negative-expr)))

 The Reader expands calls to the around-zero macro into a call to the
let special form. That contains a call to the cond function whose
arguments are its conditions and return values. The let special form
is used here for efficiency in the event that the first parameter,
number, is passed as an expression instead of a simple value. It
evaluates number once and then uses its value in two places within the
cond. The auto-gensym number# is used to generate a unique symbol name
so there is no chance the binding name can conflict with that of
another symbol. This enables the creation of hygienic macros.

The back-quote (a.k.a. syntax quote) at the beginning of the macro
definition prevents everything inside from being evaluated unless it
is unquoted. This means the contents will appear literally in the
expansion, except items preceded by a tilde (in this case, number,
zero-expr, positive-expr and negative-expr). When a symbol name is
preceded by a tilde inside a syntax quoted list, its value is
substituted. Bindings in syntax quoted lists whose values are
sequences can be preceded by ~@ to substitute their individual items.
)

(defn number-category [number]
  (around-zero number "negative" "zero" "positive"))

#_(
Here's a macro that takes two arguments. The first is a function that
expects one argument that is a number of radians, such as a
trigonometry function. The second is a number of degrees. If this were
written as a function instead of a macro, we would have to pass
#(Math/sin %) instead of simply Math/sin for the function. Note the
use of the # suffix to generate unique, local binding names. This is
sometimes necessary to avoid collisions with other bindings. Both #
and ~ must be used inside a syntax quoted list.
)

(defmacro trig-y-category [fn degrees]
  `(let [radians# (Math/toRadians ~degrees)
         result# (~fn radians#)]
     (number-category result#)))

(doseq [angle (range 0 360 90)] ; 0, 90, 180 and 270
  (println (trig-y-category Math/sin angle)))

#_(
The auto-gensym number# is used to generate a unique symbol name so
there is no chance the binding name can conflict with that of another
symbol. This enables the creation of hygienic macros.
)


(defmacro alpha-macro [number]
  `(let [rand-number# (rand)
         param-number# ~number]
                 (/ param-number# rand-number#)))

; Clojure> rand
; #<core$rand clojure.core$rand@618d0daa>
; Clojure> #(rand)
; #<sandbox9277$eval9404$fn__9405 sandbox9277$eval9404$fn__9405@14abd854>
; Clojure> (rand)
; 0.3381320243820001
; Clojure>

(defmacro beta-macro [function number]
  `(let [param-number# ~number
         ;param-function# ~function ;didn't work in this way
         res# (~function ~number)]
           ;(param-function# param-number#)
           res#
   ))
(beta-macro Math/sin 3)

;-
