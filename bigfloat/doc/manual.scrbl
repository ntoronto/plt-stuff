#lang scribble/manual

@(require scribble/eval
          "common.rkt")

@title{Bigfloat: Arbitrary-precision floating-point numbers and functions}
@author{@(author+email "Neil Toronto" "neil.toronto@gmail.com")}

@defmodule["../bigfloat.rkt"]

@section[#:tag "intro"]{Introduction}

Bigfloat is a pure Racket library for representing and manipulating floating-point numbers with any precision. It is verified through deterministic and randomized testing (but not proved) to correctly implement its operations, and correctly round its results.

Bigfloat is written in Typed Racket. Of course, to untyped Racket programs it looks untyped, with the types automagically converted to contracts.

@section[#:tag "quick"]{Quick start}

@(itemlist #:style 'ordered
@item{Require the Bigfloat module with @(racket (require "bigfloat.rkt")).}
@item{Set the working precision using @(racket (bf-bits <some-number-of-bits>)).}
@item{Convert any @(racket Real) value or properly formatted @(racket String) to a @(racket bigfloat) using @(racket (bf <some-real-or-string>)).}
@item{Operate on @(racket bigfloat) values using ``bf''-prefixed functions like @(racket bf+) and @(racket bfsin).}
@item{Convert @(racket bigfloat)s to @(racket Real) values using @(racket bigfloat->rational), @(racket bigfloat->float), and @(racket bigfloat->integer). Convert them to @(racket String)s using @(racket bigfloat->string).}
)

To see some weakly motivated examples, continue on to the FAQ.

@section{Fictionally Asked Questions}

@subsection[#:tag "why bigfloat"]{Why use Bigfloat?}

There are a few reasons.

@bold{Reason:} To calculate, with arbitrary precision, irrational numbers and irrational functions of floating-point numbers.

@(bfexamples (bf-bits 16) (bfsqrt (bf 3))
             (bf-bits 179) (bf-pi))

A flonum has a 53-bit* @margin-note*{* The most significant bit in a flonum's significand is implicit, leaving one more bit for the sign, for a total of 1+11+52=64 bits.} @italic{significand} (we'll say it has 53 bits of @italic{precision}) and an 11-bit @italic{exponent}. A @(racket bigfloat) has an arbitrary precision of at least 2 bits and an unbounded exponent range.

@bold{Reason:} To compute ridiculously large or small numbers with confidence.

@(bfexamples (bf-bits 64)
             (bfexp (bfexp (bfexp (bf 4))))
             (bflog (bflog (bflog (bfexp (bfexp (bfexp (bf 4))))))))

@margin-note*{* Be careful with repeated exponentiation! The result of @(racket (bfexp (bfexp (bfexp (bf 100))))) has an @italic{exponent} that won't fit in memory on @italic{anyone's} computer---it would take millions of yottabytes. Bigfloat will try things like this anyway, and eventually run out of memory.}
The exponent in the result of @(racket (bfexp (bfexp (bfexp (bf 4))))) is well outside the exponent range of almost all floating-point libraries. Additionally, the result can't fit in memory as an exact rational.*

@bold{Reason:} To verify your floating-point hardware.

@hyperlink["http://en.wikipedia.org/wiki/IEEE_754-2008"]{IEEE 754-2008} stipulates that conforming implementations must correctly round the results of all operations. Roughly speaking, results can't be more than half a bit off, where the bit in question is the least significant in the significand.

Of course, implementations that don't adhere to the standard may not be so accurate, and implementations that @italic{claim} to adhere to the standard may not, either.

Because Bigfloat has been verified correct, you can use it to verify, in turn, the floating-point implementation on your CPU. For example, on my laptop, evaluating @(racket (exp 400)) results in @(racket 5.221469689764346e+173). But Bigfloat says it should be

@(bfexamples (bf-bits 53)
             (bigfloat->float (bfexp (bf 400))))

My laptop is about 184.2 ulps off, meaning the trailing 7.5 bits are wrong*. @margin-note*{* See @(racket bfulp) for the meaning of @italic{ulp}, and @(racket exact-ulp-error) and @(racket bfulp-error) for measuring error in ulps.} On the other hand, my desktop computes @(racket 5.221469689764144e+173) as it should.

@subsection[#:tag "why not bigfloat"]{When shouldn't I use Bigfloat?}

When you need speed. Bigfloat functions can be hundreds to thousands times slower than flonum functions.

That's not to say that they're @italic{inefficient}. For example, @(racket bflog) implements the algorithm with the best known asymptotic complexity. It just doesn't run directly in silicon, and it can't take shortcuts that implementations using fixed precision can take.

@subsection[#:tag "why not ffi"]{Why don't you just make a foreign function interface with [my favorite C/C++ numeric package]?}

I have an incomplete FFI to @hyperlink["http://www.mpfr.org"]{MPFR} that I plan to finish. I will likely use it as a Bigfloat backend when it is installed, to compute results that have exponents within its range.

@subsection[#:tag "why not prove"]{Why don't you @italic{prove} that the results are correctly rounded?}

Some algorithms are proved to round correctly, such as those for conversion to and from @(racket Real), for arithmetic, and for square root. Some could be with a little more work, such as the algorithm for @(racket bf-pi).

For the rest: it takes a lot of time to discover and prove epsilon bounds, to wrap things in Ziv loops, and to determine all the exact cases so the loops terminate.

However, it's something that can be done independently and piecemeal, so anyone is welcome to do it and submit patches. Fortunately, the @hyperlink["http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.103.7305"]{theoretical work} has already been done by the fine folks who make MPFR, and many others. It would be nice to see it implemented functionally.

@subsection[#:tag "why pi"]{Why is the last digit of @(racket (bf-pi)) not rounded correctly?}

All @italic{bits} but the last should be correct, and the last bit should be correctly rounded. This doesn't guarantee that the last digit will be.

A decimal digit represents, at most, log(10)/log(2) bits, which is about 3.3. Because the decimal/bit boundary never lines up except at the decimal point, when Bigfloat prints fractional digits, it always has to output at least one trailing digit that represents fewer than 3.3 bits. So it's actually rare for the last digit of a @(racket bigfloat)'s string representation to be correct.

@section{Bigfloat API}

@defparam[bf-bits precision Exact-Positive-Integer]{
A @seclink["Parameters" #:doc '(lib "scribblings/reference/parameters.scrbl")]{parameter} that determines the precision of new @(racket bigfloat) values. Return values from Bigfloat library functions are @(racket bigfloat)s with @(racket (bf-bits)) precision, no matter what the precisions of the inputs. (There are a few exceptions to this rule, and none of them are standard mathematical functions. The exceptions are noted in this manual.)

This parameter has a guard that ensures @(racket (bf-bits)) > @(racket 1) because one-bit floats can't be correctly rounded.
}

@defstruct*[bigfloat ([sig Integer] [exp Integer])]{
@margin-note*{* If you must construct a floating-point number from scratch, consider @(racket new-bigfloat), which rounds the result to @(racket (bf-bits)) bits.}
Represents an arbitrary-precision floating-point value. Generally, you would not construct @(racket bigfloat)s directly, but use @(racket bf) instead to convert @(racket Real) and @(racket String) values*.

There are no @(racket bigfloat)s that represent @(racket +inf.0), @(racket -inf.0), or @(racket +nan.0). Because exponents are unbounded, infinity rarely comes up. Bigfloat library functions raise exceptions instead of returning NaNs.

A @(racket bigfloat) consists of a significand and an exponent, both signed. If @(racket x) = @(racket (bigfloat s e)), its rational value is @(racket (* s (expt 2 e))). Its precision is @(racket (integer-length (abs s))), or @(racket (bigfloat-bits x)).
}

@subsection[#:tag "construction"]{Construction and conversion}

@defproc[(bf [value (U String Real)]) bigfloat]{
Constructs a @(racket bigfloat) from a @(racket String) or a @(racket Real).

@(bfexamples (bf-bits 128)
             (bf 4)
             (bf 1/7)
             (bf #e4.1)
             (bf "15e200000000000000"))

In the last example, the result of @(racket (bf "15e200000000000000")) is displayed as a string conversion because the exact rational number is too large. (String conversions are displayed when the exponent magnitude is greater than 10000 decimal.)

Converting from flonum constants is generally a bad idea because flonums have only 53 bits precision. Prefix flonum constants you pass to @(racket bf) with @tt{#e} to turn them into @(racket Exact-Rational) constants.
}

@defproc[(bf2^ [i Integer]) bigfloat]{
Equivalent to @(racket (bf (expt 2 i))) and @(racket (bfexp2 (bf i))), but much faster.

@(bfexamples (bf2^ 4)
             (bf2^ -4))
}

@deftogether[(
@defproc[(bigfloat->integer [x bigfloat]) Integer]
@defproc[(bigfloat->float [x bigfloat]) Float]
@defproc[(bigfloat->rational [x bigfloat]) Exact-Rational])]{
Convert @(racket bigfloat)s to @(racket Integer), @(racket Float), and @(racket Exact-Rational) values.

Only @(racket bigfloat->rational) returns an exact value; @(racket bigfloat->integer) truncates @(racket x) first, and @(racket bigfloat->float) rounds @(racket x) to 53 bits precision first.

@(bfexamples (bf-bits 64)
             (bigfloat->integer (bf 21/10))
             (define x (bf 1/7))
             (bigfloat->float x)
             (bigfloat->rational x)
             (rationalize (bigfloat->rational x) (expt 2 (- (bf-bits))))
             (bf= x (rational->bigfloat (bigfloat->rational x))))

@bold{Be careful} with @(racket bigfloat->integer) and @(racket bigfloat->rational). It can be easy, especially when using exponentials, to create a @(racket bigfloat) value that won't fit in memory as an @(racket Integer) or @(racket Exact-Rational).
}

@defproc[(bigfloat->string [x bigfloat]) String]{
Converts @(racket bigfloat)s to @(racket String)s.

The string includes enough digits that @(racket string->bigfloat) reconstructs the @(racket bigfloat). It will be in scientific notation when the number is large or small enough.

@(bfexamples (bigfloat->string (bf 4))
             (bigfloat->string (bf #e0.0001))
             (bigfloat->string (bf #e0.00000000000000000000000001))
             (bigfloat->string
              (bf 10000000000000000000000000000000000000000000)))
}

@defproc[(bigfloat-fields->string [x bigfloat]) String]{
Converts @(racket bigfloat)s to @(racket String)s of the form @(racket "significand*2^exponent").
         
@(bfexamples (bigfloat-fields->string (make-bigfloat 2 -1))
             (bigfloat-fields->string (bf 1)))
}

@defproc[(string->bigfloat [s String]) (U bigfloat False)]{
Converts @(racket String)s to @(racket bigfloat)s. If @(racket s) doesn't parse, @(racket string->bigfloat) returns @(racket #f).

@(bfexamples (string->bigfloat "0.14285714285714285714")
             (string->bigfloat "1*2^0")
             (string->bigfloat "square root of two"))
}

@deftogether[(
@defproc[(bftruncate [x bigfloat]) bigfloat]
@defproc[(bffloor [x bigfloat]) bigfloat]
@defproc[(bfceiling [x bigfloat]) bigfloat]
@defproc[(bfround [x bigfloat]) bigfloat])]{
These functions truncate, floor, round, and ceiling @(racket bigfloat)s.

@(bfexamples (bftruncate (bf 1.5))
             (bftruncate (bf -1.5))
             (bffloor (bf 1.5))
             (bffloor (bf -1.5))
             (bfceiling (bf 1.5))
             (bfceiling (bf -1.5)))

Rounding is to the nearest integer, with ties broken by rounding to even.

@(bfexamples (bfround (bf 1.5))
             (bfround (bf 2.5))
             (bfround (bf -1.5))
             (bfround (bf -2.5)))
}

@subsection[#:tag "properties"]{Properties}

@defproc[(bigfloat-bits [x bigfloat]) Nonnegative-Fixnum]{
Returns the precision, in bits, of a @(racket bigfloat).

@(bfexamples (bf-bits 64)
             (bigfloat-bits (bf 1/7))
             (define x (bf 1/2))
             (define y (bf 3/2))
             (bf-bits 128)
             (bigfloat-bits x)
             (bigfloat-bits (bf+ x y)))
}

@defproc[(bfulp [x bigfloat]) Exact-Rational]{
Returns the value of the @italic{unit in last place}, or the value of the lowest significant bit in the significand.

Think of an ulp as the smallest change that can possibly be made to a @(racket bigfloat) without changing its exponent. (To actually make these ``smallest changes,'' see @(racket bfnext) and @(racket bfprev).)

@(bfexamples (bfulp (bf 1/7))
             (bf (bfulp (bf 1/7)))
             (bf- (bf 1/7) (bfprev (bf 1/7))))
}

@defproc[(bflog2-ulp [x bigfloat]) Integer]{
Returns log-base-two of @(racket (bfulp x)). As a happy accident due to how the Bigfloat library represents floating-point numbers, @(racket bflog2-ulp) is a synonym for @(racket bigfloat-exp).
}

@defproc[(bfsgn [x bigfloat]) Fixnum]{
Returns @(racket -1) if @(racket x) is negative, @(racket 0) if @(racket x) is zero, and @(racket 1) if @(racket x) is positive.

@(bfexamples (bfsgn (bf -123))
             (bfsgn (bf 0))
             (bfsgn (bf 1/123)))
}

@deftogether[(
@defproc[(bfnegative? [x bigfloat]) Boolean]
@defproc[(bfzero? [x bigfloat]) Boolean]
@defproc[(bfpositive? [x bigfloat]) Boolean])]{
Predicate versions of @(racket bfsgn).
}

@defproc[(bfinteger? [x bigfloat]) Boolean]{
Returns @(racket #t) if and only if @(racket (bigfloat->rational x)) = @(racket (bigfloat->integer x)).
}

@subsection[#:tag "comparison"]{Comparison}

@deftogether[(
@defproc[(bf= [x bigfloat] [y bigfloat]) Boolean]
@defproc[(bf!= [x bigfloat] [y bigfloat]) Boolean]
@defproc[(bf> [x bigfloat] [y bigfloat]) Boolean]
@defproc[(bf< [x bigfloat] [y bigfloat]) Boolean]
@defproc[(bf>= [x bigfloat] [y bigfloat]) Boolean]
@defproc[(bf<= [x bigfloat] [y bigfloat]) Boolean])]{
Standard comparison functions.
}

@deftogether[(
@defproc[(bfmax [x bigfloat] [y bigfloat] ...) bigfloat]
@defproc[(bfmin [x bigfloat] [y bigfloat] ...) bigfloat])]{
Return the maximum and minimum of their arguments, respectively.
}

@subsection[#:tag "arithmetic"]{Arithmetic}

@deftogether[(
@defproc[(bf+ [x bigfloat] [y bigfloat]) bigfloat]
@defproc[(bf- [x bigfloat] [y bigfloat]) bigfloat]
@defproc[(bf* [x bigfloat] [y bigfloat]) bigfloat]
@defproc[(bf/ [x bigfloat] [y bigfloat]) bigfloat]
@defproc[(bfadd1 [x bigfloat]) bigfloat]
@defproc[(bfsub1 [x bigfloat]) bigfloat]
@defproc[(bfsqr [x bigfloat]) bigfloat]
@defproc[(bfabs [x bigfloat]) bigfloat])]{
Standard arithmetic functions.
}

@defproc[(bfneg [x bigfloat]) bigfloat]{
Negates @(racket x). Equivalent to @(racket (bf- (bf 0) x)), but much faster.
}

@defproc[(bfinv [x bigfloat]) bigfloat]{
Returns the multiplicative inverse of @(racket x). Equivalent to @(racket (bf/ (bf 1) x)), but a little faster.
}

@defproc[(bf*i [x bigfloat] [i Integer]) bigfloat]{
Equivalent to @(racket (bf* x (bf i))), but a little faster.
}

@defproc[(bfexact-quotient [x bigfloat] [y bigfloat]) bigfloat]{
Returns @(racket (bigfloat->integer (bf/ x y))), except it uses enough bits in calculating the result that the result is exact. (It may be that @(racket (bigfloat->integer (bf/ x y))) is so large that it cannot be represented using @(racket (bf-bits)) bits.)
}

@defproc[(bfremainder [x bigfloat] [y bigfloat]) bigfloat]{
Returns @(racket (bf- x (bf*i y (bfexact-quotient x y)))), but computed using enough bits that the result is exact.
}

@defproc[(bfsum [xs (Listof bigfloat)]) bigfloat]{
Returns the sum of the @(racket bigfloat)s in @(racket xs), computed using enough bits that the result is exact. Just using @(racket foldl) or a loop to add will accumulate rounding errors; use @(racket bfsum) instead.
}

@deftogether[(
@defproc[(bf*2 [x bigfloat]) bigfloat]
@defproc[(bf/2 [x bigfloat]) bigfloat]
@defproc[(bf*4 [x bigfloat]) bigfloat]
@defproc[(bf/4 [x bigfloat]) bigfloat]
@defproc[(bf*2^ [x bigfloat] [i Integer]) bigfloat]
@defproc[(bf/2^ [x bigfloat] [i Integer]) bigfloat])]{
Equivalent to @(racket (bf* x (bf 2))), @(racket (bf/ x (bf 2))), @(racket (bf* x (bf 4))), @(racket (bf/ x (bf 4))), @(racket (bf* x (bf (expt 2 i)))), and @(racket (bf/ x (bf (expt 2 i)))), but much faster because they only adjust the exponent.
}

@subsection[#:tag "constants"]{Cached constants}

@defform[(define-bf-constant id e ...+)]{
Defines a new constant. Constants defined using @(racket define-bf-constant) are cached at certain precisions and rounded to target precision when asked for.

@(bfexamples (define-bf-constant bf-1/7 (bf 1/7))
             (bf-bits 64)
             (bf-1/7)
             (bf-bits 128)
             (bf-1/7))

In addition to binding @(racket id), for debugging purposes, @(racket define-bf-constant) binds @(racket id-cache) to the cache. To allow a little manual cache management, it also binds @(racket id-clear) to a zero-argument procedure that clears @(racket id)'s cache.

@(bfexamples bf-1/7-cache
             (bf-1/7-clear)
             bf-1/7-cache)

Don't write programs that depend on @(racket id-cache)'s type or contents, or on how @(racket id) builds the cache.
}

@deftogether[(
@defproc[(bf-pi) bigfloat]
@defproc[(bf-log-2) bigfloat]
@defproc[(bf-log-10) bigfloat]
@defproc[(bf-e) bigfloat])]{
Cached constants for pi, @(racket (bflog (bf 2))), @(racket (bflog (bf 10))), and @(racket (bfexp (bf 1))), respectively.

@(bfexamples (bf-pi)
             (bf-log-2)
             (bf-log-10)
             (bf-e))
}

@subsection[#:tag "functions"]{Functions}

@deftogether[(
@defproc[(bfsqrt [x bigfloat]) bigfloat]
@defproc[(bfexp [x bigfloat]) bigfloat]
@defproc[(bfexpt [b bigfloat] [x bigfloat]) bigfloat]
@defproc[(bfexp2 [x bigfloat]) bigfloat]
@defproc[(bfexp10 [x bigfloat]) bigfloat]
@defproc[(bflog [x bigfloat]) bigfloat]
@defproc[(bflogb [b bigfloat] [x bigfloat]) bigfloat]
@defproc[(bflog2 [b bigfloat] [x bigfloat]) bigfloat]
@defproc[(bflog10 [b bigfloat] [x bigfloat]) bigfloat]
@defproc[(bfsin [x bigfloat]) bigfloat]
@defproc[(bfcos [x bigfloat]) bigfloat]
@defproc[(bftan [x bigfloat]) bigfloat]
@defproc[(bfasin [x bigfloat]) bigfloat]
@defproc[(bfacos [x bigfloat]) bigfloat]
@defproc[(bfatan [x bigfloat]) bigfloat]
@defproc[(bfsinh [x bigfloat]) bigfloat]
@defproc[(bfcosh [x bigfloat]) bigfloat]
@defproc[(bftanh [x bigfloat]) bigfloat]
@defproc[(bfasinh [x bigfloat]) bigfloat]
@defproc[(bfacosh [x bigfloat]) bigfloat]
@defproc[(bfatanh [x bigfloat]) bigfloat])]{
Standard mathematical functions: square root, exponential, exponentials with arbitrary base, logarithm, logarithms with arbitrary base, trigonometric, inverse trigonometric, hyperbolic, and inverse hyperbolic.

(Note: inverse hyperbolic functions aren't implemented yet.)
}

@defproc[(bfagm [x bigfloat] [y bigfloat]) bigfloat]{
Computes the @hyperlink["http://en.wikipedia.org/wiki/Arithmetic-geometric_mean"]{arithmetic-geometric mean} of @(racket x) and @(racket y). This is typically not directly useful, but it is integral to some algorithms such as the one that computes @(racket bflog).
}

@margin-note*{Some Bigfloat functions, like @(racket bf>), use @(racket bfceiling-log2) and @(racket bffloor-log2) to make fast, coarse-grained semidecisions before making relatively computationally intensive, fine-grained decisions.}
@deftogether[(
@defproc[(bfceiling-log2 [x bigfloat]) Integer]
@defproc[(bffloor-log2 [x bigfloat]) Integer])]{
Equivalent to @(racket (bigfloat->integer (bfceiling (bflog2 x)))) and @(racket (bigfloat->integer (bffloor (bflog2 x)))), respectively, but much, much faster. }

@subsection[#:tag "stats"]{Randomness and statistics}

@deftogether[(
@defproc[(bfrandom) bigfloat]
@defproc[(bfrandom-signed) bigfloat])]{
Returns uniformly distributed random @(racket bigfloat)s in the ranges [0,1] and [-1,1], respectively.
}

@;(: bfrunif (bigfloat bigfloat -> bigfloat))
@;(: bfrnorm2 (-> (values bigfloat bigfloat)))
@;(: bfrnorm (case-lambda (-> bigfloat) (bigfloat bigfloat -> bigfloat)))
@;(: bfrcauchy (case-lambda (-> bigfloat) (bigfloat bigfloat -> bigfloat)))
@;(: bfexpected-value ((bigfloat -> bigfloat) (Listof bigfloat) -> bigfloat))
@;(: bfmean ((Listof bigfloat) -> bigfloat))
@;(: bfvariance ((Listof bigfloat) -> bigfloat))
@;(: bfstddev ((Listof bigfloat) -> bigfloat))

@subsection[#:tag "error"]{Measuring error}

@defproc[(exact-ulp-error [x bigfloat] [r Exact-Rational]) Exact-Rational]{
Returns the absolute difference, in ulps with respect to @(racket x), between @(racket x) and @(racket r). (For the definition of ulp, see @(racket bfulp).)

Because Bigfloat functions round correctly, the following examples return error less than 0.5 ulps.

@(bfexamples (bf (exact-ulp-error (bf/ (bf 1) (bf 7)) 1/7))
             (bf (exact-ulp-error (bf* (bf 1/7) (bf 14/13))
                                  (* (bigfloat->rational (bf 1/7))
                                     (bigfloat->rational (bf 14/13))))))

However, Bigfloat functions can't guarantee that @italic{compositions} introduce no more than 0.5 ulps error.

@(bfexamples (bf (exact-ulp-error (bf* (bf 1/7) (bf 14/13))
                                  (* 1/7 14/13))))
}

@defproc[(bfulp-error [x bigfloat] [y bigfloat]) bigfloat]{
Returns the absolute difference, in ulps with respect to @(racket x), between @(racket x) and @(racket r). (For the definition of ulp, see @(racket bfulp).) Assuming that @(racket y) is exact, the result itself is correct within 0.5 ulps. To ensure the result means something, @(racket bfulp-error) requires that @(racket y) have at least as much precision as @(racket x).

Use this when it is not possible to test the accuracy of @(racket x) against an exact value.

@(bfexamples (bfulp-error (bfexp (bf 2))
                          (parameterize ([bf-bits  (* 4 (bf-bits))])
                            (bfexp (bf 2))))
             (define x (bflog (bf 2)))
             (bfulp-error (bfexp x)
                          (parameterize ([bf-bits  (* 2 (bf-bits))])
                            (bfexp x))))
}

@defform*[[(bftest-error expr)
           (bftest-error expr bits)]]{
Evaluates @(racket expr) at @(racket (bf-bits)) precision. The first form then evaluates @(racket expr) at @(racket (* 2 (bf-bits))) precision, and returns the difference in ulps as given by @(racket bfulp-error). The second form allows you to set the number of bits for the second evaluation.

@(bfexamples (bftest-error (bfexp (bf 2)))
             (bftest-error (bfexp (bf 2)) (* 4 (bf-bits))))

@(racket (bftest-error (bfexp (bf 2)) (* 4 (bf-bits)))) is equivalent to the first example in @(racket bfulp-error).
}

@;(: exact-relative-error (bigfloat Exact-Rational -> Exact-Rational))
@;(: bfrelative-error (bigfloat bigfloat -> bigfloat))

@subsection[#:tag "misc"]{Low-level functions}

@defproc[(new-bigfloat [sig Integer] [exp Integer]) bigfloat]{
An alternative to the @(racket bigfloat) constructor. In contrast to @(racket make-bigfloat), @(racket new-bigfloat) ensures that the constructed value has exactly @(racket (bf-bits)) bits precision.

@(bfexamples (bf-bits 4)
             (make-bigfloat 31 0)
             (new-bigfloat 31 0))

In this example, @(racket 31) has been rounded up to @(racket 32) because @(racket 31) doesn't fit in a four-bit float. On the other hand, @(racket 32) does, as @(racket (bigfloat #b1000 2)).
}

@deftogether[(
@defproc[(integer->bigfloat [i Integer]) bigfloat]
@defproc[(float->bigfloat [f Float]) bigfloat]
@defproc[(rational->bigfloat [r Exact-Rational]) bigfloat]
@defproc[(real->bigfloat [x Real]) bigfloat])]{
Convert real numbers to @(racket bigfloat)s; @(racket real->bigfloat) dispatches to the other three.

These conversion functions are for completeness. It's shorter and easier to use @(racket bf). It's not noticeably slower, especially given the computation done in most Bigfloat functions.
}

@deftogether[(
@defproc[(bfnext [x bigfloat] [n Integer 1]) bigfloat]
@defproc[(bfprev [x bigfloat] [n Integer 1]) bigfloat])]{
Return the next largest or next smallest @(racket bigfloat) with the same exponent.

@(bfexamples (bfnext (bf 1))
             (bfulp-error (bf 1) (bfnext (bf 1))))

@bold{These functions can (rarely) return values with different precisions than their inputs.}
}

@defproc[(bfnormalize [x bigfloat]) bigfloat]{
Returns a value close to @(racket x) or exactly @(racket x), but with exactly @(racket (bf-bits)) bits precision. If @(racket x) has more precision than @(racket (bf-bits)), it returns @(racket x) rounded to @(racket (bf-bits)) bits.

@(bfexamples (bf-bits 128)
             (define x (bf 1/7))
             x
             (bf-bits 64)
             (bfnormalize x))
}

@defform[(with-bf-bits bits exprs ...+)]{
Computes @(racket exprs) with @(racket bits) precision, then converts the result to the outer precision. Equivalent to @(racket (bfnormalize (parameterize ([bf-bits  bits]) exprs ...))).

@(bfexamples (bf/ (bfsin (bf 12)) (bfcos (bf 12)))
             (with-bf-bits (* 2 (bf-bits))
               (bf/ (bfsin (bf 12)) (bfcos (bf 12))))
             (with-bf-bits (add1 (bf-bits))
               (bf/ (bfsin (bf 12)) (bfcos (bf 12)))))
}

@defproc[(bfcanonicalize [x bigfloat]) bigfloat]{
@margin-note*{When hashing @(racket bigfloat)s, the Bigfloat library canonicalizes them first to ensure that equality implies an equal hash.}
Returns a new @(racket bigfloat) with no more bits of precision than are necessary to encode @(racket x) exactly, by removing all low-order zeros from the significand and adjusting the exponent. Two @(racket bigfloat)s are @(racket equal?) (equivalently @(racket bf=)) if and only if their canonicalized forms' struct fields are equal.

Canonicalizing @(racket bigfloat)s will not change answers computed from them, but may change how they convert to strings.

@(bfexamples (bf= (bf+ (bf 1/7) (bf 1/4))
                  (bf+ (bfcanonicalize (bf 1/7)) (bfcanonicalize (bf 1/4))))
             (bf2^ -2)
             (bfcanonicalize (bf2^ -2)))
}
