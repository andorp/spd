
import Test.Themis.Test
import Test.Themis.Test.Asserts
import Test.Themis.Provider.Interactive

{-
Exercise 30.
Design the function string-first, which extracts the first character from a non-empty string.
Don't worry about empty strings.
-}

-- | Extracts the first character from a non-empty string
stringFirst :: String -> Char
stringFirst (x:_) = x

stringFirstTests = do
  assertEquals "a" 'a' (stringFirst "a") "Not the expected first character."
  assertEquals "bnb" 'b' (stringFirst "bnb") "Not the expected first character."

{-
Exercise 31.
Design the function string-last, which extracts the last character from a non-empty string.
-}

-- | Extracts the last character of a non-empty string
stringLast :: String -> Char
stringLast [x] = x
stringLast (_:xs) = stringLast xs

stringLastTests = do
  assertEquals "a" 'a' (stringLast "a") "Not the expected last character."
  assertEquals "abcd" 'd' (stringLast "abcd") "Not the expected last character."

{-
Exercise 33.
Design the function string-rest, which produces a string like the given one with
the first character removed.
-}

-- | Removes the first character from a string
stringRest :: String -> String
stringRest []     = []
stringRest (_:xs) = xs

stringRestTests = do
  assertEquals "\"\"" "" (stringRest "") "Empty string was consumed incorreclty"
  assertEquals "a" "" (stringRest "a") "One length string was consumed incorrectly"
  assertEquals "abcd" "bcd" (stringRest "abcd") "Longer than one string was consumed incorrectly"

{-
Exercise 34.
Design the function string-remove-last, which produces a string like the given one
with the last character removed.
-}

-- | Removes the last character of the string
stringRemoveLast :: String -> String
stringRemoveLast [] = []
stringRemoveLast [x] = []
stringRemoveLast (x:xs) = x : stringRemoveLast xs

stringRemoveLastTests = do
  assertEquals "" "" (stringRemoveLast "") "Empty string consumed incorrectly"
  assertEquals "a" "" (stringRemoveLast "a") "One length string consumed incorrectly"
  assertEquals "abcd" "abc" (stringRemoveLast "abcd") "Longer than one string was removed incorrectly"

{-
The Process: Once you understand how to represent input information as data and
to interpret output data as information, the design of an individual function
proceeds according to a straightforward process:

1) Express how you wish to represent information as data. A one-line comment
suffices, for example,
-- We use plain numbers to represent temperatures.

Formulate data definitions, like the one for Temperature above for the classes
of data you consider critical for the success of your program.

2) Write down a signature, a purpose statement, and a function header.

A function signature (shortened to signature here) is a BSL comment that tells
the readers of your design how many inputs your function consumes, from what
collection of data they are drawn, and what kind of output data it produces.
Here are three examples:

for a function that consumes one string and produces a number:
; String -> Number

for a function that consumes a temperature and that produces a string:
; Temperature -> String

As this signature points out, introducing a data definition as an alias for
an existing form of data makes it easy to read the intention behind signatures.
Nevertheless, we recommend to stay away from aliasing data definitions for now.
A proliferation of such names can cause quite some confusion. It takes practice
to balance the need for new names and the readability of programs, and there
are more important ideas to understand for now.

for a function that consumes a number, a string, and an image and that produces
an image:
; Number -> String -> Image -> Image

A purpose statement is a BSL comment that summarizes the purpose of the function
in a single line. If you are ever in doubt about a purpose statement, write down
the shortest possible answer to the question

what does the function compute?

Every reader of your program should understand what your functions compute without
having to read the function itself. A multi-function program should also come with
a purpose statement. Indeed, good programmers write two purpose statements:
one for the reader who may have to modify the code and another one for the person
who wishes to use the program but not read it.

Finally, a header is a simplistic function definition, also called a stub.
Pick one parameter for each input data class in the signature; the body of the
function can be any piece of data from the output class. The following three
function headers match the above three signatures:
(define (f a-string) 0)

(define (g n) "a")

(define (h num str img) (empty-scene 100 100))

Our parameter names reflect what kind of data the parameter represents. Sometimes,
you may wish to use names that suggest the purpose of the parameter. When you
formulate a purpose statement, it is often useful to employ the parameter names
to clarify what is computed. For example,
; Number String Image -> Image
; add s to img, y pixels from top, 10 pixels to the left
(define (add-image y s img)
  (empty-scene 100 100))
At this point, you can click the RUN button and experiment with the function.
Of course, the result is always the same value, which makes these experiments
quite boring.

3) Illustrate the signature and the purpose statement with some functional
examples. To construct a functional example, pick one piece of data from each
input class from the signature and determine what you expect back.

Suppose you are designing a function that computes the area of a square.
Clearly this function consumes the length of the square's side, and that is
best represented with a (positive) number. The first process step should
have produced something like this:

; Number -> Number
; compute the area of a square whose side is len
(define (area-of-square len) 0)
Add the examples between the purpose statement and the function header:

; Number -> Number
; compute the area of a square whose side is len
; given: 2, expect: 4
; given: 7, expect: 49
(define (area-of-square len) 0)

4) The next step is to take inventory, to understand what are the givens
and what we do need to compute. For the simple functions we are considering
right now, we know that they are given data via parameters. While parameters
are placeholders for values that we don't know yet, we do know that it is
from this unknown data that the function must compute its result. To remind
ourselves of this fact, we replace the function's body with a template.

For now, the template contains just the parameters, e.g.,
; Number -> Number
; compute the area of a square whose side is len
; given: 2, expect: 4
; given: 7, expect: 49
(define (area-of-square len)
  (... len ...))
The dots remind you that this isn't a complete function, but a template,
a suggestion for an organization. The templates of this section look boring.
Later, when we introduce complex forms of data, templates become interesting,
too.

5) It is now time to code. In general, to code means to program, though often
in the narrowest possible way, namely, to write executable expressions and
function definitions.

To us, coding means to replace the body of the function with an expression
that attempts to compute from the pieces in the template what the purpose
statement promises. Here is the complete definition for area-of-square:
; Number -> Number
; compute the area of a square whose side is len
; given: 2, expect: 4
; given: 7, expect: 49
(define (area-of-square len)
  (sqr len))
To complete the add-image function takes a bit more work than that:
; Number String Image -> Image
; add s to img, y pixels from top, 10 pixels to the left
; given:
; 5 for y,
; "hello" for s, and
; (empty-scene 100 100) for img
; expected:
; (place-image (text "hello" 10 "red") 10 5 (empty-scene 100 100))
(define (add-image y s img)
  (place-image (text s 10 "red") 10 y img))
In particular, the function needs to turn the given string s into an image,
which is then placed into the given scene.

6) The last step of a proper design is to test the function on the examples
that you worked out before. For now, click the RUN button and enter function
applications that match the examples in the interactions area:
> (area-of-square 2)
4
> (area-of-square 7)
49
The results must match the output that you expect; you must inspect each
result and make sure it is equal to what is written down in the example portion
of the design. If the result doesn't match the expected output, consider the
following three possibilities:
* You miscalculated and determined the wrong expected output for some of the examples.
* Alternatively, the function definition computes the wrong result.
  When this is the case, you have a logical error in your program, also known as a bug.
* Both the examples and the function definition are wrong.

When you do encounter a mismatch between expected results and actual values,
we recommend that you first re-assure yourself that the expected results are correct.
If so, assume that the mistake is in the function definition. Otherwise, fix
the example and then run the tests again. If you are still encountering problems,
you may have encountered the third, somewhat rare situation.
-}
