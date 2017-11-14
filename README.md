# cambda
Write lambdas in C++ within an embedded Lisp-like language. Header-only. Very 'constexpr'-friendly. The `c` in `cambda` stands for `constexpr`.

*{ Written by [Aaron McDaid](https://aaronmcdaid.github.io/) }*

In C++14, lambdas are very useful but they have some restrictions. They can't be used in certain contexts and they aren't very friendly with `constexpr`.

I developed this while working on my own range library, in order to be able to perform flexible testing of my ranges at compile time.

In plain C++, we can write this, but we can't mark them as `constexpr` and therefore we can't do a `static_assert` on the result:

```
auto cpp_lambda = [](auto x){return x*x;};
auto squared_cpp = cpp_lambda(15);
// static_assert(squared_cpp = 225 ,"");
```

With the `cambda` library, we can write this and use `constexpr`

```
#include "cambda.hh"

constexpr auto cambda_lambda = "(lambda [x] [{x * x}])"_cambda();
constexpr auto squared_cambda = cambda_lambda(15);
static_assert(squared_cambda = 225 ,"");
```

## Basics

This uses the *string-literal-operator-template* extension, but otherwise I believe it's standard C++14.
That extension is'nt strictly necessary, we can build a preprocessor macro to work around it.
This has been tested on `clang version 3.8.0`, `g++ (GCC) 7.2.0` and `g++ (GCC) 5.5.0`, please give me feedback for other compilers.
I suggest passing the `-Wno-gnu-string-literal-operator-template` argument to these compilers to suppress a warning that you
might get about the fact this is an extension.

`_cambda` is a user-defined string literal which parses a string. It must always be followed by `()` in order to "simplify" (i.e. "evaluate") the code.
If you've never seen Lisp before, the main thing is that functions are called by putting the function name *after* the parenthesis.
In C++, we write `foo(5, 3.14)`, but in Lisp we write `(foo 5 3.14)`. Note also that whitespace, instead of commas, is used to separated arguments.

```
    constexpr auto a = "15"_cambda();            // a is 15
    constexpr auto b = "(+ 8 7)"_cambda();       // Function call. This is addition. b is 15
    constexpr auto b = "(* 8 7)"_cambda();       // Multiplication
    constexpr auto b = "{8 * 7}"_cambda();       // If there are two args, use {} instead of () for infix notation
    constexpr auto b = "{ {8 * 7} + {6 * 3} }"_cambda();  // Nested application
```

Please note that the whitespace is important here. Unlike C++, Lisp is very flexible about what can appear in identifiers.
Symbols such as `+` are equivalent to letters.
Therefore `"2+2"_cambda()` will try to find a function with the name `2+2`.
That's we we need `(* 8 7)` and `{8 * 7}` instead of `(*8 7)` or `{8*7}`.

## Extra bindings

The 'standard library' is currently very very small. You can add your own bindings, names or functions, by
use `[ ]` after the `_cambda` and just before the `()` as follows:

```
constexpr auto four_squared = "{x * x}"_cambda ["x"_binding = 4] ();
```

```
std::vector<int> v{2,3,4};
auto size_of_v = "(size v)"_cambda
        [   "v"_binding = v
        ,   "size"_binding =
                        [](auto && x){return x.size();}
        ]   //  [] attaches the bindings
        (); //  () executes
```

## simple side effects

We can assign via `assign`:

```
int x;
"(assign x 1234)"_cambda ["x"_binding = x] ();
// x is now equal to 1234
```

## lambdas

We don't just want to return value, we want to return functions too.
The following are equivalent:

```
auto lambda_from_cpp = [](auto x, auto y) { return x+y; };
auto lambda_from_cambda = "(lambda [x y] [{x + y}])"_cambda();
lambda_from_cpp(5,6) == lambda_from_cambda(5,6)
```

## range_based_for
We can combine all this with this more complicated demo. The lambda in this
next example is essentially `[](auto &r) { r = r*r; }`, which modifies a
number by replacing it with its square.
This example then uses `range_based_for` to apply this modifier to each
element of an array.
The `range_based_for` function is implemented using [C++'s range-based-for](http://en.cppreference.com/w/cpp/language/range-for)
and hence should work for any container that has `std::begin` and `std::end`:

```
constexpr auto static
run()
{
        int test_data[] {5,6,7};
        "(range_based_for test_data (lambda [r] [(assign r {r * r})]))"_cambda
            ["test_data"_binding = test_data]
            ();
        return test_data[0]+test_data[1]+test_data[2];
}
static_assert( run() == 5*5 + 6*6 + 7*7 ,"");
```
