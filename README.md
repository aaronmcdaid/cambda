# cambda
Write lambdas in C++ within an embedded Lisp-like language. Header-only. Very 'constexpr'-friendly. The `c` in `cambda` stands for `constexpr`.

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
