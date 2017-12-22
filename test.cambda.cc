#include "cambda.hh"

#include <vector>
#include <iostream>

using cambda::operator"" _cambda;
using cambda::operator"" _cambda_empty_library;
using cambda::operator"" _charpack;
using cambda::operator"" _binding;

namespace testing_namespace_empty_library {
    auto constexpr
    plus(int a, int b) { return a + b; }

    struct a_lib_with_plus
    {

        template< typename LibToForward
                , typename Self
                , typename Ti
                , typename Tj >
        auto constexpr
        apply_after_simplification
            ( Self &&
            , LibToForward &&
            , decltype( "+"_charpack )
            , Ti && i
            , Tj && j) const
        ->decltype(std::forward<Ti>(i) + std::forward<Tj>(j) )
        {   return std::forward<Ti>(i) + std::forward<Tj>(j); }
    };

    constexpr auto b = "(+ 7 8)"_cambda_empty_library["+"_binding = plus]();
    static_assert(b == 15 ,"");
    //constexpr auto c = "(+ 7 8)"_cambda_empty_library[a_lib_with_plus{}](); // TODO: reenable this test
    //static_assert(c == 15 ,"");
}

constexpr auto a = "15"_cambda();            // a is 15
constexpr auto b = "(+ 8 7)"_cambda();       // Function call. This is addition. b is 15
constexpr auto c = "(* 8 7)"_cambda();       // Multiplication
constexpr auto d = "{8 * 7}"_cambda();       // If there are two args, use {} instead
                                             // of () for infix notation
constexpr auto e = "{ {8 * 7} + {6 * 3} }"_cambda();  // Nested application

static_assert(a == 15   ,"");
static_assert(b == 15   ,"");
static_assert(c == 56   ,"");
static_assert(d == 56   ,"");
static_assert(e == 74   ,"");


constexpr auto four_squared = "{x * x}"_cambda ["x"_binding = 4] ();
static_assert(four_squared == 16 ,"");

constexpr auto
foo()
{
    int y = 0;
    "{y = 4}"_cambda        // 'compile' a cambda, which performs an assignment
        ["y"_binding &= y]  // attach a binding to the C++ variable
        ();                 // 'execute' the cambda
    return y;
}
static_assert(foo() == 4 ,"");

#if 1
constexpr auto cambda_lambda = "(lambda [x] [{x * x}])"_cambda();
constexpr auto squared_cambda = cambda_lambda(15);
static_assert(squared_cambda == 225 ,"");

auto constexpr
test_lambda_with_binding()
{
    int y = 0;
    (void)y;
    auto cambda_lambda_bound = "(lambda [x] [{y = 2} {x * x}])"_cambda
        ["y"_binding &= y]
        ()
        ;
    auto res = cambda_lambda_bound(10);
    return y * res;
}
static_assert(test_lambda_with_binding() == 200 ,"");
#endif

constexpr auto a_integralconstant = "15c"_cambda();            // a is 15
static_assert(a_integralconstant.value == 15   ,"");

static_assert(42    == "(* 21 2)"_cambda ()   ,"");
static_assert(56088 == R"--(
                            ([] [left] 123)
                            ([] [right] 456)
                            (* left right)
                            )--"_cambda () ,"");

static_assert(17 == R"--(
                            ([] [x] 5)
                            (assign x {x * 2})
                            ([] [y] 7)
                            {x + y}
                            )--"_cambda () ,"");

static_assert(std::is_same<std::integral_constant<int,42>, decltype( "42c"_cambda() )>{}  ,"");
static_assert(std::is_same<int, decltype( "42"_cambda() )>{}  ,"");
static_assert(std::is_same<double, decltype( "3.4"_cambda() )>{}  ,"");
static_assert(42 ==                       "42"_cambda()       ,"");
static_assert(3.4 ==                        "3.4"_cambda()       ,"");
static_assert(3.41932 ==                        "3.41932"_cambda()       ,"");

static_assert("(if.constexpr truec  [3.14] ['hi'])"_cambda() == 3.14 ,"");
static_assert("(if.constexpr falsec [3.14] ['hi'c])"_cambda() == "hi"_charpack ,"");

std::vector<int> v{2,3,4};
auto size_of_v = "(size v)"_cambda
        [   "v"_binding &= v ]
        [  "size"_binding = // TODO: renable use of comma operator here
                        [](auto && x){return x.size();}
        ]   //  [] attaches the bindings
        (); //  () executes

std::initializer_list<int> il{2,3,4};

static_assert(cambda_utils::equal_string_array("hi", "hi") ,"");

static_assert("'it''s mine'"_cambda_empty_library()[0] == 'i' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[1] == 't' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[2] == '\'' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[3] == 's' ,"");
static_assert(cambda_utils::equal_string_array("'it''s mine'"_cambda_empty_library(), "it's mine") ,"");
static_assert(!cambda_utils::equal_string_array("'it''s mine'"_cambda_empty_library(), "it's mIne") ,"");
static_assert(!cambda_utils::equal_string_array("'it''s mine'"_cambda_empty_library(), "it's min") ,"");

static_assert("(length 'it''s mine'c)"_cambda() == 9,"");
static_assert(!("'a string'c"_cambda() == "'different'c"_cambda()) ,"");

static_assert(std::is_same<decltype("()"_cambda()) , cambda::nil_t>{} ,"");

void README_md_tests()
{
    constexpr auto a = "15"_cambda();            // a is 15
    constexpr auto b = "(+ 8 7)"_cambda();       // Function call. This is addition. b is 15
    constexpr auto c = "(* 8 7)"_cambda();       // Multiplication
    constexpr auto d = "{8 * 7}"_cambda();       // If there are two args, use {} instead of () for infix notation
    constexpr auto e = "{ {8 * 7} + {6 * 3} }"_cambda();  // Nested application

    static_assert(a == 15 ,"");
    static_assert(b == 15 ,"");
    static_assert(c == 56 ,"");
    static_assert(d == 56 ,"");
    static_assert(e == 74 ,"");
    static_assert("(lambda [x y] [{x + y}])"_cambda() (20,30) == 50   ,"20+30 should equal 50");
    /* This next line isn't accepted in C++14, whereas cambdas are useable in unevaluated contexts
    static_assert( [](auto x, auto y){ return x+y;}   (20,30) == 50   ,"20+30 should equal 50");
    */

    /* For complex cambdas, it's good to use raw string
     * literals (C++11) to enable multi-line strings.
     */
    static_assert(42 ==
    R"--(
        #()     A single-line comment is introduced by #().
        #()     strings are surrounded by single-quotes.

        #()     Each expression is evaluated in turn, and only
        #()     the last one is returned

        #()     New bindings can be introduced with ([] [name] value)

        ([] [six] 6)

        ([] [seven] 7)

        {six * seven}   #() returns 42
    )--"_cambda () ,""); // compute the factorial of 7.

    static_assert(49 ==
    R"--(
        #()     The 'lambda' function can create an anonymous function,
        #()     and we use a  #()  to give it a name

        ([] [square] (lambda [x] [{x * x}]))

        (square 7)
    )--"_cambda () ,""); // compute the factorial of 7.

    static_assert(174 ==
    R"--(
        #()     This is an example of a lambda that takes two arguments.
        #()     Also, the body of a lambda can have multiple statements.

        ([]
            [sum.of.square.and.cube]
                (lambda
                    [x y]       #() ... the two argument names
                    [           #() A multi-line lambda body starts here

                            #() First, two bindings inside the lambda
                            ([] [x.squared] {x * x})
                            ([] [y.cubed] {y * {y * y}})

                            #() Then, the return-expression from the lambda
                            {x.squared + y.cubed}
                    ]           #() ... end the lambda body
                )
        )

        (sum.of.square.and.cube 7 5)    #() returns 174 (7*7 + 5*5*5)
    )--"_cambda () ,"wrong result computed"); // compute the factorial of 7.

    static_assert(3.14 ==
    R"--(
        #()     'if' evaluates one of two expressions

        (if {5 > 3} [3.14] [2.718])

    )--"_cambda () ,"wrong result computed"); // compute the factorial of 7.

    static_assert(12 ==
    R"--(

    #() Begin with x==0

    ([] [x] 0)

    #() Now a while loop

    (while
        [{x < 10}]          #() condition for 'while'
        [{x = {x + 3}}]     #() add 3 to 'x' each time
    )

    #() We can't simply return 'x' here because it will return #() by
    #() reference for simple names. It kind of like this C++ code:
    #()         return std::forward<decltype(x)>(x);
    #() ... which would return a reference to a temporary.

    #() Therefore, we call a function ('ref2val') which simply
    #() returns it by value.

    (ref2val x)         #() return the final value of 'x', by value.

    )--"_cambda () ,"wrong result computed"); // compute the factorial of 7.

    static_assert("HelloWorld"_charpack ==
    R"--(
        #()     a single-line comment is introduced by #().
        #()     strings are surrounded by single-quotes.

        #()     Appending "c" to the end of the string gives
        #()     a compile-time string (char_pack<chars...>)

        #()     ++ concatenates two strings.

        {'Hello'c ++ 'World'c}
    )--"_cambda () ,""); // compute the factorial of 7.

    static_assert(5040 ==
    R"--(

        ([]
            [fact]
            (lambda
                [(& fact) n]
                [ (if {n < 1} [ 1 ] [ {n * (fact {n - 1})} ]) ]
            ))

        (lambda [N] [
            (fix
                (typeof 0)
                fact
                (ref2val N)
                )])

    )--"_cambda ()(7)
    ,""); // compute the factorial of 7.
};

int main() {
    int x=0;
    "(assign x 1234)"_cambda ["x"_binding = x] ();
    // x is now equal to 1234

    std:: cout << "x = " << x << " should be 1234" << '\n';
#if 1
    constexpr auto lambda_from_cambda = "(lambda [x y] [{x + y}])"_cambda();
    static_assert  (11 == lambda_from_cambda(5,6) ,"");
#endif

    auto product =
    "(mult 3c 4c)"_cambda
        ["mult"_binding = [](auto x, auto y){return std::integral_constant<int, x.value*y.value>{};}]
        ();
    static_assert(product.value == 12 ,"");

    constexpr
    auto z =
    R"--(
            ([] [r] 7)
            (ref2val r)
    )--"_cambda
        ();
    static_assert(z == 7 ,"");
    std::cout << z << '\n';
}

constexpr auto static
test_if_with_side_effects()
{
    int x = -1;
    int y = -1;
    "(if truec [(assign x 3)] [(assign x 5)])"_cambda["x"_binding &= x]();
    "(if falsec[(assign x 300)] [(assign x 500)])"_cambda["x"_binding &= y]();
    return x + y;
}
static_assert(test_if_with_side_effects() == 503 ,"");

constexpr auto static
test_ifNonConstant_with_side_effects()
{
    // very similar to the last test, but the boolean input is not constexpr in this case
    int x = -1;
    int y = -1;
    bool bt = true;
    bool bf = false;
    "(if bt  [(assign x 3)] [(assign x 5)])"_cambda       ["bt"_binding = bt]["x"_binding &= x](); // TODO: reenable comma operator here and on next line
    "(if bf  [(assign x 300)] [(assign x 500)])"_cambda   ["bf"_binding = bf]["x"_binding &= y](); // TODO: reenable comma operator here and on next line
    return x + y;
}
static_assert(test_ifNonConstant_with_side_effects() == 503 ,"");


#if 1
constexpr auto static
test_range_based_for()
{
        int test_data[] {5,6,7};
        R"--(
            (range_based_for
                test_data
                (lambda
                    [(& r)]
                    [
                        (assign r {r * r})
                    ]
                )
            )
            )--"_cambda
            ["test_data"_binding &= test_data]
            ();
        return test_data[0]+test_data[1]+test_data[2];
}
static_assert( test_range_based_for() == 5*5 + 6*6 + 7*7 ,"");
#endif

constexpr auto static
test_while()
{
    //int x = 10;
    int y = 0;
    R"--(
        (while
            [   ([] [max] 100)
                (< y max)       ]
            [   {y = {y + 1}}
                {y = {y + 2}}   ])
    )--"_cambda["y"_binding &= y]();
    return y;
}
static_assert(test_while() == 102 ,"");

#if 1
constexpr bool
test_partition()
{
        int a[] = {6,2,5,8,3,9,7};
        R"--(
                ([] [swap] (lambda [(& x) (& y)] [
                                            ([] [tmp] x)
                                            {x = y}
                                            {y = tmp}
                                    ]))
                (while
                    [{{b != e} && {{b + 1} != e}}]
                    [(if
                        {(* {b + 1}) < (* b)}
                        [
                                (swap (* {b + 1}) (* b))
                                (++ b)
                                ()
                        ]
                        [
                            (-- e)
                            (swap (* {b + 1}) (* e))
                            ()
                        ]
                    )]
                )
        )--"_cambda
                [   "b"_binding = std::begin(a)] // TODO: reenable comma operator
                [   "e"_binding = std::end  (a)] // TODO: reenable comma operator
                ();
        return cambda_utils::equal_array(a, (int[]){2,5,3,6,9,7,8});
}

static_assert(test_partition() ,"");
#endif

#if 1
constexpr bool
test_quicksort()
{
    constexpr auto quicksort_cambda =
    R"--(

    #() define a 'swap' function.
    ([] [swap]  (lambda [(& x) (& y)]       #() (& x)  means capture by reference
        [
                ([] [tmp] x)
                {x = y}
                {y = tmp}
        ]))

    #() 'partition': Takes two iterators, begin and end of a non-empty range
    #() The first value in the input range is the 'pivot'. This function
    #() rearranges the data such that the pivot is ordered after all the data
    #() points that are smaller than it, and ordered before larger data points.
    #()
    #() The 'quicksort' function will then be able to recursively call
    #() 'partition' on each of these two sub-ranges until everything is sorted.

    ([] [partition] (lambda [b e]
        [
            (while
                [{{b != e} && {{b + 1} != e}}]
                [(if
                    {(* {b + 1}) < (* b)}   #() dereference the iterators
                    [
                        (swap (* {b + 1}) (* b))
                        (++ b)
                        ()              #() return a value of 'nil_t' from this branch of the if
                    ]
                    [
                        (-- e)
                        (swap (* {b + 1}) (* e))
                        ()              #() return a value of 'nil_t' from this branch of the if
                    ]
                )]
            )
            (ref2val b)                 #() return the iterator to the pivot (by value)
        ]))

    #() As 'quicksort' will need to be recursive, it takes an extra argument which
    #() can be called to do the recursion. 'fix' will then pass quicksort "to itself"

    ([] [quicksort]     (lambda [(& quicksort') b0 e0]
        [(if {b0 != e0} [               #() check if the range to be sorted is non-empty
            ([] [iterator.to.pivot] (partition b0 e0))  #() partition into two parts
            (if {b0 != iterator.to.pivot}               #() if before the pivot is non.empty
                [
                    (quicksort' b0 iterator.to.pivot) #() recursive call
                ])
            (if {e0 != {iterator.to.pivot + 1}}         #() if after the pivot is non.empty
                [
                    (quicksort' {iterator.to.pivot + 1} e0       ) #() recursive call
                ])
            ()
        ])]))

    #() Finally, we can put all this together to return an anonymous
    #() function which takes an array by reference and sorts it in place

    (lambda [(& arr)]                       #() capture an array by reference
    [


        #() The lines above define the necessary functions. The next line
        #() calls 'fix' to actually do the sorting. 'fix' is used to
        #() enable recursion in this language

        (fix
            (typeof ())     #() 'fix' needs to know the return type, in this case simply 'nil_t'
            quicksort
            (std::begin arr)
            (std::end   arr)
            )
    ])

    )--"_cambda();

        int a[] = {9,8,7,6,5,3,2};
        quicksort_cambda(a);

        return cambda_utils::equal_array(a, (int[]){2,3,5,6,7,8,9});
}

static_assert(test_quicksort() ,"");
#endif
