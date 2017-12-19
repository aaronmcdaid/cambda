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
                , typename Ti
                , typename Tj >
        auto constexpr
        apply_after_simplification
            ( LibToForward
            , decltype( "+"_charpack )
            , Ti && i
            , Tj && j) const
        ->decltype(std::forward<Ti>(i) + std::forward<Tj>(j) )
        {   return std::forward<Ti>(i) + std::forward<Tj>(j); }
    };

    constexpr auto b = "(+ 7 8)"_cambda_empty_library["+"_binding = plus]();
    static_assert(b == 15 ,"");
    constexpr auto c = "(+ 7 8)"_cambda_empty_library[a_lib_with_plus{}]();
    static_assert(c == 15 ,"");
}

constexpr auto a = "15"_cambda();            // a is 15
constexpr auto b = "(+ 8 7)"_cambda();       // Function call. This is addition. b is 15
constexpr auto c = "(* 8 7)"_cambda();       // Multiplication
constexpr auto d = "{8 * 7}"_cambda();       // If there are two args, use {} instead of () for infix notation
constexpr auto e = "{ {8 * 7} + {6 * 3} }"_cambda();  // Nested application

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

static_assert(a == 15   ,"");
static_assert(b == 15   ,"");
static_assert(c == 56   ,"");
static_assert(d == 56   ,"");
static_assert(e == 74   ,"");

constexpr auto a_integralconstant = "15c"_cambda();            // a is 15
static_assert(a_integralconstant.value == 15   ,"");

constexpr auto four_squared = "{x * x}"_cambda ["x"_binding = 4] ();
static_assert(four_squared == 16   ,"");

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
        [   "v"_binding = v
        ,   "size"_binding =
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

#if 1
    struct factorial {
        constexpr auto static
        bar() {
        auto answer =
    R"--(
        (lambda [N] [
            (fix    (typeof 0)
                    (lambda
                        [(& rec) n]
                        [
                            (if {n < 1}
                                [ 1 ]
                                [ {n * (rec {n - 1})} ]
                                )
                        ])
                    (ref2val N)
                    )
                    ])
    )--"_cambda ()(7); // compute the factorial of 7.
            return answer;
        }
    };
    constexpr
    auto factorial7 = factorial::bar();
    static_assert(5040 == factorial7 ,"");
    std::cout << "Fix: " << factorial7 << '\n';
#endif

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
    "(if bt  [(assign x 3)] [(assign x 5)])"_cambda       ["bt"_binding = bt, "x"_binding &= x]();
    "(if bf  [(assign x 300)] [(assign x 500)])"_cambda   ["bf"_binding = bf, "x"_binding &= y]();
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
                                            ([] [tmp] (ref2val x))
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
                [   "b"_binding = std::begin(a)
                ,   "e"_binding = std::end  (a)
                ]();
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
    (lambda [(& arr)]
    [
        ([] [swap]      (lambda [(& x) (& y)] #() define a 'swap' function. Captures by reference
            [
                    ([] [tmp] (ref2val x))  #() 'ref2val' in order that 'tmp' is a copy of 'x', not a reference to it
                    {x = y}
                    {y = tmp}
            ]))
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
        ([] [quicksort]     (lambda [(& rec) b0 e0]
            [(if {b0 != e0} [               #() check if the range to be sorted is non-empty
                ([] [iterator.to.pivot] (partition b0 e0))  #() partition into two parts
                (if {b0 != iterator.to.pivot}               #() if before the pivot is non.empty
                    [
                        (rec (ref2val b0) (ref2val iterator.to.pivot)) #() recursive call
                    ])
                (if {e0 != {iterator.to.pivot + 1}}         #() if after the pivot is non.empty
                    [
                        (rec {iterator.to.pivot + 1} (ref2val e0       )) #() recursive call
                    ])
                ()
            ])]))

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
