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
            , Tj && j)
        ->decltype(std::forward<Ti>(i) + std::forward<Tj>(j) )
        {   return std::forward<Ti>(i) + std::forward<Tj>(j); }
    };

    constexpr auto a = "15"_cambda_empty_library();
    static_assert(a == 15 ,"");
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

constexpr auto cambda_lambda = "(lambda [x] [{x * x}])"_cambda();
constexpr auto squared_cambda = cambda_lambda(15);
static_assert(squared_cambda == 225 ,"");


static_assert(a == 15   ,"");
static_assert(b == 15   ,"");
static_assert(c == 56   ,"");
static_assert(d == 56   ,"");
static_assert(e == 74   ,"");

constexpr auto a_integralconstant = "15c"_cambda();            // a is 15
static_assert(a_integralconstant.value == 15   ,"");

constexpr auto four_squared = "{x * x}"_cambda ["x"_binding = 4] ();
static_assert(four_squared == 16   ,"");

static_assert(42    == "(let [(* 21 2)])"_cambda ()   ,"");
static_assert(56088 == R"--(
                        (let [
                            left 123
                            right 456
                            (* left right) ])
                            )--"_cambda () ,"");

static_assert(std::is_same<std::integral_constant<int,42>, decltype( "42c"_cambda() )>{}  ,"");
static_assert(std::is_same<int, decltype( "42"_cambda() )>{}  ,"");
static_assert(std::is_same<double, decltype( "3.4"_cambda() )>{}  ,"");
static_assert(42 ==                       "42"_cambda()       ,"");
static_assert(3.4 ==                        "3.4"_cambda()       ,"");
static_assert(3.41932 ==                        "3.41932"_cambda()       ,"");

static_assert("(if truec  [3.14] ['hi'])"_cambda() == 3.14 ,"");
static_assert("(if falsec [3.14] ['hi'c])"_cambda() == "hi"_charpack ,"");

std::vector<int> v{2,3,4};
auto size_of_v = "(size v)"_cambda
        [   "v"_binding = v
        ,   "size"_binding =
                        [](auto && x){return x.size();}
        ]   //  [] attaches the bindings
        (); //  () executes

std::initializer_list<int> il{2,3,4};

constexpr bool
equal_char_array    (   const char *l
                    ,   const char *r)
{
    for(int i=0; ; ++i)
    {
        if(l[i] == '\0' && r[i] == '\0')
            return true;
        if(l[i] == '\0' || r[i] == '\0')
            return false;
        if(l[i] != r[i])
            return false;
    }
}

static_assert(equal_char_array("hi", "hi") ,"");

static_assert("'it''s mine'"_cambda_empty_library()[0] == 'i' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[1] == 't' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[2] == '\'' ,"");
static_assert("'it''s mine'"_cambda_empty_library()[3] == 's' ,"");
static_assert(equal_char_array("'it''s mine'"_cambda_empty_library(), "it's mine") ,"");
static_assert(!equal_char_array("'it''s mine'"_cambda_empty_library(), "it's mIne") ,"");
static_assert(!equal_char_array("'it''s mine'"_cambda_empty_library(), "it's min") ,"");

int main() {
    int x=0;
    "(assign x 1234)"_cambda ["x"_binding = x] ();
    // x is now equal to 1234

    std:: cout << "x = " << x << " should be 1234" << '\n';

    auto lambda_from_cpp = [](auto x, auto y) { return x+y; }; // cannot be made constexpr
    constexpr auto lambda_from_cambda = "(lambda [x y] [{x + y}])"_cambda();
    std::cout << lambda_from_cpp(5,6) << " == " << lambda_from_cambda(5,6) << '\n';
    //static_assert(11 == lambda_from_cpp(5,6)    ,"");
    static_assert  (11 == lambda_from_cambda(5,6) ,"");

    auto product =
    "(mult 3c 4c)"_cambda
        ["mult"_binding = [](auto x, auto y){return std::integral_constant<int, x.value*y.value>{};}]
        ();
    static_assert(product.value == 12 ,"");
}

constexpr auto static
test_if_with_side_effects()
{
    int x = -1;
    int y = -1;
    "(if truec [(assign x 3)] [(assign x 5)])"_cambda["x"_binding = x]();
    "(if falsec[(assign x 300)] [(assign x 500)])"_cambda["x"_binding = y]();
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
    "(if bt  [(assign x 3)] [(assign x 5)])"_cambda       ["bt"_binding = bt, "x"_binding = x]();
    "(if bf  [(assign x 300)] [(assign x 500)])"_cambda   ["bf"_binding = bf, "x"_binding = y]();
    return x + y;
}
static_assert(test_ifNonConstant_with_side_effects() == 503 ,"");


constexpr auto static
test_range_based_for()
{
        int test_data[] {5,6,7};
        "(range_based_for test_data (lambda [r] [(assign r {r * r})]))"_cambda
            ["test_data"_binding = test_data]
            ();
        return test_data[0]+test_data[1]+test_data[2];
}
static_assert( test_range_based_for() == 5*5 + 6*6 + 7*7 ,"");

constexpr auto static
test_while()
{
    //int x = 10;
    int y = 0;
    "(while [(< y 100)] [{y = {y + 3}}])"_cambda["y"_binding = y]();
    return y;
}
static_assert(test_while() == 102 ,"");
