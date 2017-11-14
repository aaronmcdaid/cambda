#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

#include<algorithm>

using hambda::operator"" _charpack;

namespace utils { // 'utils' namespace, in order to use ADL

    template<char ...c>
    std::ostream & operator<<(std::ostream &o, utils::char_pack<c...> str)
    {
        o << str.c_str0();
        return o;
    }

    template<typename ...T>
    std::ostream & operator<<(std::ostream &o, hambda::grouped_t<'[', T...> quoted)
    {
        o << toString(quoted);
        return o;
    }

    template<typename T>
    std::ostream & operator<<(std::ostream &o, just_a_type<T> a_type)
    {
        o << type_as_string(a_type);
        return o;
    }

    template<char ...c>
    static constexpr auto
    operator==(std::string const & l, utils::char_pack<c...> r)
    { return l == r.c_str0(); }

    template<char ...c>
    static constexpr auto
    operator==(utils::char_pack<c...> l, std::string const & r)
    { return r == l.c_str0(); }
}

using namespace hambda;

int main() {
    // toString gives a nicely-indented breakdown of the parsed object
    //std::cout << toString( parse_ast( "(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)   ,0) << '\n';

    TEST_ME ( "char_pack_to_int"
            , 345789.0
            ) ^ []()
            { return char_pack_to_int( "345789"_charpack ); };

    TEST_ME ( "recursive addition"
            , 111
            ) ^ []()
            { return simplify(parse_ast("(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)); };

    TEST_ME ( "recursive addition"
            , 87
            ) ^ []()
            { return simplify(parse_ast("(- (+ (+ 90 9) 0) (+ 5 7))"_charpack)); };

    TEST_ME ( "heavily nested 'id', especially in function position"
            , 87
            ) ^ []()
            { return simplify(parse_ast("((((((id id) id) id) id) -) ((id +) ((((id id) (id id)) +) (id 90) 9) 0) (+ 5 7))"_charpack)); };

    TEST_ME ( "string literals"
            , std::string("helloworld")
            ) ^ []()
            { return simplify(parse_ast("'helloworld'"_charpack)); };

    TEST_ME ( "string literals"
            , 3  // yes '''''''' is equivalent to "'''"
            ) ^ []()
            { return simplify(parse_ast("(length '''''''')"_charpack)); };

    TEST_ME ( "string literals"
            , std::string(" ' ' ' ")  // yes ' '' '' '' ' is equivalent to " ' ' ' "
            ) ^ []()
            { return simplify(parse_ast("' '' '' '' '"_charpack)); };

    TEST_ME ( "extra_lib for '*'"
            , 60
            ) ^ []()
            { return simplify(parse_ast("(* 6 (+ 7 3))"_charpack)); };


    TEST_ME ( "a 'constexpr' test."
            , 60
            ) ^ []()
            {
                constexpr auto result = simplify(parse_ast("(* 6 (+ 7 3))"_charpack));
                return result;
            };

    TEST_ME ( "user-supplied library with a reference"
            , 35
            ) ^ []()
            {
                int foo = 1337;
                auto && result = "(assign foo 35)"_cambda[ "foo"_binding = foo]();
                return foo * (&result == &foo);
            };


    TEST_ME ( "wrapped, to get integral_constant out again"
            , 60
            ) ^ []()
            { return "(* 6 (+ 7 3))"_cambda (); };


    TEST_ME ( "very simple \"...\"_cambda, with one binding"
            , 45
            ) ^ []()
            {
                int forty = 40;
                return "(+ 5 forty)"_cambda ["forty"_binding = forty] ();
            };

    TEST_ME ( "multiple bindings [] []"
            , 90
            ) ^ []()
            {
                int forty = 40;
                int fifty = 50;
                (void)fifty;
                return "(+ forty fifty)"_cambda ["forty"_binding = forty]["fifty"_binding = fifty]
                ();
            };

    TEST_ME ( "multiple bindings, using comma operator"
            , 90
            ) ^ []()
            {
                return "(+ forty fifty)"_cambda [ "forty"_binding = 40, "fifty"_binding = 50 ]
                ();
            };

    TEST_ME ( "binding to a C++-lambda"
            , 17
            ) ^ []()
            {
                return "(max 1 2 3 17 8 9 0)"_cambda [ "max"_binding = [](auto ... x){ return std::max(std::initializer_list<int>{x...});} ] ();
            };

    TEST_ME ( "cambda-lambda"
            , 50
            ) ^ []()
            {
                return "(lambda [x y z] [(+ (* x y) z)])"_cambda  () (15, 3, 5);
            };

    TEST_ME ( "[1 2 3]"
            //, 225
            , utils::type< hambda::grouped_t<'[', types_t<decltype("1"_charpack), decltype("2"_charpack), decltype("3"_charpack)>> >
            ) ^ []()
            {
                return utils:: as_type( "[1 2 3]"_cambda () );
            };

    TEST_ME ( "[]"
            //, 225
            , utils::type< hambda::grouped_t<'[', types_t<>> >
            ) ^ []()
            {
                return utils:: as_type( "[]"_cambda () );
            };

    TEST_ME ( "lambda with assign"
            , 500
            ) ^ []()
            {
                int foo = 100;
                return "(lambda [w] [(assign w (* 5 w))])"_cambda  () (foo);
            };

    TEST_ME ( "lambda constexpr"
            , 500
            ) ^ []()
            {
                constexpr int foo = 100;
                constexpr auto result = "(lambda [w] [(* 5 w)])"_cambda  () (foo);
                return result;
            };

    TEST_ME ( "lambda [w] [(+ w w)]"
            , 200
            ) ^ []()
            {
                constexpr auto result = "(lambda [w] [(+ w w)])"_cambda  () (100);
                return result;
            };

    TEST_ME ( "lambda inside"
            , 256
            ) ^ []()
            {
                constexpr auto result = "( (lambda [w] [(* w w)]) 16 )"_cambda  ();
                return result;
            };

    TEST_ME ( "{10 * {2 + 2}}"
            , 40
            ) ^ []()
            {
                constexpr auto result = "{10 * {2 + 2}}"_cambda  ();
                return result;
            };

    TEST_ME ( "lambda with {} and &"
            , 256
            ) ^ []()
            {
                constexpr auto result = "{16 & (lambda [w] [(* w w)]) }"_cambda  ();
                return result;
            };

    TEST_ME ( "no-arg lambda"
            , 1234
            ) ^ []()
            {
                constexpr auto result = "((lambda [] [1234]))"_cambda  ();
                return result;
            };

    struct test0_25
    {
        constexpr static double
        foo() {
            double r = 0.5;
            auto result = "{(id r) assign ((lambda [] [{r * r}]))}"_cambda[ "r"_binding = r] ();
            return result;
        }
    };

    TEST_ME ( "no-arg lambda with constexpr and assign and stuff"
            , 0.25
            ) ^ []()
            {
                constexpr auto result = test0_25::foo();
                return result;
            };


    TEST_ME ( "type_as_string"
            , std::string("double")
            ) ^ []()
            {
                double v;
                auto result = "(type_as_string v)"_cambda[ "v"_binding = v] ();
                return result;
            };

}
