#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

using hambda::operator"" _charpack;

namespace utils { // 'utils' namespace, in order to use ADL

    template<char ...c>
    std::ostream & operator<<(std::ostream &o, utils::char_pack<c...> str)
    {
        o << str.c_str0();
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

struct user_supplied_library {
    constexpr user_supplied_library(){} // a default constructor, just because clang requires them for constexpr objects

    auto constexpr
    apply_after_simplification  ( decltype( "%"_charpack )
                        , int i
                        , int j
                        )
    -> int
    { return i%j; }

};

int main() {
    std::cout << toString( parse_ast( "(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)   ,0) << '\n';

    TEST_ME ( "char_pack_to_int"
            , 345789.0
            ) ^ []()
            { return char_pack_to_int( "345789"_charpack ); };

    TEST_ME ( "recursive addition"
            , 111
            ) ^ []()
            { return simplify(parse_ast("(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)); };

    constexpr int i= simplify(parse_ast("(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack));
    static_assert(i==111 ,"");

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

    static_assert(3 == simplify(parse_ast("(length '''''''')"_charpack)) ,"");

    TEST_ME ( "string literals"
            , std::string(" ' ' ' ")  // yes ' '' '' '' ' is equivalent to " ' ' ' "
            ) ^ []()
            { return simplify(parse_ast("' '' '' '' '"_charpack)); };

    TEST_ME ( "extra_lib for '*'"
            , 60
            ) ^ []()
            { return simplify(parse_ast("(* 6 (+ 7 three))"_charpack), combined_lib_v); };

    std::cout << simplify(parse_ast( "(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)) << '\n';
    std::cout << simplify(parse_ast( "'''e''''o'"_charpack)) << '\n';
    std::cout << simplify(parse_ast( "three"_charpack), extra_lib_with_multiplication{}) << '\n';
    std::cout << simplify(parse_ast( "four"_charpack), extra_lib_with_multiplication{}) << '\n';

    TEST_ME ( "a 'constexpr' test."
            , 60
            ) ^ []()
            {
                constexpr auto result = simplify(parse_ast("(* 6 (+ 7 three))"_charpack), combined_lib_v);
                return result;
            };

    TEST_ME ( "user-supplied library"
            , 1
            ) ^ []()
            {
                constexpr auto result = simplify(parse_ast("(% 100 3)"_charpack)
                        , combine_libraries(starter_lib_v, user_supplied_library{})
                        );
                return result;
            };
}
