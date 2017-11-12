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

    std::cout << simplify(parse_ast( "(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)) << '\n';
    std::cout << simplify(parse_ast( "'''e''''o'"_charpack)) << '\n';
}
