#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

using hambda::operator"" _charpack;

using namespace hambda;

int main() {
    constexpr
    auto ex0 = "(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack; // closers don't seem to match

    std::cout << char_pack_to_int( "345789"_charpack ) << '\n';
    static_assert(345789 ==char_pack_to_int( "345789"_charpack ) ,"");

    using r = utils:: reverse_pack<char, utils:: char_pack<'a','b','c'>> :: type;
    std::cout << r{}.c_str0() << '\n';


    std::cout << ex0.c_str0() << "\n\n";

    constexpr auto ast = parse_ast(ex0);
    std::cout << toString( ast   ,0) << '\n';
    PP(simplify(ast));

    TEST_ME ( "char_pack_to_int"
            , 345789.0
            ) ^ []()
            { return char_pack_to_int( "345789"_charpack ); };

    TEST_ME ( "recursive addition"
            , 111
            ) ^ []()
            { return simplify(parse_ast("(+ (+ (+ 90 9) 0) (+ 5 7))"_charpack)); };
}
