#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

using hambda::operator"" _ex;

using namespace hambda;

int main() {
    constexpr
    auto ex0 = "(+ (+ (+ 9 9) 0) (+ 5 7))"_ex; // closers don't seem to match

    std::cout << ex0.c_str0() << "\n\n";

    constexpr auto ast = parse_ast(ex0);
    std::cout << toString( ast   ,0) << '\n';
    PP(simplify(ast));
}
