#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

using hambda::operator"" _ex;

using namespace hambda;

int main() {
    //auto ex0 = "   (323  (5lkj  lkj) (7jlkj 1 ()) ((2)) 23 )    "_ex;
    constexpr
    auto ex0 = "(+ (+ (+ 9 9) 0) (+ 5 7))"_ex; // closers don't seem to match
    //auto ex0 = "(3)"_ex; // closers don't seem to match
    (void)ex0;

    using utils::operator<<;

    //PP(toString( parse_ast(ex0) ));
    //utils::print_type( decltype(parse_ast(ex0))::me{} );
    //utils::print_type( decltype(parse_ast(ex0))::rest{} );

    std::cout << ex0.c_str0() << "\n\n";

    std::cout << toString( parse_ast(ex0)   ,0) << '\n';
    constexpr auto ast = parse_ast(ex0);
    (void)ast;
    auto x = simplify(ast);
    PP(x);
}
