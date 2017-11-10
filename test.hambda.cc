#include "hambda.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include "../module-bits.and.pieces/PP.hh"

using hambda::operator"" _ex;

using namespace hambda;

int main() {
    //auto ex0 = "   (323  (5lkj  lkj) (7jlkj 1 ()) ((2)) 23 )    "_ex;
    auto ex0 = "(+ 3 5)"_ex; // closers don't seem to match
    //auto ex0 = "(3)"_ex; // closers don't seem to match
    (void)ex0;

    using utils::operator<<;

    //PP(toString( ast(ex0) ));
    //utils::print_type( decltype(ast(ex0))::me{} );
    //utils::print_type( decltype(ast(ex0))::rest{} );

    std::cout << ex0.c_str0() << "\n\n";

    std::cout << toString( decltype(ast(ex0))::me{}   ,0) << '\n';
    std::cout << "\n...\n\n";
    std::cout << toString( decltype(ast(ex0))::rest{} ,0) << '\n';
}
