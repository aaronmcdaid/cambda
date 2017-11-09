#include "../module-bits.and.pieces/utils.hh"
namespace hambda {
    bool constexpr is_whitespace    (char c) { return c==' ' || c=='\t' || c=='\n'; }
    bool constexpr is_grouper       (char c) { return c=='(' || c==')' || c=='[' || c == ']' || c=='{' || c=='}'; }

    template< typename C >
    auto constexpr
    find_next_token(C, size_t o) {
        // first, skip whitespace
        while(C::at(o) != '\0' && is_whitespace(C::at(o)))
            ++o;
        size_t start = o;

        // then, check if we're at the end
        if(C::at(o) == '\0')
            return std::make_pair(start,start); // empty token, i.e. we're finished

        // if it's a grouper, (){}[], then we have a single-character token
        if(is_grouper(C::at(o)))
            return std::make_pair(start, start+1);

        // finally, we just have a string of non-special characters.
        // We must read them all in
        while (C::at(o) != '\0' && !is_whitespace(C::at(o)) && !is_grouper(C::at(o)))
            ++o;
        return std:: make_pair(start, o);

        throw 1324; // fail to parse. Should give an error message somehow.
    }


    template<typename T, T ... chars>
    auto operator"" _ex () {
        utils:: char_pack<chars...> chrpck;
        return chrpck;
    }
}
