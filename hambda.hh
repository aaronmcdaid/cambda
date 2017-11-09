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

    template<char c>    using c_char    = std::integral_constant<char, c>;
    template<int c>     using c_int     = std::integral_constant<int, c>;
    template<size_t c>  using c_sizet   = std::integral_constant<size_t, c>;

    template<typename ... T>
    struct types_t {};

    template<typename Parsed, size_t Unprocessed>
    struct parse_result_t{
        static constexpr Parsed     parsed() {return{};}
        static constexpr size_t     remain() {return{};}
    };

    /* Starting from position 'o', parse as much as possible (up to the
     * end, or to a closing grouper) and then return a pair representing
     * what was parsed and what was left over
     */
    template<typename E, size_t F, size_t S>
    auto constexpr
    ast_from_offset ( E e
                    , std::integral_constant<size_t, F> f
                    , std::integral_constant<size_t, S> s
                    , std::integral_constant<char, '\0'> current_char
                    ) {
        static_assert(e.at(f) == current_char ,"");
        (void)e; (void)f; (void)s; (void) current_char;
        return parse_result_t< types_t<>, 0 >{};
    }
    template<typename E, size_t F, size_t S>
    auto constexpr
    ast_from_offset (E e
                    , std::integral_constant<size_t, F> f
                    , std::integral_constant<size_t, S> s
                    , std::integral_constant<char, ']'> current_char
                    ) {
        static_assert(e.at(f) == current_char ,"");
        (void)e; (void)f; (void)s; (void) current_char;
        return parse_result_t< types_t<>, 0 >{};
    }
    template<typename E, size_t F, size_t S, char C>
    auto constexpr
    ast_from_offset (E e
                    , std::integral_constant<size_t, F> f
                    , std::integral_constant<size_t, S> s
                    , std::integral_constant<char, C> current_char
                    ) {
        static_assert(e.at(f) == current_char ,"");
        (void)e; (void)f; (void)s; (void) current_char;
        return parse_result_t< types_t<>, 0 >{};
    }

    template<typename E, size_t O>
    auto constexpr
    ast_from_offset(E e, std::integral_constant<size_t, O> ) {
        constexpr auto tk = find_next_token(e, O);
        constexpr char selecting_char = e.at(tk.first); // one char, which may be '\0', is enough to decide what to do next
        return ast_from_offset(e, c_sizet<tk.first>{}, c_sizet<tk.second>{}, c_char< selecting_char >{});
    }

    template<typename E>
    auto constexpr
    ast(E e)
    { return ast_from_offset(e, std::integral_constant<size_t, 0>{}); }
}
