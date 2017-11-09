#include "../module-bits.and.pieces/utils.hh"
namespace hambda {
    bool constexpr is_whitespace    (char c) { return c==' ' || c=='\t' || c=='\n'; }
    bool constexpr is_opener        (char c) { return c=='(' || c=='[' || c=='{'; }
    bool constexpr is_closer        (char c) { return c==')' || c==']' || c=='}'; }
    bool constexpr is_grouper       (char c) { return is_opener(c) || is_closer(c); }

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
    struct types_t {
        template<typename Prepend>
        using prepend = types_t<Prepend, T...>;
    };

    template<char c, typename T>
    struct grouped_t {
        static_assert(is_opener(c) ,"");
        constexpr static char my_closer =
            c=='(' ? ')' :
            c=='{' ? '}' :
            c=='[' ? ']' : '?';
        static_assert(my_closer != '?' ,"");
    };


    template<typename Parsed, size_t Unprocessed>
    struct parse_result_t{
        static constexpr Parsed     parsed() {return{};}
        static constexpr size_t     remain() {return{};}
    };

    /* Starting from position 'o', parse as much as possible (up to the
     * end, or to a closing grouper) and then return a pair representing
     * what was parsed and what was left over
     */

    template<typename E, size_t F, size_t S, char C>
    struct parser;

    template<typename E, size_t F, size_t S, char C>
    struct parser {
        static_assert(C != '\0' ,"");
        static_assert(C != ')' ,"");

        static E e;

        constexpr static auto
        tk = find_next_token(e, S);

        static_assert(tk.second >= tk.first, "");

        using future = parser<E, tk.first, tk.second, e.at(tk.first) >;

        static_assert(future::remain >= S ,"");
        constexpr static size_t remain = future::remain;

        using future_parsed = decltype(future::parsed());

        constexpr static auto parsed() {
            return typename future_parsed:: template prepend<c_char<C>> {};
        };
    };

    template<typename E, size_t F, size_t S>
    struct parser<E,F,S, '\0'> {
        constexpr static auto parsed() { return types_t< >{}; };
        constexpr static size_t remain = S;
    };

    template<typename E, size_t F, size_t S>
    struct parser<E,F,S, ')'> {
        constexpr static auto parsed() { return types_t< >{}; };
        constexpr static size_t remain = S;
    };

    template<typename E, size_t F, size_t S>
    struct parser<E,F,S, '('> {

        static E e;

        constexpr static auto
        tk = find_next_token(e, S);

        using future = parser<E, tk.first, tk.second, e.at(tk.first) >;

        static_assert(future::remain > S ,"");
        constexpr static size_t remain = future::remain;

        using future_parsed = decltype(future::parsed());

        constexpr static auto parsed() {
            return types_t<grouped_t<'(', future_parsed >>{};
        };
    };

    template<typename E>
    auto constexpr
    ast(E e)
    {
        constexpr auto tk = find_next_token(e, 0);
        constexpr char selecting_char = e.at(tk.first); // one char, which may be '\0', is enough to decide what to do next
        return parser<E, tk.first, tk.second, selecting_char>{};
    }
}
