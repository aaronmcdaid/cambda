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


    // a few toString overloads just to print the output after parsing to help debugging

    template<char ... c>
    std::string
    toString( utils::char_pack<c...> s, int indent = -1)
    { return std::string(indent, ' ') + s.c_str0() + "\n"; }

    std::string
    toString( types_t<> , int indent = -1)
    {   (void) indent;
        return "";
    }

    template<typename S>
    std::string
    toString( types_t<S> , int indent = -1)
    {   (void)indent;
        return toString( S{}, indent);
    }

    template<typename R, typename S, typename ...T>
    std::string
    toString( types_t<R, S, T...> , int indent = -1)
    {   (void)indent;
        return toString( R{}, indent) + toString( types_t<S, T...>{}, indent);
    }

    template<char c, typename T>
    std::string
    toString(grouped_t<c,T> grp, int indent = -1)
    {   (void) indent;
        if(indent != -1)
        {
            return      std::string(indent, ' ')
                    +   std::string{c}
                    +   "\n"

                    +   toString(T{}, indent+2)

                    +   std::string(indent, ' ')
                    +   std::string{grp.my_closer}
                    +   "\n"
                    ;
        }
        else
        {
            throw 3;
            return      std::string{c, c ,c , '\0'}
                    +   toString(T{}, indent)
                    +   std::string{grp.my_closer, grp.my_closer , grp.my_closer, '\0'}
                    ;
        }
    }

    /* Starting from position 'o', parse as much as possible (up to the
     * end, or to a closing grouper) and then return a pair representing
     * what was parsed and what was left over
     */

    template<typename E, size_t O>
    struct parse_flat_list_of_terms
    {
        constexpr static E e {};

        constexpr static auto tk = find_next_token(e, O);

        constexpr static bool at_the_end = E::at(tk.first) == '\0';

        static_assert(at_the_end || (tk.first<tk.second) ,"");

        template<size_t ... I>
        static auto constexpr
        full_token_helper(std::index_sequence<I...>)
        -> utils::char_pack< E::at(tk.first+I) ... >
        { return {}; }

        using full_token_here = decltype(full_token_helper(std::make_index_sequence<tk.second-tk.first>{}));

        using towards_the_next = parse_flat_list_of_terms<E, tk.second>;

        constexpr static auto
        all_the_terms_helper(std::true_type)
        -> types_t<>
        { return {}; }

        template<size_t From = tk.second>
        constexpr static auto
        all_the_terms_helper(std::false_type)
        -> typename parse_flat_list_of_terms<E,From>::all_the_terms :: template prepend<
                //c_char<E::at(tk.first)>
                full_token_here
                >
        {
            return {};
        }

        using all_the_terms = decltype(all_the_terms_helper(std::integral_constant<bool, at_the_end>{}));
    };


    // parse_many_things. Returns two 'types_t', everything up to the next grouper, and the 'rest'

    // most symbols are simply prepended to the rest of the list they're currently in:
    template<typename First, typename ... T>
    struct parse_many_things
    {
        using future =  parse_many_things<T...>;

        using me    = typename future :: me :: template prepend<First>;
        using rest  = typename future :: rest;
    };

    // but ')' closes the list:
    template<typename ... T>
    struct parse_many_things<utils::char_pack<')'>, T...>
    {
        using future =  parse_many_things<T...>;

        using me    = types_t<>;
        using rest  = types_t<T...>;
    };

    // but '(' closes the list:
    template<typename ... T>
    struct parse_many_things<utils::char_pack<'('>, T...>
    {
        using future =  parse_many_things<T...>;

        using me    = types_t<grouped_t<'(', types_t< typename future:: me >>>;
        using rest  = typename future:: rest;
    };

    template<char ... c>
    struct parse_many_things<utils::char_pack<c...>>
    {
        using me    = types_t< utils::char_pack<c...> >;
        using rest  = types_t<>;
    };

    template<typename ...T>
    constexpr static auto
    parser(types_t<T...>)
    {
        return parse_many_things<T...> {};
    }

    template<typename E>
    auto constexpr
    ast(E )
    {
        using all_the_terms_t = typename parse_flat_list_of_terms<E, 0>::all_the_terms;
        return parser( all_the_terms_t{} );
        //return all_the_terms_t {};
    }
}
