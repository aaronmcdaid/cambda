#include "../module-bits.and.pieces/utils.hh"
namespace hambda {
    bool constexpr is_whitespace    (char c) { return c==' ' || c=='\t' || c=='\n'; }
    bool constexpr is_opener        (char c) { return c=='(' || c=='[' || c=='{'; }
    bool constexpr is_closer        (char c) { return c==')' || c==']' || c=='}'; }
    bool constexpr is_grouper       (char c) { return is_opener(c) || is_closer(c); }
    bool constexpr is_digit_constexpr (char c) { return c >= '0' && c <= '9'; }
    char constexpr opener_to_closer (char c) { switch(c) {
                                                    break; case '(': return ')';
                                                    break; case '[': return ']';
                                                    break; case '{': return '}';
                                                }
                                                return -1; // should never get here
    }

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

        // literal strings are special. We quote them with single quotes instead
        // of double quotes, simply to make them easier to embed within C++.
        // The contents of a literal are entirely read in raw, except that two
        // consecutive single-quotes are interpreted as one such quote.
        // This allows any string to be specified
        if(C::at(o) == '\'') {
            ++o;
            while(true)
            {
                if(C::at(o) == '\'' && C::at(o+1) == '\'')
                { o+=2; continue; }

                if(C::at(o) == '\'')
                { o+=1; break; }

                ++o;
            }
            return std::make_pair(start, o);
        }

        // finally, we just have a string of non-special characters.
        // We must read them all in
        while (C::at(o) != '\0' && !is_whitespace(C::at(o)) && !is_grouper(C::at(o)))
            ++o;
        return std:: make_pair(start, o);
    }


    template<typename T, T ... chars>
    constexpr auto
    operator"" _charpack () {
        utils:: char_pack<chars...> chrpck{};
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

    template<typename T>
    auto
    get_last_type( types_t<T> )
    -> T
    { return {}; }

    template<typename S, typename T, typename ... U>
    auto
    get_last_type( types_t<S, T, U...> )
    ->decltype( get_last_type(std::declval< types_t<T,U...> >()) )
    { return {}; }


    template<typename T>
    auto
    drop_last_type( types_t<T> )
    -> types_t<>
    { return {}; }

    template<typename S, typename T, typename ... U>
    auto
    drop_last_type( types_t<S, T, U...> )
    ->
        // recursively call, i.e. ignoring S:
        typename decltype( drop_last_type(std::declval< types_t<T,U...> >()) )
                // , but then prepend S to the result:
              ::template prepend<S>
    { return{}; }


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

    constexpr int indent_each_time = 4;

    template<char ... c>
    std::string
    toString( utils::char_pack<c...> s, int indent = 0)
    {
        (void)indent;
        return s.c_str0();
    }

    std::string
    toString( types_t<> , int = 0)
    {
        return "";
    }

    template<typename S>
    std::string
    toString( types_t<S> , int indent = 0)
    {
        return toString( S{}, indent);
    }

    template<typename R, typename S, typename ...T>
    std::string
    toString( types_t<R, S, T...> , int indent = 0)
    {
        return toString( R{}, indent)
            + "\n" + std::string(indent, ' ')
            + toString( types_t<S, T...>{}, indent);
    }

    template<char c, typename T>
    std::string
    toString(grouped_t<c,T> grp, int indent = 0)
    {
        return
                    std::string{c}
                +   std::string(indent_each_time-1, ' ')
                +   toString(T{}, indent+indent_each_time)
                +   std::string{grp.my_closer} // the closer appears on the same line as the last item
                ;
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


    /* parse_many_things.
     * =================
     * Returns two 'types_t', everything up to the next grouper, and the 'rest'
     *
     * Many cases to be handled. There is a template specialization for each
     */
    template<typename, typename = void> /* second type is for 'void_t' */
    struct parse_many_things;

    // Simplest case is an empty list:
    template<>
    struct parse_many_things<hambda::types_t<>, void>
    {
        using me = types_t<>;
        using rest = types_t<>;
    };

    // Any symbol that is not a grouper is simply prepended to the rest of the list they're currently in:
    template<typename First, typename ... T>
    struct parse_many_things<types_t<
            First, T...
        >
        , std::enable_if_t< !is_grouper( First::at(0) )>
        >
    {
        static_assert(!is_grouper(First::at(0)) ,"");
        using future =  parse_many_things<types_t<T...>>;

        using me    = typename future :: me :: template prepend<First>;
        using rest  = typename future :: rest;
    };

    // but ')' closes the list:
    template<char c, typename ... T>
    struct parse_many_things<types_t<
            utils::char_pack<c>, T...
        >
        , std::enable_if_t<is_closer(c)>
        >
    {
        using me    = types_t< utils::char_pack<c> >;
        using rest  = types_t<T...>;
    };

    // while '('/'{'/'[' open the list. This is more complex:
    template<char o, typename ... T>
    struct parse_many_things<types_t<
            utils::char_pack<o>, T...
        >
        , std::enable_if_t<is_opener(o)>
        >
    {
        using next_and_future =  parse_many_things<types_t<T...>>;
        // next_and_future::me is the rest of my group
        // next_and_future::rest is the distant future, which must be parsed here too

        using mygroup_with_the_closer = typename next_and_future::me;

        // Check that the openers and closers "match". See the static_assert
        using my_closer = decltype(get_last_type(mygroup_with_the_closer{}));

        static_assert( my_closer::size() == 1 ,""); // it must be one character ...
        static_assert( is_closer(my_closer::at(0)) ,""); // .. one of    })]
        static_assert( my_closer::at(0) == opener_to_closer(o) ,"opener and closer should match, i.e. (...) or {...} or (...}");

        using mygroup_without_the_closer = decltype(drop_last_type(mygroup_with_the_closer{}));

        using mygroup    = grouped_t<o, mygroup_without_the_closer >;

        using future = parse_many_things<typename next_and_future:: rest>;

        using me    = typename future :: me :: template prepend< mygroup >;

        using rest  = typename future :: rest;
    };

    template<typename ...T>
    constexpr static auto
    parser(types_t<T...>)
    {
        return parse_many_things<types_t<T...>> {};
    }

    template<typename T>
    struct should_be_just_one_term;

    template<typename T>
    struct should_be_just_one_term< types_t<T> >
    { using single_type = T; };

    template<typename E>
    auto constexpr
    parse_ast(E )
    {
        using all_the_terms_t = typename parse_flat_list_of_terms<E, 0>::all_the_terms;
        auto parsed =  parser( all_the_terms_t{} );
        static_assert(std::is_same< typename decltype(parsed) :: rest , types_t<>>{} ,"");
        using x = typename should_be_just_one_term< typename decltype(parsed) :: me > :: single_type;
        return x{};
    }

    struct starter_lib {


        // id :: a -> a
        template<typename T>
        auto constexpr
        apply_after_simplification  ( decltype( "id"_charpack )
                            , T t
                            )
        -> T
        { return std::move(t); }


        template<int I, int J>
        auto constexpr
        apply_after_simplification  ( decltype( "+"_charpack )
                            , std::integral_constant<int,I>
                            , std::integral_constant<int,J>
                            )
        -> std::integral_constant<int, I+J>
        { return {}; }


        template<int I, int J>
        auto constexpr
        apply_after_simplification  ( decltype( "-"_charpack )
                            , std::integral_constant<int,I>
                            , std::integral_constant<int,J>
                            )
        -> std::integral_constant<int, I-J>
        { return {}; }


        template<char ...c>
        auto constexpr
        apply_after_simplification  ( decltype( "length"_charpack )
                            , utils::char_pack<c...>
                            )
        -> std::integral_constant<int, sizeof...(c)>
        { return {}; }


    };
    struct extra_lib_with_multiplication {

        template<int I, int J>
        auto constexpr
        apply_after_simplification  ( decltype( "*"_charpack )
                            , std::integral_constant<int,I>
                            , std::integral_constant<int,J>
                            )
        -> std::integral_constant<int, I*J>
        { return {}; }

        auto constexpr
        get_simple_named_value  ( decltype( "three"_charpack ) )
        { return std::integral_constant<int, 3>{}; }

        auto constexpr
        get_simple_named_value  ( decltype( "four"_charpack ) )
        { return std::integral_constant<int, 4>{}; }
    };

    struct combined_lib
    {
        starter_lib lib1;
        extra_lib_with_multiplication lib2;

        /*
         * Two types of call must be forwarded, 'apply_after_simplification_helper'
         * and 'get_simple_named_value'. The relevant call must appear in
         * exactly one of the two libraries.
         */


        // First, apply_after_simplification_helper
        template< typename ...T
                , typename id = utils:: id_t
                >
        auto constexpr
        apply_after_simplification_helper  ( T ...t)
        ->decltype(id{}(lib1).apply_after_simplification(std::move(t)...)  )
        {   return id{}(lib1).apply_after_simplification(std::move(t)...); }

        template< typename ...T
                , typename id = utils:: id_t
                >
        auto constexpr
        apply_after_simplification_helper  ( T ...t)
        ->decltype(id{}(lib2).apply_after_simplification(std::move(t)...)  )
        {   return id{}(lib2).apply_after_simplification(std::move(t)...); }

        template<typename ...T>
        auto constexpr
        apply_after_simplification  ( T ...t)
        {   return combined_lib::apply_after_simplification_helper(std::move(t)...); }


        /* Second, 'get_simple_named_value'
         * Define two helper overloads, one for each sub-library.
         * Then forward the call
         */
        template<typename T
                , typename id = utils:: id_t
                >
        auto constexpr
        get_simple_named_value_overload  ( T&& t)
        ->decltype(id{}(lib1).get_simple_named_value(std::forward<T>(t))  )
        {   return id{}(lib1).get_simple_named_value(std::forward<T>(t)); }

        template<typename T
                , typename id = utils:: id_t
                >
        auto constexpr
        get_simple_named_value_overload  ( T&& t)
        ->decltype(id{}(lib2).get_simple_named_value(std::forward<T>(t))  )
        {   return id{}(lib2).get_simple_named_value(std::forward<T>(t)); }

        template<char ... c>
        auto constexpr
        get_simple_named_value  ( utils::char_pack<c...> name)
        ->decltype(combined_lib::get_simple_named_value_overload(name))
        {   return combined_lib::get_simple_named_value_overload(name); }
    };



    template< typename T
            , typename Lib // for the 'library' - initially simply 'starter_lib', but the user can extend it
            , typename = void /* for void_t */>
    struct simplifier;



    template<typename T, typename Lib>
    constexpr auto
    call_the_simplifier(T t, Lib l)
    { return simplifier<T, Lib>::simplify(t, l); }



    // simplifier for all digits
    template<char first_digit, char ...c, typename Lib>
    struct simplifier   < utils::char_pack<first_digit, c...>
                        , Lib
                        , utils::void_t<std::enable_if_t<
                            is_digit_constexpr(first_digit)
                          >>>
    {
        static auto constexpr
        simplify(utils::char_pack<first_digit, c...> digits, Lib)
        { return std::integral_constant<int, utils::char_pack_to_int(digits)>{}; }
    };

    namespace detail {
        template<char ...c>
        static auto constexpr
        drop_leading_apostrophe(utils::char_pack<'\'', c...>)
        -> utils::char_pack<c...>
        { return {}; }

        template<char ...c>
        static auto constexpr
        reverse(utils::char_pack<c...>)
        -> typename utils::reverse_pack<char, utils::char_pack<c...> >::type
        { return {}; }

        constexpr char apostrophe = '\'';

        template<typename T> // T is always a char_pack
        struct squash_consecutive_apostrophes_struct;

        // base case, the empty string
        template<>
        struct squash_consecutive_apostrophes_struct<utils::char_pack< >>
        { using type = utils::char_pack<>; };

        template<char ...c>
        struct squash_consecutive_apostrophes_struct<utils::char_pack< apostrophe,apostrophe, c... >>
        {
            using recursive_type = typename squash_consecutive_apostrophes_struct< utils::char_pack<c...> >::type;
            using type = typename utils::concat_nontype_pack< char
                                                            , utils::char_pack<apostrophe>
                                                            //, utils::char_pack<c...>
                                                            , recursive_type
                                                            > :: type;
        };

        template<char next, char ...c>
        struct squash_consecutive_apostrophes_struct<utils::char_pack< next, c... >>
        {
            static_assert(next != apostrophe ,"");
            using recursive_type = typename squash_consecutive_apostrophes_struct< utils::char_pack<c...> >::type;
            using type = typename utils::concat_nontype_pack< char
                                                            , utils::char_pack<next>
                                                            //, utils::char_pack<c...>
                                                            , recursive_type
                                                            > :: type;
        };

        template<char ...c>
        static auto constexpr
        squash_consecutive_apostrophes(utils::char_pack<c...>)
        -> typename squash_consecutive_apostrophes_struct<utils::char_pack<c...>> :: type
        { return {}; }

        template<char ...c>
        static auto constexpr
        parse_string_literal(utils::char_pack<'\'', c...> crpk)
        {
            return squash_consecutive_apostrophes( reverse(drop_leading_apostrophe( reverse (drop_leading_apostrophe(crpk) ))));
        }
    }

    // simplifier for string literals
    template<typename StringLiteral, typename Lib>
    struct simplifier   < StringLiteral
                        , Lib
                        , utils::void_t<std::enable_if_t<
                                   '\'' ==            StringLiteral::at(0)
                          >>>
    {
        static auto constexpr
        simplify(StringLiteral sl, Lib)
        { return detail::parse_string_literal(sl); }
    };

    namespace detail
    {
        /* has_get_simple_named_value */


        template<typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value_helper(utils::priority_tag<2>)
        -> decltype( std::declval<Lib&>().get_simple_named_value(std::declval<Name&>())
            , std:: true_type{})
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value_helper(utils::priority_tag<1>)
        -> std:: false_type
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value()
        -> decltype(auto)
        { return detail:: has_get_simple_named_value_helper<Name,Lib>(utils::priority_tag<9>{}); }

        template<typename Name, typename Lib>
        bool constexpr
        has_get_simple_named_value_v =
            detail:: has_get_simple_named_value<Name,Lib>();

    }

    // simplifier for simple names, that directly appear in the library
    template<typename Name, typename Lib>
    struct simplifier   < Name
                        , Lib
                        , utils::void_t<std::enable_if_t<
                                    detail::has_get_simple_named_value_v<Name, Lib>
                          >>>
    {
        static_assert(!is_grouper(Name::at(0)) ,"");
        struct gather_args_later
        {
            Name m_f;
            Lib m_lib;

            template<typename ...T>
            auto constexpr
            operator()  ( T && ...t)
            { return m_lib.apply_after_simplification( m_f , std::forward<T>(t) ... ); }
        };

        static_assert( detail::has_get_simple_named_value_v<Name, Lib> ,"");
        static_assert(!is_digit_constexpr(Name::at(0))  ,"");
        static_assert(!( '\'' ==          Name::at(0))  ,"");

        static auto constexpr
        simplify(Name name, Lib lib) -> auto
        { return lib.get_simple_named_value(name); }
    };

    // simplifier for names (functions only, for now)
    template<typename Name, typename Lib>
    struct simplifier   < Name
                        , Lib
                        , utils::void_t<std::enable_if_t<
                                !( is_digit_constexpr(Name::at(0)) )
                             && !( '\'' ==            Name::at(0)  )
                             && ! detail::has_get_simple_named_value_v<Name, Lib>
                          >>>
    {
        static_assert(!is_grouper(Name::at(0)) ,"");
        struct gather_args_later
        {
            Name m_f;
            Lib m_lib;

            template<typename ...T>
            auto constexpr
            operator()  ( T && ...t)
            { return m_lib.apply_after_simplification( m_f , std::forward<T>(t) ... ); }
        };

        static auto constexpr
        simplify(Name f, Lib lib) -> gather_args_later { return {std::move(f), std::move(lib)}; }
    };


    // simplifier to apply '('
    template<typename Func, typename ...Args, typename Lib>
    struct simplifier   < grouped_t<'(', types_t<Func, Args...   >>
                        , Lib
                        >
    {
        static auto constexpr
        simplify(grouped_t<'(', types_t<Func, Args...> >, Lib lib)
        {
            return
            call_the_simplifier(Func{}, lib)
                (
                    call_the_simplifier(Args{}, lib)...  // pass the arguments
                );
        }
    };

    template<typename AST>
    auto constexpr
    simplify(AST ast)
    {   return call_the_simplifier(ast, starter_lib{}); }

    template<typename AST, typename Lib>
    auto constexpr
    simplify(AST ast, Lib l)
    {   return call_the_simplifier(ast, l); }
}
