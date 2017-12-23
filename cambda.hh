/*
 * cambda - Write lambdas in C++ within an embedded Lisp-like language. Header-only. Very 'constexpr'-friendly. The c in cambda stands for constexpr.
 *
 * https://github.com/aaronmcdaid/cambda
 */

#include<utility>
#include<tuple>
#include<algorithm> // just for std::min
#include<string>

using std::size_t;


namespace cambda_utils {


template<int i>
struct priority_tag;
template<int i>
struct priority_tag : public priority_tag<i-1> {};
template<>
struct priority_tag<0> {};


template<char ... chars>
struct char_pack {
    constexpr static char   c_str0_[] =  {chars..., '\0'};

    constexpr static size_t size()      { return sizeof...(chars); }
    constexpr static char   const   (&c_str0(void)) [size()+1]     { return c_str0_; }
    constexpr static char   at(size_t i)        { return c_str0_[i]; }

    constexpr static char   last()              { return c_str0_[ size()-1 ]; }

    auto constexpr
    operator==(cambda_utils:: char_pack<chars...> )
    -> std::true_type
    { return {}; }

    template<char ... other_chars>
    auto constexpr
    operator==( cambda_utils:: char_pack<other_chars...> )
    -> std::false_type
    { return {}; }
};
template<char ... chars>
constexpr char   char_pack<chars...>:: c_str0_[]; // storage for this data member

template< typename Stream, char ... c>
Stream &
operator<<(Stream &o, cambda_utils::char_pack<c...> s)
{
    o << s.c_str0();
    return o;
}

constexpr
int char_pack_to_number(cambda_utils::char_pack<>, int prefix_already_processed)
{ return prefix_already_processed; }

// this is to deal with literals such as '15c', which should be returned as
// std::integral_constant<int, 15>{} from the simplifier
constexpr
int char_pack_to_number(cambda_utils::char_pack<'c'>, int prefix_already_processed)
{ return prefix_already_processed; }

template<char one_digit>
constexpr
double char_pack_to_number(cambda_utils::char_pack<'.', one_digit>, int prefix_already_processed)
{ return prefix_already_processed + 0.1 * (one_digit-'0'); }

template<char digit1, char digit2, char ... digits>
constexpr
double char_pack_to_number(cambda_utils::char_pack<'.', digit1, digit2, digits...>, int prefix_already_processed)
{
    return prefix_already_processed
                + 0.1 * (digit1-'0')
                + 0.1 * char_pack_to_number(cambda_utils::char_pack<'.', digit2, digits...>{}, 0)
                ;
}

template<char next_digit
        , char ... c
        , typename = std::enable_if_t< (next_digit >= '0' && next_digit <= '9') >
        >
constexpr auto
char_pack_to_number(cambda_utils::char_pack<next_digit, c...>, int prefix_already_processed = 0)
{
    static_assert( next_digit >= '0' && next_digit <= '9' ,"");
    return  char_pack_to_number(cambda_utils::char_pack<c...>{}, 10*prefix_already_processed + (next_digit-'0'));
}


/* id_t
 * ====
 *
 * useful to convert some errors into SFINAE. For example, if you have a
 * trailing return type
 *      -> decltype(x.foo())
 * where the type of 'x' is not deduced, then you will get a hard error
 * instead of SFINAE. If this is undesirable, and you want this method
 * to dissappear if 'x' does not have a 'foo' method, then you can do
 * this instead:
 *      template< typename id = cambda_utils:: id_t >
 *      auto bar()
 *      -> decltype( id{}(x) .foo())
 *      {
 *      ...
 *      }
 * You replace 'x' with 'id{}(x)' (which is essentially a no-op), and
 * put 'id' in template arguments with the suitable default.
 *
 */

struct id_t
{
    template<typename T>
    constexpr T
    operator() (T&& t) const
    { return t; }
};


/*
 * void_t
 * ======
 */

namespace detail { template<typename ...T> struct voider { using type = void; }; }

template<typename ...T>
using void_t = typename detail:: voider<T...>:: type;


/* concat_nontype_pack and reverse_pack
 * ===================     ============
 *  Some utilities for packs of non-types
 */

template< typename T
        , typename L
        , typename R>
struct concat_nontype_pack;

template< typename T
        , T ... left
        , T ... right
        , template<T...> class tmplt
        >
struct concat_nontype_pack  < T
                        , tmplt<left...>
                        , tmplt<right...>>
{   using type = tmplt<left..., right...>; };

template< typename T , typename Pack >
struct reverse_pack;

template< typename T , template<T...> class tmplt >
struct reverse_pack<T, tmplt<> >
{ using type = tmplt<>; };

template< typename T , template<T...> class tmplt
        , T first
        , T ... c>
struct reverse_pack<T, tmplt<first,c...>>
{
    using tail_reversed = typename reverse_pack<T, tmplt<c...> > :: type;
    using type = typename cambda_utils:: concat_nontype_pack  < T, tail_reversed , tmplt<first> >::type;
};


/* equal_string_array and equal_array
 * ==================     ===========
 */

constexpr bool
equal_string_array  (   const char *l
                    ,   const char *r  )
{
    for(int i=0; ; ++i)
    {
        if(l[i] == '\0' && r[i] == '\0')
            return true;
        if(l[i] == '\0' || r[i] == '\0')
            return false;
        if(l[i] != r[i])
            return false;
    }
}

template< typename T , size_t N1, size_t N2 >
constexpr bool
equal_array (   T const (&l) [N1]
            ,   T const (&r) [N2] )
{
    if(N1 != N2)
        return false;
    for(size_t i=0; i<N1 ; ++i)
    {
        if(l[i] != r[i])
            return false;
    }
    return true;
}


/*
 * scalable_make_index_sequence
 * ============================
 * Because clang 3.8 doesn't like std::make_index_sequence with more than about 256.
 * So I'll make my own one
 *
 */
namespace detail {
template< size_t N
        , typename = void > /* this is pointless, just to allow me to make a very general specialization */
struct scalable_make_index_sequence_helper;

template<>
struct scalable_make_index_sequence_helper<0>
{ using type = std::index_sequence<>; };

template<>
struct scalable_make_index_sequence_helper<1>
{ using type = std::index_sequence<0>; };

template< size_t add_me_to_the_right_hand_side
        , size_t ... Ls
        , size_t ... Rs >
auto
concat_index_packs  (   std::index_sequence<Ls...>
                    ,   std::index_sequence<Rs...>  )
-> std::index_sequence<Ls..., add_me_to_the_right_hand_side + Rs...>
{ return {}; }

template<size_t N>
struct scalable_make_index_sequence_helper<N>
{
    static_assert(N >= 2, "");
    constexpr static size_t mid = N/2;
    using left          = typename scalable_make_index_sequence_helper<   mid > :: type;
    using right_shifted = typename scalable_make_index_sequence_helper< N-mid   > :: type;
    using type          = decltype(concat_index_packs<mid>(left{}, right_shifted{}));

};
} // namespace detail

template<size_t N>
using scalable_make_index_sequence = typename detail:: scalable_make_index_sequence_helper<N> :: type;

static_assert(std::is_same< scalable_make_index_sequence<0> , std::make_index_sequence<0> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<1> , std::make_index_sequence<1> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<2> , std::make_index_sequence<2> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<3> , std::make_index_sequence<3> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<4> , std::make_index_sequence<4> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<42 > , std::make_index_sequence<42 > >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<200> , std::make_index_sequence<200> >{} ,"");



/* type_t
 * ======
 *  Used only for the 'typeof' cambda function. Handy though,
 *  it can be accepted by 'fix'
 */
template<typename T>
struct type_t {
    using type = T;
};


/* my_forward_as_tuple
 * ===================
 * Not for &&-refs. Values and &-refs are OK though
 */
template<typename ... T>
constexpr auto
my_forward_as_tuple(T && ... t)
-> std::tuple<T ...>
{ return std::tuple<T ...>{ std::forward<T>(t) ... }; }


} // namespace cambda_utils



namespace cambda {
    /* types_t
     * =======
     *  This is used a lot in this. It's simply to store a list of
     *  types. The AST (Abstract Syntax Tree) that is parsed from the
     *  input string is represented as an empty object whose type
     *  encodes everything. A set of recursive 'types_t' instantiations.
     */

    template<typename ... T>
    struct types_t {
        template<typename Prepend>
        using prepend = types_t<Prepend, T...>;

        constexpr static size_t size = sizeof...(T);

        using is_a_types_t_object = void; // no really needed. Just to help an assertion in 'grouped_t'
    };




    /*
     * UDL for _charpack. Convenient in this header file, no need to use
     * it from outside.
     */
    template<typename T, T ... chars>
    constexpr auto
    operator"" _charpack ()
    -> cambda_utils:: char_pack<chars...>
    { return {}; }

namespace parsing {
    // Open up the 'parsing' namespace for a few lines here,
    // these few functions are useful when defining 'grouped_T'

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

} // namespace parsing

    /*
     * grouped_t
     * =========
     *  The contents of any (...), {...}, or [...] will
     *  be stored in this.
     */
    template< char c
            , typename T // T will always be an instance of types_t<U...>. See assertion below
            >
    struct grouped_t {
        static_assert(parsing::is_opener(c) ,"");
        constexpr static char my_closer = parsing::opener_to_closer(c);
        static_assert(my_closer != -1 ,"");

        // finally, we confirm T is an instance of types_t<U...>
        static_assert(std::is_same<void, typename T::is_a_types_t_object>{} ,"");
    };


    /* Now to finally start on the parsing. First, tokenizing */

namespace parsing {

    template< typename C >
    auto constexpr
    find_next_token(C, size_t o)
    ->std::pair<size_t,size_t>
    {
        // first, skip whitespace
        while(C::at(o) != '\0' && is_whitespace(C::at(o)))
        {
            while(C::at(o) != '\0' && is_whitespace(C::at(o)))
                ++o;
            // a one-line comment begins with whitespace followed by #{},
            // and runs until the the end of the line
            //  (foo bar)       #{} this is a comment
            // If you, for some reason, want this to not be interpreted as a comment,
            // then put a space after the '#', or between the '{' and '}'
            if  (   C::at(o) == '#'
                 && C::at(o+1) == '('
                 && C::at(o+2) == ')')
            {
                while(C::at(o) != '\n')
                    ++o;
            }
        }


        size_t const start = o;

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
        //
        // A trailing  c  on the end means a "constant string",
        // kind of like std::integral_constant for strings
        if(C::at(o) == '\'') {
            ++o;
            while(true)
            {
                if(C::at(o) == '\'' && C::at(o+1) == '\'')
                { o+=2; continue; }

                if(C::at(o) == '\'')
                {
                    o+=1;
                    if(C::at(o) == 'c')
                        o+=1;
                    break;
                }

                ++o;
            }
            return std::make_pair(start, o);
        }

        // if we get this far, we just have a string of non-special characters.
        // We must read them all in
        while (C::at(o) != '\0' && !is_whitespace(C::at(o)) && !is_grouper(C::at(o)))
            ++o;
        return std:: make_pair(start, o);
    }


    template<typename T>
    auto
    get_last_type( ::cambda:: types_t<T> )
    -> T
    { return {}; }

    template<typename S, typename T, typename ... U>
    auto
    get_last_type(  ::cambda::types_t<S, T, U...> )
    {
        return get_last_type( ::cambda::types_t<T,U...> {});
    }


    template<typename T>
    auto
    drop_last_type( types_t<T> )
    -> types_t<>
    { return {}; }

    template<typename S, typename T, typename ... U>
    auto
    drop_last_type( types_t<S, T, U...> )
    {
        using return_type =
        typename decltype( drop_last_type(std::declval< types_t<T,U...> >()) )
                // , but then prepend S to the result:
              ::template prepend<S>;
        return return_type{};
    }


    // a few toString overloads just to print the output after parsing to help debugging

    constexpr int indent_each_time = 4;

    template<char ... c>
    std::string
    toString( cambda_utils::char_pack<c...> s, int = 0)
    { return s.c_str0(); }

    std::string
    toString( types_t<> , int = 0)
    { return ""; }

    template<typename S>
    std::string
    toString( types_t<S> , int indent = 0)
    { return toString( S{}, indent); }

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
        return      std::string{c}
                +   std::string(indent_each_time-1, ' ')
                +   toString(T{}, indent+indent_each_time)
                +   std::string{grp.my_closer} // the closer appears on the same line as the last item
                ;
    }



    template<typename E>
    constexpr static size_t
    count_the_terms(E e)
    {
        size_t n = 0;
        size_t o = 0;
        while(true) {
            auto tk = find_next_token(e, o);
            if(tk.first == tk.second)
                break;
            o = tk.second;
            ++n;
        }
        return n;
    }

    template< size_t number_of_terms >
    struct all_token_pairs_t
    {
        size_t starts[number_of_terms+1]; // extra one for the special zero-width 'end' token
        size_t ends  [number_of_terms+1];
    };

    template< size_t number_of_terms
            , typename E>
    auto constexpr
    get_all_token_pairs(E e)
    -> all_token_pairs_t<number_of_terms>
    {
        auto first_token = find_next_token(e, 0);
        all_token_pairs_t<number_of_terms> res{};
        res.starts[0] = first_token.first;
        res.ends  [0] = first_token.second;
        for(size_t i = 1; i<=number_of_terms; ++i)
        {
            auto next_token = find_next_token(e, res.ends[i-1]);
            res.starts[i] = next_token.first;
            res.ends  [i] = next_token.second;
        }
        return res;
    }

    template<typename E>
    struct all_the_terms_as_types
    {
        constexpr all_the_terms_as_types(){}

        constexpr static E e {};

        constexpr static auto number_of_terms = count_the_terms(e);
        constexpr static auto all_token_pairs = get_all_token_pairs<number_of_terms>(e);
        static_assert(all_token_pairs.starts[number_of_terms] == all_token_pairs.ends[number_of_terms] ,"");
        static_assert(all_token_pairs.starts[number_of_terms-1] < all_token_pairs.ends[number_of_terms-1] ,"");

        template<size_t I, size_t ...J>
        static auto constexpr
        just_one_token_as_a_string(std::index_sequence<J...>)
        {
            constexpr size_t offset_of_first_char_of_this_token = all_token_pairs.starts[I];
            return cambda_utils::char_pack< E::at(offset_of_first_char_of_this_token + J) ... >{};
        }

        template<size_t ...I>
        static auto constexpr
        every_token_as_a_string(std::index_sequence<I...>)
        {
            return types_t<
                decltype(just_one_token_as_a_string<I>(
                            std::make_index_sequence<
                                                    all_token_pairs.ends[I]
                                                    -all_token_pairs.starts[I]
                                                    >{}))
                ... // across all the tokens
                >{};
        }

        using all_the_terms_t = decltype(every_token_as_a_string( cambda_utils::scalable_make_index_sequence<number_of_terms>{} ));

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
    struct parse_many_things<cambda::types_t<>, void>
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
            cambda_utils::char_pack<c>, T...
        >
        , std::enable_if_t<is_closer(c)>
        >
    {
        using me    = types_t< cambda_utils::char_pack<c> >;
        using rest  = types_t<T...>;
    };

    // ... but '('/'{'/'[' open the list. This is more complex:
    template<char o, typename ... T>
    struct parse_many_things<types_t<
            cambda_utils::char_pack<o>, T...
        >
        , std::enable_if_t<is_opener(o)>
        >
    {
        using next_and_future =  parse_many_things<types_t<T...>>;
        // next_and_future::me is the rest of my group
        // next_and_future::rest is the distant future, which must be parsed here too

        using mygroup_with_the_closer = typename next_and_future::me;
        static_assert(std::is_same<void, typename mygroup_with_the_closer :: is_a_types_t_object>{} ,"");

        // Check that the openers and closers "match". See the static_assert
        using my_closer = decltype(::cambda::parsing::get_last_type(mygroup_with_the_closer{}));

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


    template<typename E>
    auto constexpr
    parse_ast(E )
    {
        //using all_the_terms_t = typename parse_flat_list_of_terms<E, 0>::all_the_terms; /*This was the broken version - clang doesn't like this*/
        using all_the_terms_t = typename parsing::all_the_terms_as_types<E>::all_the_terms_t;

        auto parsed =  parsing::parser( all_the_terms_t{} );
        static_assert(std::is_same< typename decltype(parsed) :: rest , types_t<>>{} ,"");
        using x = typename decltype(parsed) :: me;
        return x{};
    }
}// namespace parsing


} // namespace ?
namespace cambda {


    /* is_valid_member_of_a_tuple_of_libs
     * ==================================
     *  This starts the new (Xmas 2017) approach to replace the combiners
     *  By default, return false. Define other specializations later in this
     *  file, as required.
     */
    template<typename>
    struct is_valid_member_of_a_tuple_of_libs
    { constexpr static bool value = false; };

    template<typename>
    struct is_valid_tuple_of_libs
    { constexpr static bool value = false; };

    template<typename ... T>
    struct is_valid_tuple_of_libs<std::tuple<T...>>
    {
        static_assert(  std::min(std::initializer_list<bool>{!std::is_rvalue_reference<T>{}                 ... }  ) ,""); // all are refs (l-ref or r-ref)
        static_assert(  std::min(std::initializer_list<bool>{ is_valid_member_of_a_tuple_of_libs<T>::value  ... }  ) ,""); // no 'combiners' allowed
        constexpr static
        bool value =    std::min(std::initializer_list<bool>{!std::is_rvalue_reference<T>{}                 ... }  )
                     && std::min(std::initializer_list<bool>{ is_valid_member_of_a_tuple_of_libs<T>::value  ... }  );
    };

    template<typename ... T>
    struct is_valid_tuple_of_libs<const std::tuple<T...>>
    : public is_valid_tuple_of_libs<std::tuple<T...>>
    {};

    template<typename T>
    constexpr bool
    is_valid_tuple_of_libs_v = is_valid_tuple_of_libs<T>::value;


    template< typename Lib1
            , typename Lib2 >
    struct library_combiner
    {
        Lib1 lib1;
        Lib2 lib2;

        template< typename Self>
        constexpr auto static
        getlib1(Self && self)
        -> decltype(auto)
        {
            static_assert(!std::is_rvalue_reference<Lib1>{} ,"");
            using ReturnType = std::conditional_t   <   std::is_lvalue_reference<Lib1>{}
                                                    ,   decltype((self.lib1))
                                                    ,   std::remove_reference_t<decltype((self.lib1))>
                                                    >;
            return std::forward<ReturnType>(self.lib1);
        }
        template< typename Self>
        constexpr auto static
        getlib2(Self && self)
        -> decltype(auto)
        {
            static_assert(!std::is_rvalue_reference<Lib2>{} ,"");
            using ReturnType = std::conditional_t   <   std::is_lvalue_reference<Lib2>{}
                                                    ,   decltype((self.lib2))
                                                    ,   std::remove_reference_t<decltype((self.lib2))>
                                                    >;
            return std::forward<ReturnType>(self.lib2);
        }

        static_assert(!std::is_rvalue_reference<Lib1>{} ,"");
        static_assert(!std::is_rvalue_reference<Lib2>{} ,"");

        template< typename T
                , typename U
                >
        constexpr
        library_combiner(T&&t, U&&u)
            : lib1(std::forward<T>(t))
            , lib2(std::forward<U>(u))
        {}


        /* First, 'apply_after_simplification'
         * Define two helper overloads, one for each sub-library.
         * Then forward the call
         */
        template< typename ... T
                , typename Self
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification( Self && self, T && ... t)
        ->decltype(id{}(self). getlib1(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...)  )
        {   return     (self). getlib1(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...); }

        template< typename ... T
                , typename Self
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification(Self && self, T && ... t)
        ->decltype(id{}(self). getlib2(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...)  )
        {   return     (self). getlib2(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...); }

        // ... same as above, but const
        template< typename ... T
                , typename Self
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification(Self && self, T && ... t) const
        ->decltype(id{}(self). getlib1(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...)  )
        {   return     (self). getlib1(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...); }

        template< typename ... T
                , typename Self
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification(Self && self, T && ... t) const
        ->decltype(id{}(self). getlib2(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...)  )
        {   return     (self). getlib2(*this). apply_after_simplification(std::forward<Self>(self), std::forward<T>(t)...); }

        /* Now, 'static_get_simple_named_value'
         * Define two helper overloads, one for each sub-library.
         * Then forward the call
         */
        template< typename Self
                , char ... cs
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr static
        static_get_simple_named_value(Self && self, cambda_utils::char_pack<cs...> name)
        ->decltype(self.getlib1(std::forward<Self>(self)).static_get_simple_named_value(self.getlib1(std::forward<Self>(self)), name)  )
        {   return self.getlib1(std::forward<Self>(self)).static_get_simple_named_value(self.getlib1(std::forward<Self>(self)), name); }
        template< typename Self
                , char ... cs
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr static
        static_get_simple_named_value(Self && self, cambda_utils::char_pack<cs...> name)
        ->decltype(self.getlib2(std::forward<Self>(self)).static_get_simple_named_value(self.getlib2(std::forward<Self>(self)), name)  )
        {   return self.getlib2(std::forward<Self>(self)).static_get_simple_named_value(self.getlib2(std::forward<Self>(self)), name); }


    };


    template< typename Lib1
            , typename Lib2
            >
    constexpr auto
    combine_libraries(Lib1 && lib1, Lib2 && lib2)
    -> library_combiner <   Lib1
                        ,   Lib2
                        >
    { return {std::forward<Lib1>(lib1),std::forward<Lib2>(lib2)}; }


    template< typename Lib1
            , typename Lib2
            , typename Lib3
            , typename ...Libs
            >
    constexpr auto
    combine_libraries   (   Lib1 && lib1
                        ,   Lib2 && lib2
                        ,   Lib3 && lib3
                        ,   Libs && ... libs   )
    -> decltype(auto)
    {   return combine_libraries    (   std::forward<Lib1>(lib1)
                                    ,   combine_libraries   (   std::forward<Lib2>(lib2)
                                                            ,   std::forward<Lib3>(lib3)
                                                            ,   std::forward<Libs>(libs)... )); }

    struct nil_t { }; // to be returned if you write () in cambda, i.e. "()"_cambda()

    template< typename T
            , typename = void /* for void_t */>
    struct simplifier;



    template<typename Libs, typename T, typename Lib>
    constexpr auto
    call_the_simplifier(Libs &libs, T t, Lib && l)
    ->decltype(simplifier<T>::simplify(libs, t, std::forward<Lib>(l))  )
    {
        static_assert(is_valid_tuple_of_libs_v<Libs> ,"");
        return simplifier<T>::simplify(libs, t, std::forward<Lib>(l));
    }



    // simplifier for all digits
    template<char first_digit, char ...c>
    struct simplifier   < cambda_utils::char_pack<first_digit, c...>
                        , cambda_utils::void_t<std::enable_if_t<
                            parsing::is_digit_constexpr(first_digit)
                            && parsing::is_digit_constexpr(cambda_utils::char_pack<first_digit, c...> :: last())
                          >>>
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &, cambda_utils::char_pack<first_digit, c...> digits, L && )
        { return cambda_utils::char_pack_to_number(digits); }
    };

    // simplifier for digits with trailing 'c', for an integral constant
    template<char first_digit, char ...c>
    struct simplifier   < cambda_utils::char_pack<first_digit, c...>
                        , cambda_utils::void_t<std::enable_if_t<
                            parsing::is_digit_constexpr(first_digit)
                            && cambda_utils::char_pack<first_digit, c...> :: last() == 'c'
                          >>>
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &, cambda_utils::char_pack<first_digit, c...> , L &&)
        { return std::integral_constant<int, cambda_utils::char_pack_to_number(cambda_utils::char_pack<first_digit, c...>{})>{}; }
    };

    namespace detail {
        template<char ...c>
        static auto constexpr
        drop_leading_c(cambda_utils::char_pack<'c', c...>)
        -> cambda_utils::char_pack<c...>
        { return {}; }

        template<char ...c>
        static auto constexpr
        drop_leading_apostrophe(cambda_utils::char_pack<'\'', c...>)
        -> cambda_utils::char_pack<c...>
        { return {}; }

        template<char ...c>
        static auto constexpr
        reverse(cambda_utils::char_pack<c...>)
        -> typename cambda_utils::reverse_pack<char, cambda_utils::char_pack<c...> >::type
        { return {}; }

        constexpr char apostrophe = '\'';

        template<typename T> // T is always a char_pack
        struct squash_consecutive_apostrophes_struct;

        // base case, the empty string
        template<>
        struct squash_consecutive_apostrophes_struct<cambda_utils::char_pack< >>
        { using type = cambda_utils::char_pack<>; };

        template<char ...c>
        struct squash_consecutive_apostrophes_struct<cambda_utils::char_pack< apostrophe,apostrophe, c... >>
        {
            using recursive_type = typename squash_consecutive_apostrophes_struct< cambda_utils::char_pack<c...> >::type;
            using type = typename cambda_utils::concat_nontype_pack< char
                                                            , cambda_utils::char_pack<apostrophe>
                                                            //, cambda_utils::char_pack<c...>
                                                            , recursive_type
                                                            > :: type;
        };

        template<char next, char ...c>
        struct squash_consecutive_apostrophes_struct<cambda_utils::char_pack< next, c... >>
        {
            static_assert(next != apostrophe ,"");
            using recursive_type = typename squash_consecutive_apostrophes_struct< cambda_utils::char_pack<c...> >::type;
            using type = typename cambda_utils::concat_nontype_pack< char
                                                            , cambda_utils::char_pack<next>
                                                            //, cambda_utils::char_pack<c...>
                                                            , recursive_type
                                                            > :: type;
        };

        template<char ...c>
        static auto constexpr
        squash_consecutive_apostrophes(cambda_utils::char_pack<c...>)
        -> typename squash_consecutive_apostrophes_struct<cambda_utils::char_pack<c...>> :: type
        { return {}; }

        template<char ...c>
        static auto constexpr
        parse_string_literal(cambda_utils::char_pack<'\'', c...> crpk)
        {
            return squash_consecutive_apostrophes( reverse(drop_leading_apostrophe( reverse (drop_leading_apostrophe(crpk) ))));
        }
    }

    // simplifier for string literals
    template<typename StringLiteral>
    struct simplifier   < StringLiteral
                        , cambda_utils::void_t<std::enable_if_t<
                                   '\'' ==            StringLiteral::at(0)
                                   && '\'' ==         StringLiteral::last()
                          >>>
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &,StringLiteral sl, L &&)
        { return detail::parse_string_literal(sl).c_str0(); }
    };

    // simplifier for string literals
    template<typename StringLiteral>
    struct simplifier   < StringLiteral
                        , cambda_utils::void_t<std::enable_if_t<
                                   '\'' ==            StringLiteral::at(0)
                                   && 'c' ==          StringLiteral::last()
                          >>>
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &,StringLiteral sl, L &&)
        {
            (void)sl;
            return  detail::parse_string_literal(
                    detail::reverse(
                    detail::drop_leading_c(
                    detail::reverse(sl))));
        }
    };

    namespace detail
    {
        /* has_static_get_simple_named_value */

        template< typename Name, typename Lib>
        static auto constexpr
        has_static_get_simple_named_value_helper(cambda_utils::priority_tag<2>)
        -> decltype(void(
                        std::declval<Lib&>().
                    static_get_simple_named_value(
                        std::declval<Lib&>(),
                        std::declval<Name&>())
                    )
                ,   std:: true_type{}
                )
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_static_get_simple_named_value_helper(cambda_utils::priority_tag<1>)
        -> std:: false_type
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_static_get_simple_named_value()
        -> decltype(auto)
        { return detail:: has_static_get_simple_named_value_helper<Name,Lib>(cambda_utils::priority_tag<9>{}); }

        template<typename Name, typename ... OneLibAmongMany>
        static auto constexpr
        one_of_these_has_static_get_simple_named_value(std::tuple<OneLibAmongMany...> const &)
        -> std::integral_constant<bool, std::max(std::initializer_list<bool>{ has_static_get_simple_named_value<Name, OneLibAmongMany>() ...})>
        { return {}; }

    }

    // simplifier for names.
    // Two overloads, one for where 'get_simple_named_value' is present, and one
    // to capture-and-store-and-forward to 'apply_after_simplification' later
    template<typename Name>
    struct simplifier   < Name
                        , cambda_utils::void_t<std::enable_if_t<
                                !( parsing::is_digit_constexpr(Name::at(0)) )
                             && !( '\'' ==            Name::at(0)  )
                          >>>
    {
        static_assert(!parsing::is_grouper(Name::at(0))          ,"");
        static_assert(!parsing::is_digit_constexpr(Name::at(0))  ,"");
        static_assert( '\'' !=            Name::at(0)   ,"");

        template<typename L, typename Libs
                , bool b = detail::has_static_get_simple_named_value<Name, L>()
                , std::enable_if_t<b, std::integral_constant<int,__LINE__>>* =nullptr
                >
        static auto constexpr
        simplify(Libs & libs,Name name, L && lib)
        ->decltype(lib.static_get_simple_named_value(std::forward<L>(lib), name) )
        {
            static_assert(is_valid_tuple_of_libs_v<Libs> ,"");
            auto B = detail::one_of_these_has_static_get_simple_named_value<Name>(libs);
            (void)B;
            static_assert(B  ,"");
            return lib.static_get_simple_named_value(std::forward<L>(lib), name);
        }

        template<typename Libs, typename L>
        struct gather_args_later
        {
            static_assert(is_valid_tuple_of_libs_v<Libs> ,"");

            Libs & m_libs_reference; // a reference to a tuple (members may be non-refs)
            Name m_f;
            L && m_lib; // L may, or may not, be &-

            template<typename ...T>
            auto constexpr
            operator()  ( T && ...t) && // && means, I think, we are entitled to forward 'm_lib' out, and the 'm_libs_reference' will still be good
            ->decltype( std::forward<L>(m_lib).apply_after_simplification(std::forward<L>(m_lib), m_libs_reference, std::forward<L>(m_lib), m_f , std::forward<T>(t) ... )  )
            {   return  std::forward<L>(m_lib).apply_after_simplification(std::forward<L>(m_lib), m_libs_reference, std::forward<L>(m_lib), m_f , std::forward<T>(t) ... ); }
        };

        template<typename L, typename Libs
                , bool b = !detail::has_static_get_simple_named_value<Name, L>()
                , std::enable_if_t< b, std::integral_constant<int,__LINE__>>* =nullptr
                >
        static auto constexpr
        simplify(Libs & libs_to_be_stored_by_reference,Name f, L && lib) -> gather_args_later<Libs, L> {
            static_assert(is_valid_tuple_of_libs_v<Libs> ,"");
            auto B = detail::one_of_these_has_static_get_simple_named_value<Name>(libs_to_be_stored_by_reference);
            (void)B;
            static_assert(!B  ,"");
            return {libs_to_be_stored_by_reference, std::move(f), std::forward<L>(lib)};
        }
    };


    // simplifier to apply '('
    template<typename Func, typename ...Args>
    struct simplifier   < grouped_t<'(', types_t<Func, Args...   >>
                        >
    {
        template<typename id = cambda_utils::id_t
                , typename Libs
                , typename L >
        static auto constexpr
        simplify(Libs & libs, grouped_t<'(', types_t<Func, Args...> >, L && lib)
        ->decltype(call_the_simplifier(libs, Func{}, id{}(std::forward<L>(lib))) ( call_the_simplifier(libs, Args{}, id{}(std::forward<L>(lib)))...)  )
        {
            return call_the_simplifier(libs, Func{},      std::forward<L>(lib) ) ( call_the_simplifier(libs, Args{},      std::forward<L>(lib)) ...);
        }
    };


    // simplifier to apply '[', i.e. just quote it
    template<typename ...Args>
    struct simplifier   < grouped_t<'[', types_t<Args...   >>
                        >
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &,grouped_t<'[', types_t<Args...> > quoted, L &&)
        ->decltype(auto)
        {
            return quoted;
        }
    };

    // simplifier to apply '{', i.e. swap the first two of three arguments
    template< typename Arg1
            , typename Func
            , typename Arg2>
    struct simplifier   < grouped_t<'{', types_t<Arg1, Func, Arg2>   >
                        >
    {
        template< typename id = cambda_utils:: id_t
                , typename L, typename Libs
                , class ...
                > // we must perfect forward here to avoid a bug in older gcc (5.5.0) where the while loop wasn't repeating
        static auto constexpr
        simplify(Libs & libs,grouped_t<'{', types_t<Arg1, Func, Arg2> >, L && lib)
        ->decltype(simplifier       < grouped_t<'(', types_t<Func, Arg1, Arg2>> >
                        :: simplify ( libs, grouped_t<'(', types_t<Func, Arg1, Arg2>> {} , id{}(std::forward<L>(lib)))
                )
        {
            return simplifier       < grouped_t<'(', types_t<Func, Arg1, Arg2>> >
                        :: simplify ( libs, grouped_t<'(', types_t<Func, Arg1, Arg2>> {} ,      std::forward<L>(lib) )
                ;
        }
    };

    // simplifier for () - simply returns nil_t{}
    template<>
    struct simplifier   < grouped_t<'(', types_t<>   > >
    {
        template<typename L, typename Libs>
        static auto constexpr
        simplify(Libs &, grouped_t<'(', types_t<> >, L &&)
        -> nil_t
        { return {}; }
    };

    template<typename T, char ...c>
    struct binded_name_with_valueOrReference
    {
        // NOTE: T might be a l-reference type
        // However, we always return as l-ref
        T m_x;

        static_assert(!std::is_rvalue_reference<T>{}, "");

        template<typename Self>
        auto constexpr static
        static_get_simple_named_value  (Self && self, cambda_utils::char_pack<c...> )
        ->decltype((self.m_x))
        {
            return self.m_x;
        }
    };

    template<typename T, char ... c>
    struct is_valid_member_of_a_tuple_of_libs<binded_name_with_valueOrReference<T, c...> >
    { constexpr static bool value = true; };

    enum class capture_policy   { byLvalueReference // strictly T&
                                , byValue // std::decay_t<T&&>
                                };

    template< typename B
            , typename T2
            , char ...c2 >
    auto constexpr
    operator,   (   B && beforeComma
                ,   binded_name_with_valueOrReference<T2, c2...> && afterComma)
    ->decltype(auto)
    {
        return combine_libraries(std::forward<B>(beforeComma), std::move(afterComma));
    }

    template< capture_policy cap
            , char ...c>
    struct binding_name
    {
        template<typename V>
        auto constexpr
        operator=(V && v)
        ->decltype(auto)
        {
            static_assert(!std::is_rvalue_reference<V>{} ,"");
            using BoundStorageType = std::conditional_t
                                        < cap == capture_policy:: byLvalueReference
                                        , V &
                                        , std::decay_t<V>
                                        >;
            return binded_name_with_valueOrReference<BoundStorageType, c...>{std::forward<V>(v)};
        }

        template<typename V>
        auto constexpr
        operator &= (V && v)
        ->decltype(auto)
        {
            static_assert(!std::is_rvalue_reference<V>{} ,"");
            using BoundStorageType = V &; // in operator &=, we ignore the capture_policy and just
                                            // store as an lvalue
            static_assert(std::is_lvalue_reference<V>{} ," operator&=() should be used only with lvalues");
            return binded_name_with_valueOrReference<BoundStorageType, c...>{std::forward<V>(v)};
        }
    };
    template<char ...c>
    auto constexpr
    char_pack__to__binding_name(cambda_utils::char_pack<c...>)
    -> binding_name<capture_policy:: byValue, c...>
    {return {};}
    template<char ...c>
    auto constexpr
    char_pack__to__binding_name(grouped_t<'(', types_t<cambda_utils::char_pack<'&'>, cambda_utils::char_pack<c...>>>)
    -> binding_name<capture_policy:: byLvalueReference, c...>
    {return {};}
    template<char ...c>
    auto constexpr
    char_pack__to__binding_name(grouped_t<'(', types_t<cambda_utils::char_pack<'='>, cambda_utils::char_pack<c...>>>)
    -> binding_name<capture_policy:: byValue, c...>
    {return {};}

    template<typename T, T ... chars>
    constexpr auto
    operator"" _binding () {
        return binding_name<capture_policy:: byValue, chars...>{};
    }

    template< typename CodeType>
    struct multi_statement_execution;

    template< typename SingleOne >
    struct multi_statement_execution< types_t<SingleOne> >
    {
        template<typename LibToForward
                , typename Libs >
        auto constexpr static
        eval(Libs & libs, LibToForward && l2f)
        -> decltype(cambda::call_the_simplifier (libs,   SingleOne{} ,   std::forward<LibToForward>(l2f))   )
        {
            return  cambda::call_the_simplifier (libs,   SingleOne{} ,   std::forward<LibToForward>(l2f));
        }
    };

    template< typename A
            , typename ...B
            >
    struct multi_statement_execution< types_t<A, B...>>
    {
        template< typename Libs, typename LibToForward >
        auto constexpr static
        eval  (Libs & libs, LibToForward && lib)
        ->decltype(multi_statement_execution<types_t<B...>> :: eval( libs, std::forward<LibToForward>(lib) ) )
        {

            cambda::call_the_simplifier (libs,   A{} ,   std::forward<LibToForward>(lib));
            return multi_statement_execution<types_t<B...>> :: eval( libs, std::forward<LibToForward>(lib) );
        }
    };
    template< typename BindingName
            , typename BindingExpression
            , typename B
            , typename ... C
            >
    struct multi_statement_execution< types_t<grouped_t<'(',types_t<grouped_t<'[',types_t<>>, grouped_t<'[',types_t<BindingName>>, BindingExpression>>, B, C...>>
    {
        template< typename LibToForward
                , typename Libs
                , typename DecltypeOfTheBoundValue      = decltype( cambda::call_the_simplifier(std::declval<Libs&>(), BindingExpression{}, std::declval<LibToForward>()) )
                , typename NewLibs                      = decltype( std::tuple_cat
                                    (   std::declval<Libs&>()
                                    ,   cambda_utils::my_forward_as_tuple(char_pack__to__binding_name(BindingName{}) = std::declval<DecltypeOfTheBoundValue>())))
                >
        auto constexpr static
        eval  (Libs & libs, LibToForward && lib) // TODO: add in the extra binding
        ->decltype(multi_statement_execution<types_t< B, C...  >>
                    ::eval(std::declval<NewLibs&>(), cambda::combine_libraries    (   std::forward<LibToForward>(lib)
                                                        ,   char_pack__to__binding_name(BindingName{}) = std::declval<DecltypeOfTheBoundValue>()))  )
        {

            decltype(auto) // not-an r-ref. May be l-ref though
                bound_value = cambda::call_the_simplifier(libs, BindingExpression{}, std::forward<LibToForward>(lib));

            auto new_libs = std::tuple_cat ( libs , cambda_utils::my_forward_as_tuple(char_pack__to__binding_name(BindingName{}) = std::forward<DecltypeOfTheBoundValue>(bound_value)));
            static_assert(std::is_same<decltype((new_libs)), NewLibs&>{} ,"");
            static_assert(is_valid_tuple_of_libs_v<NewLibs> ,"");
            (void)new_libs;

            // 'bound_value' is the storage, assuming storage is required.

            static_assert(std::is_same
                            <   decltype(bound_value)
                            ,   decltype(cambda::call_the_simplifier(libs, BindingExpression{}, std::forward<LibToForward>(lib)))    >{} ,"Argh, what does decltype(auto) do on vars?");
            static_assert(!std::is_rvalue_reference<decltype(bound_value)>{} ,""); // TODO: this is probably too strict

            return multi_statement_execution<types_t< B, C...  >>
                    ::eval(new_libs, cambda::combine_libraries    (   std::forward<LibToForward>(lib)
                                                        ,   char_pack__to__binding_name(BindingName{}) = std::forward<DecltypeOfTheBoundValue>(bound_value)));
        }
    };

    template< typename Libs, typename AST, typename Lib>
    struct cambda_object_from_the_string_literal
    {
        Libs   libs; // Shouldn't be an &-ref. Maybe be a tuple of &-refs though
        AST m_ast;
        Lib lib; // may be a &-ref

        static_assert(!std::is_reference<Libs>{} ,"");
        static_assert(is_valid_tuple_of_libs_v<Libs>         ,"");

        template< typename id = cambda_utils:: id_t>
        constexpr auto
        operator() () &&
        -> decltype(auto)
        {
            return ::cambda:: multi_statement_execution<decltype(m_ast)> :: eval( libs, std::forward<Lib> (lib));
        }

        template<typename Binding>
        constexpr auto
        operator[] (Binding && binding_to_insert) && // the '&&' is important, allows us to 'move' from this->lib
        -> decltype(auto)
        {
            static_assert(!std::is_reference<Binding>{} ,"");

            static_assert(is_valid_member_of_a_tuple_of_libs<Binding> :: value ,"");

            auto new_libs = std::tuple_cat(
                    libs ,
                    cambda_utils::my_forward_as_tuple(Binding{binding_to_insert}) // copy it, not move it. TODO: change to move later
                    );
            static_assert( is_valid_tuple_of_libs_v<decltype(new_libs)> ,"");

            return cambda_object_from_the_string_literal    <   decltype(new_libs) // TODO: extend this
                                                            ,   AST
                                                            ,   decltype(
                                                                    combine_libraries(std::forward<Lib>(lib), std::forward<Binding>(binding_to_insert))
                                                                        )>
            { new_libs, m_ast , combine_libraries(std::forward<Lib>(lib), std::forward<Binding>(binding_to_insert)) };
            //return *this;
        }
    };
    template<typename AST, typename Lib
            , typename Libs    >
    auto constexpr
    make_cambda_object_from_the_string_literal(Libs & libs, AST ast, Lib & lib)
    -> cambda_object_from_the_string_literal<Libs
                                            ,AST
                                            ,Lib&
                                            >
    {
        static_assert(is_valid_tuple_of_libs_v<Libs>         ,"");
        return {libs, ast, lib};
    }
    struct starter_lib {
        int ignore;
        constexpr starter_lib() : ignore(0) {} // a default constructor, just because clang requires them for constexpr objects


        // range_based_for
        template< typename LibToForward
                , typename Libs
                , typename Self
                , typename Data
                , typename Func
                >
        auto constexpr
        apply_after_simplification  (   Self &&, Libs &, LibToForward &&, decltype( "range_based_for"_charpack )
                                    ,   Data && data
                                    ,   Func && func
                                    )   const
        -> int
        {
            int count = 0;
            for(auto && x : std::forward<Data>(data))
                std::forward<Func>(func) (std::forward<decltype(x)>(x));
            return count;
        }


        // typeof
        template<typename T
                , typename Libs
                , typename Self
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "typeof"_charpack )
                            , T&& ) const
        -> ::cambda_utils::type_t<T>
        { return {}; }

        // id
        template<typename T
                , typename Libs
                , typename Self
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "id"_charpack )
                            , T&& t) const
        -> T // must be T. Not T&&, not decltype(std::forward<T>(t)). Otherwise, clang notices lifetimes have expired
        { return std::forward<T>(t); }

        // ref2val
        template<typename T
                , typename Libs
                , typename Self
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "ref2val"_charpack )
                            , T& t) const
        -> T
        { return t; }

#define MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION(op_name, infix_op) \
        template< typename Self, typename Libs, typename LibToForward , typename Ti , typename Tj >   \
        auto constexpr                                                  \
        apply_after_simplification ( Self &&, Libs &, LibToForward &&                    \
            , decltype( op_name )                                          \
            , Ti && i , Tj && j) const                                  \
        ->decltype(std::forward<Ti>(i) infix_op std::forward<Tj>(j) )         \
        {   return std::forward<Ti>(i) infix_op std::forward<Tj>(j); }

MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "+"_charpack   , + )
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "*"_charpack   , * )
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "-"_charpack   , - )
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "=="_charpack  , ==)
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "!="_charpack  , !=)
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "<"_charpack   , < )
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( ">"_charpack   , > )
MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION( "&&"_charpack  , && )

#define MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(op_name, op)                            \
        template<typename T                                                             \
                , typename Libs                                                         \
                , typename Self                                                         \
                , typename LibToForward >                                               \
        auto constexpr                                                                  \
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( op_name       ) , T&& t) const   \
        ->decltype(op std::forward<T>(t)  )                                             \
        {   return op std::forward<T>(t); }

MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(    "++"_charpack,  ++  )
MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(    "--"_charpack,  --  )
MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(     "*"_charpack,   *  )
MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(     "&"_charpack,   &  )
        template<typename T
                , typename Libs
                , typename Self
                , typename LibToForward >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "&"_charpack       ) , T& t) const
        ->decltype(&t) // TODO: is this redundant? Two unary '&' overloads?
        {   return &t; }



        /* length
         * ======
         *  The length of a constant-expression string literal
         */
        template< typename LibToForward
                , typename Libs
                , typename Self
                , char ...c>
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "length"_charpack )
                            , cambda_utils::char_pack<c...>
                            ) const
        -> std::integral_constant<int, sizeof...(c)>
        { return {}; }


        /* ++
         * ==
         *  Concatentate two compile-time strings
         */
        template< typename LibToForward
                , typename Libs
                , typename Self
                , char ...l
                , char ...r>
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "++"_charpack )
                            , cambda_utils::char_pack<l...>
                            , cambda_utils::char_pack<r...>
                            ) const
        { return cambda_utils::char_pack<l..., r...>{}; }


        template< typename Libs
                , typename LibToForward
                , typename MultiStatement
                , typename ...BindingName>
        struct lambda_capturing_struct
        {
            static_assert(is_valid_tuple_of_libs_v<Libs> ,"");

            Libs libs; // this might store a copy of some of the libs
            LibToForward m_lib; // may be an &-ref  (in fact, in tests so far, it always is
            // in fact, we should probably treat m_lib as an &-ref always, even if
            // it isn't, in order to allow multi-call lambdas
            //static_assert( std::is_reference<LibToForward>{} ,"");

            //static_assert(is_valid_tuple_of_libs_v<decltype(m_libs)> ,"");

            template< typename ...T
                    , typename NewLibs =
                        decltype(std::tuple_cat ( libs , cambda_utils::my_forward_as_tuple( char_pack__to__binding_name(BindingName{}) = std::declval<T>() ...)))
                    >
            constexpr auto
            operator() (T && ... x) &
            ->decltype(multi_statement_execution< MultiStatement >
                        :: eval(std::declval<NewLibs&>(), cambda::combine_libraries(m_lib, char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...)) )
            {
                static_assert(sizeof...(x) == sizeof...(BindingName) ,"");

                auto new_libs = std::tuple_cat ( libs , cambda_utils::my_forward_as_tuple( char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...));
                static_assert(std::is_same<decltype((new_libs)), NewLibs&>{} ,"");

                return multi_statement_execution< MultiStatement >
                        :: eval(new_libs, cambda::combine_libraries(m_lib, char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...));

            };

            template< typename ...T
                    , typename NewLibs =
                        decltype(std::tuple_cat ( libs , cambda_utils::my_forward_as_tuple( char_pack__to__binding_name(BindingName{}) = std::declval<T>() ...)))
                    >
            constexpr auto
            operator() (T && ... x) const &
            ->decltype(multi_statement_execution<MultiStatement>
                            ::eval(std::declval<NewLibs&>(), cambda::combine_libraries   (m_lib, char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...))   )
            {
                static_assert(sizeof...(x) == sizeof...(BindingName) ,"");

                auto new_libs = std::tuple_cat ( libs , cambda_utils::my_forward_as_tuple( char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...));
                static_assert(std::is_same<decltype((new_libs)), NewLibs&>{} ,"");

                return  multi_statement_execution<MultiStatement>
                            ::eval(new_libs, cambda::combine_libraries   (m_lib, char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...))   ;

            };
        };

        template< typename ...BindingName
                , typename Self
                , typename Libs
                , typename LibToForward
                , typename ...QuotedExpression
                , class...
                , typename Lnonref = std::remove_reference_t<LibToForward>
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "lambda"_charpack )
                                    , cambda::grouped_t<'[', types_t<BindingName...>>
                                    , cambda::grouped_t<'[', types_t<QuotedExpression...>>
                                    ) const
        ->         lambda_capturing_struct<Libs, LibToForward, types_t<QuotedExpression...>, BindingName...>
        {
            return lambda_capturing_struct<Libs, LibToForward, types_t<QuotedExpression...>, BindingName...> {libs, std::forward<LibToForward>(l2f)};
        }


        /* assign   (also simply called '=')
         * ======
         */

        template< typename T
                , typename Self
                , typename Libs
                , typename LibToForward
                , typename S >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "assign"_charpack )
                            , T &  target
                            , S && source
                            ) const
        ->decltype(target = std::forward<S>(source)  )
        {   return target = std::forward<S>(source); }
        template< typename T
                , typename Self
                , typename Libs
                , typename LibToForward
                , typename S >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "="_charpack )
                            , T &  target
                            , S && source
                            ) const
        ->decltype(target = std::forward<S>(source)  )
        {   return target = std::forward<S>(source); }


        /* &
         * =
         *
         *  Simply apply a function to a value. This allows a 'pipe-like'
         *  behaviour:
         *      { d & f }   is the same as  (f d)
         */
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename Arg
                , typename Func
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward &&, decltype( "&"_charpack )
                            , Arg && arg
                            , Func && func
                            ) const
        ->decltype(std::forward<Func>(func)(std::forward<Arg>(arg))  )
        {   return std::forward<Func>(func)(std::forward<Arg>(arg)); }


        /*
         * begin
         * =====
         *  Evaluate each term, returning the last one. The name 'begin' might
         *  seem a bit strange, but it is common in Lisp-like languages
         *  (e.g. http://docs.racket-lang.org/guide/begin.html)
         *
         *  This also allows binding via ([] [name] value)
         */
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... Statements
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward && lib, decltype( "begin"_charpack )
                , cambda::grouped_t<'[', types_t<Statements...>>
                ) const
        ->decltype(multi_statement_execution<types_t<Statements...>>::eval(std::forward<LibToForward>(lib)))
        {
            return multi_statement_execution<types_t<Statements...>>::eval(std::forward<LibToForward>(lib));
        }



        /*
         * truec and falsec
         */

        template<typename Self, char ... c>
        auto constexpr static
        static_get_simple_named_value  ( Self &&, decltype( "truec"_charpack ) )
        ->  std::true_type
        {   return {}; }

        template<typename Self, char ... c>
        auto constexpr static
        static_get_simple_named_value  ( Self &&,decltype( "falsec"_charpack ) )
        ->  std::false_type
        {   return {}; }


        /*
         * 'if'
         *  Needs three overloads. The first two take 'true_type' and 'false_type'
         *  and are therefore like 'constexpr if' in C++; in particular, the
         *  two operaands do not need to be of the same type
         *
         *  The third overload is a simple boolean overload
         */

        // 'if' with true
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... QuotedExpressionTrue
                , typename ... QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "if.constexpr"_charpack )
                            , std::true_type
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue...>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse...>>
                            ) const
        ->decltype(multi_statement_execution<types_t<QuotedExpressionTrue...>>
                    :: eval(libs,   std::forward<LibToForward>(l2f) )    )
        {
            return multi_statement_execution<types_t<QuotedExpressionTrue...>>
                    :: eval(libs,   std::forward<LibToForward>(l2f) );
        }

        // 'if' with false
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... QuotedExpressionTrue
                , typename ... QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "if.constexpr"_charpack )
                            , std::false_type
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue...>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse...>>
                            ) const
        ->decltype(multi_statement_execution<types_t<QuotedExpressionFalse...>>
                    ::eval(libs, std::forward<LibToForward>(l2f) )   )
        {
            return multi_statement_execution<types_t<QuotedExpressionFalse...>>
                    ::eval(libs, std::forward<LibToForward>(l2f) );
        }

        // 'if' with bool
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... QuotedExpressionTrue
                , typename ... QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "if"_charpack )
                            , bool b
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue...>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse...>>
                            ) const
        ->decltype(b    ?   multi_statement_execution<types_t<QuotedExpressionTrue...>>  ::eval(libs,   std::forward<LibToForward>(l2f) )
                        :   multi_statement_execution<types_t<QuotedExpressionFalse...>> ::eval(libs,   std::forward<LibToForward>(l2f) ))
        {
                if(b)
                    return multi_statement_execution<types_t<QuotedExpressionTrue...>>  ::eval(libs,   std::forward<LibToForward>(l2f) );
                else
                    return multi_statement_execution<types_t<QuotedExpressionFalse...>> ::eval(libs,   std::forward<LibToForward>(l2f) );
        }

        // 'if' with bool and just one branch
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... QuotedExpressionTrue
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "if"_charpack )
                            , bool b
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue...>>
                            ) const
        -> nil_t
        {
                if(b)
                    multi_statement_execution<types_t<QuotedExpressionTrue...>>  ::eval(libs,   std::forward<LibToForward>(l2f) );
                return {};
        }

        /* while
         */
        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ... QuotedExpressionCondition
                , typename ... QuotedExpressionBody
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs & libs, LibToForward && l2f, decltype( "while"_charpack )
                            , cambda::grouped_t<'[', types_t<QuotedExpressionCondition...>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionBody...>>
                            ) const
        ->void
        {
            while(  multi_statement_execution<types_t<QuotedExpressionCondition...>> :: eval(libs, std::forward<LibToForward>(l2f)))
            {
                    multi_statement_execution<types_t<QuotedExpressionBody...>> :: eval(libs, std::forward<LibToForward>(l2f));
            }
        }


        /* fix
         * ===
         *  Important for recursion. I had a lot of trouble getting this to work
         *  with older clang as it wasn't good at 'decltype' in the presence
         *  of recursion. Hence, We require that the return type be specified.
         */

        template< typename LibToForward
                , typename Self
                , typename Libs
                , typename ReturnType
                , typename F
                , typename ... D
                >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward && , decltype( "fix"_charpack )
                            , ::cambda_utils::type_t<ReturnType>
                            , F && f
                            , D && ... d
                            ) const
        //->decltype( std::declval<fix_holder<F> &>() (std::forward<D>(d) ...))
        ->decltype(auto)
        {
            struct fix_holder
            {
                F & m_f;

                constexpr
                fix_holder(F& f) : m_f(f)   {}

                constexpr auto
                operator() (D    ... t)
                ->ReturnType
                { return exec(m_f, *this,   std::forward<D>(t)...); }

                // This ('exec') must be defined after 'operator()';
                // no idea why, but older clang (3.8) hangs in constexpr otherwise
                constexpr static auto
                exec(F& f, fix_holder &fh, D && ... x)
                ->ReturnType
                {
                    return f(fh, std::forward<D>(x) ...);
                }
            };

            fix_holder fh{f};
            return fh(std::forward<D>(d) ...);
        }


        /* std::{begin,end}
         * ================
         */
        template< typename Self, typename Libs, typename LibToForward , typename ... T >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward && , decltype( "std::begin"_charpack ) , T && ... t) const
        ->decltype(std::begin(std::forward<T>(t) ...) )
        {
            return std::begin(std::forward<T>(t) ...);
        }

        template< typename Self, typename Libs, typename LibToForward , typename ... T >
        auto constexpr
        apply_after_simplification  (Self &&, Libs &, LibToForward && , decltype( "std::end"_charpack ) , T && ... t) const
        ->decltype(std::end(std::forward<T>(t) ...) )
        {
            return std::end(std::forward<T>(t) ...);
        }

    };

    struct empty {
        int ignore;
        constexpr empty() : ignore(0) {}
    };

    constexpr starter_lib starter_lib_v;
    constexpr empty empty_v;

    template<>
    struct is_valid_member_of_a_tuple_of_libs<starter_lib const &>
    { constexpr static bool value = true; };

    template<>
    struct is_valid_member_of_a_tuple_of_libs<empty const &>
    { constexpr static bool value = true; };

    template<typename T, T ... chars>
    constexpr auto
    operator"" _cambda ()
    {
        auto ast = ::cambda::parsing::parse_ast(cambda_utils:: char_pack<chars...>{});

        constexpr auto libs_tuple = cambda_utils::my_forward_as_tuple(empty_v, starter_lib_v);
        static_assert(is_valid_tuple_of_libs_v<decltype(libs_tuple)> ,"");

        return ::cambda::make_cambda_object_from_the_string_literal(libs_tuple, ast, starter_lib_v);
    }

    template<typename T, T ... chars>
    constexpr auto
    operator"" _cambda_empty_library ()
    {
        auto ast = ::cambda::parsing::parse_ast(cambda_utils:: char_pack<chars...>{});

        constexpr auto libs_tuple = cambda_utils::my_forward_as_tuple(empty_v);
        static_assert(is_valid_tuple_of_libs_v<decltype(libs_tuple)> ,"");

        return ::cambda::make_cambda_object_from_the_string_literal(libs_tuple, ast, empty_v);
    }

}
