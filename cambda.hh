/*
 * cambda - Write lambdas in C++ within an embedded Lisp-like language. Header-only. Very 'constexpr'-friendly. The c in cambda stands for constexpr.
 *
 * https://github.com/aaronmcdaid/cambda
 */

#include<utility>
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
    /*
    template<typename ...C>
    constexpr static size_t find_first_of(C ... targets) {
        for(size_t i=0; i<   1+ size(); ++i) {
            if( !utils:: and_all( at(i) != targets ... ))
                return i;
        }
        return -1;
    }
    template<size_t l>
    constexpr static auto   substr(void) {
        return
        make_a_pack_and_apply_it<l, size_t>([](auto ... idxs) {
            return utils:: char_pack< char_pack:: at(idxs) ... >{};
        });
    }
    template<size_t b, size_t e>
    constexpr static auto   substr(void) {
        static_assert( e>=b ,"");
        return
        make_a_pack_and_apply_it<e-b, size_t>([](auto ... idxs) {
            return utils:: char_pack< char_pack:: at(b+idxs) ... >{};
        });
    }
    */
};
template< typename Stream, char ... c>
Stream &
operator<<(Stream &o, cambda_utils::char_pack<c...> s)
{
    o << s.c_str0();
    return o;
}

template<char ... chars>
constexpr char   char_pack<chars...>:: c_str0_[];

constexpr
int char_pack_to_int(cambda_utils::char_pack<>, int prefix_already_processed)
{ return prefix_already_processed; }

// this is to deal with literals such as '15c', which should be returned as
// std::integral_constant<int, 15>{} from the simplifier
constexpr
int char_pack_to_int(cambda_utils::char_pack<'c'>, int prefix_already_processed)
{ return prefix_already_processed; }

template<char one_digit>
constexpr
double char_pack_to_int(cambda_utils::char_pack<'.', one_digit>, int prefix_already_processed)
{
    return prefix_already_processed + 0.1 * (one_digit-'0');
}

template<char digit1, char digit2, char ... digits>
constexpr
double char_pack_to_int(cambda_utils::char_pack<'.', digit1, digit2, digits...>, int prefix_already_processed)
{
    return prefix_already_processed
                + 0.1 * (digit1-'0')
                + 0.1 * char_pack_to_int(cambda_utils::char_pack<'.', digit2, digits...>{}, 0)
                ;
}

template<char next_digit
        , char ... c
        , typename = std::enable_if_t< (next_digit >= '0' && next_digit <= '9') >
        >
constexpr auto
char_pack_to_int(cambda_utils::char_pack<next_digit, c...>, int prefix_already_processed = 0)
//-> std::enable_if_t< (next_digit >= '0' && next_digit <= '9'),double>
{
    static_assert( next_digit >= '0' && next_digit <= '9' ,"");
    return  char_pack_to_int(cambda_utils::char_pack<c...>{}, 10*prefix_already_processed + (next_digit-'0'));
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


namespace impl {
    template<typename ...T>
    struct voider { using type = void; };
}

template<typename ...T>
using void_t = typename impl:: voider<T...>:: type;


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




template< typename T
        , typename Pack >
struct reverse_pack;

template< typename T
        , template<T...> class tmplt >
struct reverse_pack<T, tmplt<> >
{ using type = tmplt<>; };

template< typename T
        , template<T...> class tmplt
        , T first
        , T ... c>
struct reverse_pack<T, tmplt<first,c...>>
{
    using tail_reversed = typename reverse_pack<T, tmplt<c...> > :: type;
    using type = typename cambda_utils:: concat_nontype_pack  < T, tail_reversed , tmplt<first> >::type;
};



template<char ... cs>
auto constexpr
operator==( cambda_utils:: char_pack<cs...>
          , cambda_utils:: char_pack<cs...> )
-> std::true_type
{ return {}; }

template<char ... c1, char ... c2>
auto constexpr
operator==( cambda_utils:: char_pack<c1...>
          , cambda_utils:: char_pack<c2...> )
-> std::false_type
{ return {}; }


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

template< typename T
        , size_t N1, size_t N2
        >
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
        , typename = void /* this is pointless, just to allow me to make a very general specialization */
        , size_t offset = 0> // add this to every entry
struct scalable_make_index_sequence_helper;

template<size_t offset>
struct scalable_make_index_sequence_helper<0, void, offset>
{ using type = std::index_sequence<>; };

template<size_t offset>
struct scalable_make_index_sequence_helper<1, void, offset>
{ using type = std::index_sequence<offset+0>; };

template<size_t offset>
struct scalable_make_index_sequence_helper<2, void, offset>
{ using type = std::index_sequence<offset+0, offset+1>; };

template< size_t ... Ls
        , size_t ... Rs >
auto
concat_index_packs  (   std::index_sequence<Ls...>
                    ,   std::index_sequence<Rs...>  )
-> std::index_sequence<Ls..., Rs...>
{ return {}; }

template<size_t N, size_t offset>
struct scalable_make_index_sequence_helper<N, void, offset>
{
    static_assert(N > 2, "");
    constexpr static size_t mid = N/2;
    using left = typename
        scalable_make_index_sequence_helper<   mid,void,offset    > :: type;
    using right_shifted = typename
        scalable_make_index_sequence_helper< N-mid,void,offset+mid> :: type;
    using type = decltype(concat_index_packs(left{}, right_shifted{}));

};
} // namespace detail

template<size_t N>
using scalable_make_index_sequence = typename detail:: scalable_make_index_sequence_helper<N> :: type;

static_assert(std::is_same< scalable_make_index_sequence<0> , std::make_index_sequence<0> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<1> , std::make_index_sequence<1> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<2> , std::make_index_sequence<2> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<3> , std::make_index_sequence<3> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<4> , std::make_index_sequence<4> >{} ,"");
static_assert(std::is_same< scalable_make_index_sequence<200> , std::make_index_sequence<200> >{} ,"");




} // namespace cambda_utils

namespace cambda {
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
    find_next_token(C, size_t o)
    ->std::pair<size_t,size_t>
    {
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

        // finally, we just have a string of non-special characters.
        // We must read them all in
        while (C::at(o) != '\0' && !is_whitespace(C::at(o)) && !is_grouper(C::at(o)))
            ++o;
        return std:: make_pair(start, o);
    }

    template<typename T, T ... chars>
    constexpr auto
    operator"" _charpack ()
    -> cambda_utils:: char_pack<chars...>
    { return {}; }

    template<char c>    using c_char    = std::integral_constant<char, c>;
    template<int c>     using c_int     = std::integral_constant<int, c>;
    template<size_t c>  using c_sizet   = std::integral_constant<size_t, c>;

    // now, a 'type_t' type template for the 'typeof' function in cambda
    template<typename T>
    struct type_t {
        using type = T;
    };

    template<typename ... T>
    struct types_t {
        template<typename Prepend>
        using prepend = types_t<Prepend, T...>;

        constexpr static size_t size = sizeof...(T);
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
    toString( cambda_utils::char_pack<c...> s, int indent = 0)
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

    // while '('/'{'/'[' open the list. This is more complex:
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
    struct flatten_again
    {
        constexpr flatten_again(){}

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

    template<typename E>
    auto constexpr
    parse_ast(E )
    {
        //using all_the_terms_t = typename parse_flat_list_of_terms<E, 0>::all_the_terms; /*This was the broken version - clang doesn't like this*/
        using all_the_terms_t = typename flatten_again<E>::all_the_terms_t;

        static_assert(std::is_same<all_the_terms_t, typename flatten_again<E>::all_the_terms_t>{} ,"");

        auto parsed =  parser( all_the_terms_t{} );
        static_assert(std::is_same< typename decltype(parsed) :: rest , types_t<>>{} ,"");
        using x = typename should_be_just_one_term< typename decltype(parsed) :: me > :: single_type;
        return x{};
    }


    template< typename Lib
            , char ... letters
            , typename ...T >
    auto constexpr
    apply_after_simplification (Lib && lib, cambda_utils::char_pack<letters...> name, T && ...t)
    ->decltype(std::forward<Lib>(lib).apply_after_simplification(std::forward<Lib>(lib), name, std::forward<T>(t)...)  )
    {   return std::forward<Lib>(lib).apply_after_simplification(std::forward<Lib>(lib), name, std::forward<T>(t)...); }

    template< typename Lib1
            , typename Lib2 >
    struct library_combiner : public Lib1, public Lib2
    {

        template< typename T
                , typename U
                >
        constexpr
        library_combiner(T&&t, U&&u)
            : Lib1(std::forward<T>(t))
            , Lib2(std::forward<U>(u))
        {}


        /* First, 'apply_after_simplification'
         * Define two helper overloads, one for each sub-library.
         * Then forward the call
         */
        template< typename ... T
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification( T && ... t)
        ->decltype(id{}(this)->Lib1::apply_after_simplification(std::forward<T>(t)...)  )
        {   return     (this)->Lib1::apply_after_simplification(std::forward<T>(t)...); }

        template< typename ... T
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        apply_after_simplification(T && ... t)
        ->decltype(id{}(this)->Lib2::apply_after_simplification(std::forward<T>(t)...)  )
        {   return     (this)->Lib2::apply_after_simplification(std::forward<T>(t)...); }

        /* Second, 'get_simple_named_value'
         * Define two helper overloads, one for each sub-library.
         * Then forward the call
         */
        template< char ... cs
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        get_simple_named_value( cambda_utils::char_pack<cs...> name)
        ->decltype(id{}(this)->Lib1::get_simple_named_value(name)  )
        {   return     (this)->Lib1::get_simple_named_value(name); }

        template< char ... cs
                , typename id = cambda_utils::id_t
                , typename std::integral_constant<int, __LINE__> * =nullptr
                >
        auto constexpr
        get_simple_named_value( cambda_utils::char_pack<cs...> name)
        ->decltype(id{}(this)->Lib2::get_simple_named_value(name)  )
        {   return     (this)->Lib2::get_simple_named_value(name); }
    };


    template< typename Lib1
            , typename Lib2
            >
    constexpr auto
    combine_libraries(Lib1 lib1, Lib2 lib2)
    -> library_combiner<Lib1,Lib2>
    { return {std::move(lib1),std::move(lib2)}; }


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



    template<typename T, typename Lib>
    constexpr auto
    call_the_simplifier(T t, Lib && l)
    ->decltype(simplifier<T>::simplify(t, std::forward<Lib>(l))  )
    {   return simplifier<T>::simplify(t, std::forward<Lib>(l)); }



    // simplifier for all digits
    template<char first_digit, char ...c>
    struct simplifier   < cambda_utils::char_pack<first_digit, c...>
                        , cambda_utils::void_t<std::enable_if_t<
                            is_digit_constexpr(first_digit)
                            && is_digit_constexpr(cambda_utils::char_pack<first_digit, c...> :: last())
                          >>>
    {
        template<typename L>
        static auto constexpr
        simplify(cambda_utils::char_pack<first_digit, c...> digits, L && )
        { return cambda_utils::char_pack_to_int(digits); }
    };

    // simplifier for digits with trailing 'c', for an integral constant
    template<char first_digit, char ...c>
    struct simplifier   < cambda_utils::char_pack<first_digit, c...>
                        , cambda_utils::void_t<std::enable_if_t<
                            is_digit_constexpr(first_digit)
                            && cambda_utils::char_pack<first_digit, c...> :: last() == 'c'
                          >>>
    {
        template<typename L>
        static auto constexpr
        simplify(cambda_utils::char_pack<first_digit, c...> , L &&)
        { return std::integral_constant<int, cambda_utils::char_pack_to_int(cambda_utils::char_pack<first_digit, c...>{})>{}; }
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
        template<typename L>
        static auto constexpr
        simplify(StringLiteral sl, L &&)
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
        template<typename L>
        static auto constexpr
        simplify(StringLiteral sl, L &&)
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
        /* has_get_simple_named_value */


        template< typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value_helper(cambda_utils::priority_tag<2>)
        -> decltype(void(
                        std::declval<Lib&>().
                    get_simple_named_value(
                        std::declval<Name&>())
                    )
                ,   std:: true_type{}
                )
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value_helper(cambda_utils::priority_tag<1>)
        -> std:: false_type
        { return {}; }

        template<typename Name, typename Lib>
        static auto constexpr
        has_get_simple_named_value()
        -> decltype(auto)
        { return detail:: has_get_simple_named_value_helper<Name,Lib>(cambda_utils::priority_tag<9>{}); }

    }

    // simplifier for names.
    // Two overloads, one for where 'get_simple_named_value' is present, and one
    // to capture-and-store-and-forward to 'apply_after_simplification' later
    template<typename Name>
    struct simplifier   < Name
                        , cambda_utils::void_t<std::enable_if_t<
                                !( is_digit_constexpr(Name::at(0)) )
                             && !( '\'' ==            Name::at(0)  )
                          >>>
    {
        static_assert(!is_grouper(Name::at(0))          ,"");
        static_assert(!is_digit_constexpr(Name::at(0))  ,"");
        static_assert( '\'' !=            Name::at(0)   ,"");

        template<typename L
                , bool b = detail::has_get_simple_named_value<Name, L>()
                , typename = std::enable_if_t<b>
                >
        static auto constexpr
        simplify(Name name, L && lib)
        ->decltype(std::forward<L>(lib).get_simple_named_value(name) )
        {
            return std::forward<L>(lib).get_simple_named_value(name);
        }

        template<typename L>
        struct gather_args_later
        {
            Name m_f;
            L m_lib;

            template<typename ...T>
            auto constexpr
            operator()  ( T && ...t)
            ->decltype( cambda::apply_after_simplification(m_lib, m_f , std::forward<T>(t) ... )  )
            {   return  cambda::apply_after_simplification(m_lib, m_f , std::forward<T>(t) ... ); }
        };

        template<typename L
                , bool b = detail::has_get_simple_named_value<Name, L>()
                , typename = std::enable_if_t<!b>
                >
        static auto constexpr
        simplify(Name f, L && lib) -> gather_args_later<L> {
            return {std::move(f), std::forward<L>(lib)};
        }
    };


    // simplifier to apply '('
    template<typename Func, typename ...Args>
    struct simplifier   < grouped_t<'(', types_t<Func, Args...   >>
                        >
    {
        template<typename id = cambda_utils::id_t
                , typename L >
        static auto constexpr
        simplify(grouped_t<'(', types_t<Func, Args...> >, L && lib)
        ->decltype(call_the_simplifier(Func{}, id{}(std::forward<L>(lib))) ( call_the_simplifier(Args{}, id{}(std::forward<L>(lib)))...)  )
        {
            return call_the_simplifier(Func{},      std::forward<L>(lib) ) ( call_the_simplifier(Args{},      std::forward<L>(lib)) ...);
        }
    };


    // simplifier to apply '[', i.e. just quote it
    template<typename ...Args>
    struct simplifier   < grouped_t<'[', types_t<Args...   >>
                        >
    {
        template<typename L>
        static auto constexpr
        simplify(grouped_t<'[', types_t<Args...> > quoted, L &&)
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
                , typename L
                , class ...
                > // we must perfect forward here to avoid a bug in older gcc (5.5.0) where the while loop wasn't repeating
        static auto constexpr
        simplify(grouped_t<'{', types_t<Arg1, Func, Arg2> >, L && lib)
        ->decltype(simplifier       < grouped_t<'(', types_t<Func, Arg1, Arg2>> >
                        :: simplify ( grouped_t<'(', types_t<Func, Arg1, Arg2>> {} , id{}(std::forward<L>(lib)))
                )
        {
            return simplifier       < grouped_t<'(', types_t<Func, Arg1, Arg2>> >
                        :: simplify ( grouped_t<'(', types_t<Func, Arg1, Arg2>> {} ,      std::forward<L>(lib) )
                ;
        }
    };

    // simplifier for () - simply returns nil_t{}
    template<>
    struct simplifier   < grouped_t<'(', types_t<>   > >
    {
        template<typename L>
        static auto constexpr
        simplify(grouped_t<'(', types_t<> >, L &&)
        -> nil_t
        { return {}; }
    };

    template<typename AST, typename Lib>
    auto constexpr
    simplify(AST ast, Lib && l)
    ->decltype(call_the_simplifier(ast, std::forward<Lib>(l))  )
    {   return call_the_simplifier(ast, std::forward<Lib>(l)); }

    template<typename T, char ...c>
    struct binded_name_with_valueOrReference
    {
        // NOTE: T might be a l-reference type
        // However, we always return as l-ref
        T m_x;

        static_assert(!std::is_rvalue_reference<T>{}, "");

        auto constexpr
        get_simple_named_value  ( cambda_utils::char_pack<c...> )
        -> T
        { return m_x; }
    };

    template< typename B
            , typename T2
            , char ...c2 >
    auto constexpr
    operator,   (   B && beforeComma
                ,   binded_name_with_valueOrReference<T2, c2...> afterComma)
    {
        return combine_libraries(std::forward<B>(beforeComma), std::move(afterComma));
    }

    template<char ...c>
    struct binding_name
    {
        template<typename V>
        auto constexpr
        operator=(V && v)
        ->decltype(auto)
        { return binded_name_with_valueOrReference<V, c...>{std::forward<V>(v)}; }
    };
    template<char ...c>
    auto constexpr
    char_pack__to__binding_name(cambda_utils::char_pack<c...>)
    -> binding_name<c...>
    {return {};}

    template<typename T, T ... chars>
    constexpr auto
    operator"" _binding () {
        return binding_name<chars...>{};
    }

    template<typename AST, typename Lib>
    struct cambda_object_from_the_string_literal
    {
        AST m_ast;
        Lib lib;

        template< typename id = cambda_utils:: id_t>
        constexpr auto
        operator() ()
        -> decltype(auto)
        {
            return ::cambda:: simplify(     m_ast , lib);
        }

        template<typename Binding>
        constexpr auto
        operator[] (Binding && binding_to_insert) && // the '&&' is important, allows us to 'move' from this->lib
        -> decltype(auto)
        {
            return cambda_object_from_the_string_literal<AST, decltype(
                        combine_libraries(std::move(lib), std::forward<Binding>(binding_to_insert))
                    )>
            {m_ast
                        , combine_libraries(std::move(lib), std::forward<Binding>(binding_to_insert))
            };
            //return *this;
        }
    };
    template<typename AST, typename Lib>
    auto constexpr
    make_cambda_object_from_the_string_literal(AST ast, Lib lib)
    -> cambda_object_from_the_string_literal<AST,Lib>
    { return {std::move(ast),std::move(lib)}; }

    struct starter_lib {
        int ignore;
        constexpr starter_lib() : ignore(0) {} // a default constructor, just because clang requires them for constexpr objects


        // range_based_for
        template< typename LibToForward
                , typename Data
                , typename Func
                >
        auto constexpr
        apply_after_simplification  (   LibToForward &&, decltype( "range_based_for"_charpack )
                                    ,   Data && data
                                    ,   Func && func
                                    )
        -> int
        {
            int count = 0;
            for(auto && x : data)
                std::forward<Func>(func) (std::forward<decltype(x)>(x));
            return count;
        }


        // id :: a -> a
        template<typename T
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "id"_charpack )
                            , T&& t)
        -> T
        { return std::forward<T>(t); }

        // typeof
        template<typename T
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "typeof"_charpack )
                            , T&& )
        -> type_t<T>
        { return {}; }

        // ref2val
        template<typename T
                , typename LibToForward
                >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "ref2val"_charpack )
                            , T& t)
        -> T
        { return t; }

#define MACRO_FOR_SIMPLE_BINARY_INFIX_OPERATION(op_name, infix_op) \
        template< typename LibToForward , typename Ti , typename Tj >   \
        auto constexpr                                                  \
        apply_after_simplification ( LibToForward &&                    \
            , decltype( op_name )                                          \
            , Ti && i , Tj && j)                                        \
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
                , typename LibToForward >                                               \
        auto constexpr                                                                  \
        apply_after_simplification  (LibToForward &&, decltype( op_name       ) , T&& t)   \
        ->decltype(op std::forward<T>(t)  )                                             \
        {   return op std::forward<T>(t); }

MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(    "++"_charpack,  ++  )
MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(    "--"_charpack,  --  )
MACRO_FOR_SIMPLE_UNARY_PREFIX_OPERATION(     "*"_charpack,   *  )
        template<typename T
                , typename LibToForward >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "&"_charpack       ) , T& t)
        ->decltype(&t)
        {   return &t; }



        template< typename LibToForward
                , char ...c>
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "length"_charpack )
                            , cambda_utils::char_pack<c...>
                            )
        -> std::integral_constant<int, sizeof...(c)>
        { return {}; }


        template< typename LibToForward
                , typename QuotedExpression
                , typename ...BindingName>
        struct lambda_capturing_struct
        {
            LibToForward m_lib;

            template< typename ...T>
            constexpr auto
            operator() (T && ... x) const
            ->decltype(cambda::simplify
                        (   QuotedExpression{}
                        , cambda::combine_libraries   (   m_lib
                                                ,   char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...
                                                )))
            {
                static_assert(sizeof...(x) == sizeof...(BindingName) ,"");
                return cambda::simplify
                        (   QuotedExpression{}
                        , cambda::combine_libraries   (   m_lib
                                                ,   char_pack__to__binding_name(BindingName{}) = std::forward<decltype(x)>(x) ...
                                                ));
            };
        };

        template< typename ...BindingName
                , typename LibToForward
                , typename QuotedExpression>
        auto constexpr
        apply_after_simplification  (LibToForward l2f, decltype( "lambda"_charpack )
                                    , cambda::grouped_t<'[', types_t<BindingName...>>
                                    , cambda::grouped_t<'[', types_t<QuotedExpression>>
                                    )
        ->decltype(lambda_capturing_struct<LibToForward, QuotedExpression, BindingName...> {std::move(l2f)}  )
        {   return lambda_capturing_struct<LibToForward, QuotedExpression, BindingName...> {std::move(l2f)}; }

        template< typename ...BindingName
                , typename LibToForward
                , typename QuotedExpression>
        auto constexpr
        apply_after_simplification  (LibToForward l2f, decltype( "/"_charpack )
                                    , cambda::grouped_t<'[', types_t<BindingName...>>
                                    , cambda::grouped_t<'[', types_t<QuotedExpression>>
                                    )
        ->decltype(lambda_capturing_struct<LibToForward, QuotedExpression, BindingName...> {std::move(l2f)}  )
        {   return lambda_capturing_struct<LibToForward, QuotedExpression, BindingName...> {std::move(l2f)}; }


        template< typename T
                , typename LibToForward
                , typename S >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "assign"_charpack )
                            , T & target
                            , S   source
                            )
        ->decltype(target = source  )
        {   return target = source; }
        template< typename T
                , typename LibToForward
                , typename S >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "="_charpack )
                            , T & target
                            , S   source
                            )
        ->decltype(target = source  )
        {   return target = source; }

        template< typename LibToForward
                , typename Arg
                , typename Func
                >
        auto constexpr
        apply_after_simplification  (LibToForward &&, decltype( "&"_charpack )
                            , Arg && arg
                            , Func && func
                            )
        ->decltype(std::forward<Func>(func)(std::forward<Arg>(arg))  )
        {   return std::forward<Func>(func)(std::forward<Arg>(arg)); }


        /*
         * 'let' - this will be complex
         */
        template< typename LibToForward
                , typename SingleOne
                >
        auto constexpr
        apply_after_simplification  (LibToForward && lib, decltype( "let"_charpack )
                            , cambda::grouped_t<'[', types_t<SingleOne>>
                            )
        ->decltype(auto)
        {
                return cambda::simplify
                        (   SingleOne{}
                            , std::forward<LibToForward>(lib)
                        );
        }

        template< typename LibToForward
                , typename BindingName
                , typename BoundExpression
                , typename ... TheRest
                >
        auto constexpr
        apply_after_simplification  (LibToForward && lib, decltype( "let"_charpack )
                            , cambda::grouped_t<'[', types_t<BindingName, BoundExpression, TheRest...>>
                            )
        ->decltype(auto)
        {
            static_assert(sizeof...(TheRest) % 2 == 1 ,"A 'let' must always have an odd number of arguments");

            decltype(auto) // not-an r-ref. May be l-ref though
                bound_value = cambda::simplify(BoundExpression{}, std::forward<LibToForward>(lib));
            static_assert(!std::is_rvalue_reference<decltype(bound_value)>{} ,"");
            // Note: we treat bound_value as an lvalue from here on, and allow it to be
            // taken as l-reference. This means that 'bound_value' is the storage,
            // assuming storage is required.


            return cambda::simplify
                        (   cambda::grouped_t<'('
                                    , types_t<cambda_utils::char_pack<'l','e','t'>
                                             ,cambda::grouped_t<'[', types_t<
                                                    TheRest...
                                             >>>
                                    >{}
                        ,   cambda::combine_libraries   (   std::forward<LibToForward>(lib)
                                                        ,   char_pack__to__binding_name(BindingName{}) = bound_value)
                        );
        }


        /*
         * 'begin'
         */
        template< typename LibToForward
                , typename SingleOne
                >
        auto constexpr
        apply_after_simplification  (LibToForward && lib, decltype( "begin"_charpack )
                            , cambda::grouped_t<'[', types_t<SingleOne>>
                            )
        ->decltype(auto)
        {
                return cambda::simplify
                        (   SingleOne{}
                        ,   std::forward<LibToForward>(lib)
                        );
        }

        template< typename LibToForward
                , typename A
                , typename B
                , typename ... C
                >
        auto constexpr
        apply_after_simplification  (LibToForward && lib, decltype( "begin"_charpack ) nm
                            , cambda::grouped_t<'[', types_t<A, B, C...>>
                            )
        ->decltype(auto)
        {
                cambda::simplify
                        (   A{}
                        ,   std::forward<LibToForward>(lib)
                        );
                return
                    apply_after_simplification(
                        std::forward<LibToForward>(lib)
                        , nm
                        , cambda::grouped_t<'[', types_t<B, C...>>{}
                        );
        }


        /*
         * truec and falsec
         */

        template<char ... c>
        auto constexpr
        get_simple_named_value  ( decltype( "truec"_charpack ) )
        ->  std::true_type
        {   return {}; }

        template<char ... c>
        auto constexpr
        get_simple_named_value  ( decltype( "falsec"_charpack ) )
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
                , typename QuotedExpressionTrue
                , typename QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (LibToForward && l2f, decltype( "if"_charpack )
                            , std::true_type
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse>>
                            )
        ->decltype(auto)
        {
                return cambda::simplify
                        (   QuotedExpressionTrue{} ,std::forward<LibToForward>(l2f));
        }

        // 'if' with false
        template< typename LibToForward
                , typename QuotedExpressionTrue
                , typename QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (LibToForward && l2f, decltype( "if"_charpack )
                            , std::false_type
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse>>
                            )
        ->decltype(auto)
        {
                return cambda::simplify
                        (   QuotedExpressionFalse{} ,std::forward<LibToForward>(l2f));
        }

        // 'if' with bool
        template< typename LibToForward
                , typename QuotedExpressionTrue
                , typename QuotedExpressionFalse
                >
        auto constexpr
        apply_after_simplification  (LibToForward && l2f, decltype( "if"_charpack )
                            , bool b
                            , cambda::grouped_t<'[', types_t<QuotedExpressionTrue>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionFalse>>
                            )
        ->decltype(b    ? cambda::simplify (   QuotedExpressionTrue{}  ,std::forward<LibToForward>(l2f))
                        : cambda::simplify (   QuotedExpressionFalse{} ,std::forward<LibToForward>(l2f)) )
        {
                if(b)
                    return cambda::simplify (   QuotedExpressionTrue{}  ,std::forward<LibToForward>(l2f));
                else
                    return cambda::simplify (   QuotedExpressionFalse{} ,std::forward<LibToForward>(l2f));
        }


        /* while
         */
        template< typename LibToForward
                , typename QuotedExpressionCondition
                , typename QuotedExpressionBody
                >
        auto constexpr
        apply_after_simplification  (LibToForward && l2f, decltype( "while"_charpack )
                            , cambda::grouped_t<'[', types_t<QuotedExpressionCondition>>
                            , cambda::grouped_t<'[', types_t<QuotedExpressionBody>>
                            )
        ->void
        {
            while(cambda::simplify (    QuotedExpressionCondition{} ,std::forward<LibToForward>(l2f)))
                cambda::simplify   (    QuotedExpressionBody{}      ,std::forward<LibToForward>(l2f));
        }


        /* fix
         * ===
         *  Important for recursion. I had a lot of trouble getting this to work
         *  with older clang as it wasn't good at 'decltype' in the presence
         *  of recursion. Hence, We require that the return type be specified.
         */
        template<typename F, typename ... Args>
        struct fix_holder
        {

            F & m_f;

            constexpr
            fix_holder(F& f) : m_f(f)   {}

            constexpr auto
            operator() (Args && ... t) const
            //->decltype(m_f(*this,   std::forward<Args>(t)...)  )
            ->int
            {   return m_f(*this,   std::forward<Args>(t)...); }
        };

        template< typename LibToForward
                , typename ReturnType
                , typename F
                , typename ... D
                >
        auto constexpr
        apply_after_simplification  (LibToForward && , decltype( "fix"_charpack )
                            , type_t<ReturnType>
                            , F && f
                            , D && ... d
                            )
        //->decltype( std::declval<fix_holder<F> &>() (std::forward<D>(d) ...))
        ->decltype(auto)
        {
            struct fix_holder
            {
                F & m_f;

                constexpr
                fix_holder(F& f) : m_f(f)   {}

                constexpr auto
                operator() (D && ... t)
                ->ReturnType
                {

                    return exec(m_f, *this,   std::forward<D>(t)...);
                }

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


    };


    constexpr starter_lib starter_lib_v;

    template<typename AST>
    auto constexpr
    simplify(AST ast)
    ->decltype(auto)
    {   return simplify(ast, starter_lib_v); }

    template<typename T, T ... chars>
    constexpr auto
    operator"" _cambda ()
    {
        auto ast = parse_ast(cambda_utils:: char_pack<chars...>{});
        return ::cambda::make_cambda_object_from_the_string_literal(ast, starter_lib_v);
    }

    struct empty {
        int ignore;
        constexpr empty() : ignore(0) {}
    };
    template<typename T, T ... chars>
    constexpr auto
    operator"" _cambda_empty_library ()
    {
        auto ast = parse_ast(cambda_utils:: char_pack<chars...>{});
        return ::cambda::make_cambda_object_from_the_string_literal(ast, empty{});
    }

}
