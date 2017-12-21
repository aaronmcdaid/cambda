#include<iostream>
#include"cambda.hh"

using cambda::operator"" _cambda;

constexpr auto a = "15"_cambda();            // a is 15
constexpr auto b = "(+ 8 7)"_cambda();       // Function call. This is addition. b is 15
constexpr auto c = "(* 8 7)"_cambda();       // Multiplication
constexpr auto d = "{8 * 7}"_cambda();       // If there are two args, use {} instead
constexpr auto e = "{ {8 * 7} + {6 * 3} }"_cambda();  // Nested application

static_assert(a == 15   ,"");
static_assert(b == 15   ,"");
static_assert(c == 56   ,"");
static_assert(d == 56   ,"");
static_assert(e == 74   ,"");

namespace CAMBDA_MACRO_support
{
    constexpr
    size_t
    cx_strlen(char const *s)
    {
        size_t length =0;
        while( (*s) != '\0' )
        {
            ++s;
            ++length;
        }
        return length;
    }

    constexpr
    char
    string_at_with_upper_bound(const char *s, size_t i)
    {
        size_t len = cx_strlen(s);
        if(i >= len)
            return '\0';
        return s[i];
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

    template<char ... chars>
    struct temporary_holder_for_pack_of_characters
    {
        static constexpr
        char arr[] = {chars...};

        static constexpr size_t size() { return sizeof...(chars); }

        static_assert(arr[size()-2] == '\0' ,"");

        constexpr static
        size_t
        number_of_non_nulls = cx_strlen(arr);

        //static_assert(number_of_non_nulls == 11 ,"");

        template<size_t ... NonNullIndices>
        auto static constexpr
        extract_all_the_nonnull_characters(std::index_sequence<NonNullIndices...>)
        {
            //static_assert(number_of_non_nulls == 11 ,"");
            static_assert(number_of_non_nulls == sizeof...(NonNullIndices) ,"");
            char nn[sizeof...(NonNullIndices)]{ arr[NonNullIndices]... };
            (void)nn;
            return cambda_utils::char_pack< arr[NonNullIndices]... >{};
        };

        constexpr static
        auto trailing_nulls_removed = extract_all_the_nonnull_characters( scalable_make_index_sequence<number_of_non_nulls>{});

        constexpr static
        auto compiled_CAMBDA_object()
        {
            return ::cambda::make_cambda_object_from_the_string_literal(
                            ::cambda::parse_ast(trailing_nulls_removed)
                        , ::cambda::starter_lib_v);
        }
    };
}

#define TEN_LOOKUPS(string_literal, base_offset) \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+0), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+1), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+2), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+3), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+4), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+5), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+6), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+7), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+8), \
        CAMBDA_MACRO_support::string_at_with_upper_bound(string_literal,base_offset+9),

#define ONE_HUNDRED_LOOKUPS(string_literal, base_offset)     \
        TEN_LOOKUPS(string_literal, base_offset+0)  \
        TEN_LOOKUPS(string_literal, base_offset+10)  \
        TEN_LOOKUPS(string_literal, base_offset+20)  \
        TEN_LOOKUPS(string_literal, base_offset+30)  \
        TEN_LOOKUPS(string_literal, base_offset+40)  \
        TEN_LOOKUPS(string_literal, base_offset+50)  \
        TEN_LOOKUPS(string_literal, base_offset+60)  \
        TEN_LOOKUPS(string_literal, base_offset+70)  \
        TEN_LOOKUPS(string_literal, base_offset+80)  \
        TEN_LOOKUPS(string_literal, base_offset+90)

#define ONE_THOUSAND_LOOKUPS(string_literal, base_offset)     \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+0)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+100)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+200)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+300)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+400)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+500)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+600)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+700)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+800)  \
        ONE_HUNDRED_LOOKUPS(string_literal, base_offset+900)


int main()
{
    constexpr
        auto f =
            CAMBDA_MACRO_support::temporary_holder_for_pack_of_characters < ONE_THOUSAND_LOOKUPS(
R"--({7 + 8})--"
,0) '\0'>{}.compiled_CAMBDA_object()();
    std::cout << f << '\n';
    // We do nothing here, this is just to be compiled to confirm that static_assert's worked

    static_assert(CAMBDA_MACRO_support::temporary_holder_for_pack_of_characters < ONE_THOUSAND_LOOKUPS(
R"--({7 + 8})--"
,0) '\0'>{}.compiled_CAMBDA_object()() == 15 ,"");
}
