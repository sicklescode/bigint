#ifndef NUMBERS_BIGINT_HH
#define NUMBERS_BIGINT_HH

#include "elementary.hpp"
#include "radix.hpp"

#include <array>
#include <cstdint>
#include <limits>
#include <type_traits>

namespace numbers {
namespace impl {

template<typename T>
struct bigint_datatype {
    typedef void overflow_type;
};

template<>
struct bigint_datatype<std::uint8_t> {
    typedef std::uint16_t overflow_type;
};

template<>
struct bigint_datatype<std::uint16_t> {
    typedef std::uint32_t overflow_type;
};

template<>
struct bigint_datatype<std::uint32_t> {
    typedef std::uint64_t overflow_type;
};

template<typename T, std::size_t BITS>
struct required_elements : identity<
    std::size_t,
    (BITS / std::numeric_limits<T>::digits) + (BITS % std::numeric_limits<T>::digits == 0 ? 0 : 1)
> {};

template<typename T, std::size_t A_BITS, std::size_t B_BITS>
struct required_bits_addition : identity<std::size_t, std::max(A_BITS, B_BITS) + 1> {};

template<typename T, std::size_t A_BITS, std::size_t B_BITS>
struct required_bits_multiplication : identity<std::size_t, A_BITS + B_BITS> {};

template<typename T>
struct default_bits : identity<T, 256> {};

/*
 * required_bits_radix (Generic)
 */
template<std::size_t RADIX, std::size_t DIGITS>
struct required_bits_radix :
    identity<std::size_t, 0>
{};

/*
 * required_bits_radix (Base 8)
 */
template<std::size_t DIGITS>
struct required_bits_radix<8, DIGITS> :
    identity<std::size_t, DIGITS * 3>
{};

/*
 * required_bits_radix (Base 10)
 */
template<std::size_t DIGITS>
struct required_bits_radix<10, DIGITS> :
    identity<
        std::size_t,
        1 + static_cast<std::size_t>(
            static_cast<double>(DIGITS) *
            3.32192809488736234787
        )
    >
{};

/*
 * required_bits_radix (Base 16)
 */
template<std::size_t DIGITS>
struct required_bits_radix<16, DIGITS> :
    identity<std::size_t, DIGITS * 4>
{};

/*
 * required_bits_radix (Base 64)
 */
template<std::size_t DIGITS>
struct required_bits_radix<64, DIGITS> :
    identity<std::size_t, DIGITS * 6>
{};

} //namespace impl

/*
 * bigint
 */
template<
    std::size_t BITS = impl::default_bits<std::size_t>::value,
    typename T = std::uint32_t,
    typename = std::enable_if_t<
        std::is_integral<T>::value
        && !std::is_signed<T>::value
        && (BITS > 0)
    >
>
struct bigint :
    std::array<T, impl::required_elements<T, BITS>::value>
{
    static constexpr std::size_t bits = BITS;
    static constexpr std::size_t count = impl::required_elements<T, BITS>::value;

    constexpr std::size_t highest_bit() const {
        std::size_t i = count - 1;
        while (i > 0 && this->at(i) == 0) {
            i--;
        }

        std::size_t j = std::numeric_limits<T>::digits;
        while (j > 0 && (this->at(i) & (0x01 << (j - 1))) == 0) {
            j--;
        }
        return i * std::numeric_limits<T>::digits + j;
    }
};

/*
 * add
 */
template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr auto add(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    constexpr std::size_t BITS = impl::required_bits_addition<T, A_BITS, B_BITS>::value;
    std::array<T, impl::required_elements<T, BITS>::value> result = {};
    T carry = { zero<T>::value };
    for (std::size_t i = 0; i < result.size() - 1; i++) {
        T ai = i < a.count ? a[i] : zero<T>::value;
        T bi = i < b.count ? b[i] : zero<T>::value;
        T current = ai + bi;
        T partial = current + carry;
        carry = (current < ai) || (partial < current) ? one<T>::value : zero<T>::value;
        result[i] = partial;
    }
    result[result.size() - 1] = carry;
    return bigint<BITS, T>{ result };
}

/*
 * add_in_place
 */
template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr auto add_in_place(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    constexpr std::size_t BITS = std::max(A_BITS, B_BITS);
    std::array<T, impl::required_elements<T, BITS>::value> result = {};
    T carry = { zero<T>::value };
    for (std::size_t i = 0; i < result.size(); i++) {
        T ai = i < a.count ? a[i] : zero<T>::value;
        T bi = i < b.count ? b[i] : zero<T>::value;
        T current = ai + bi;
        T partial = current + carry;
        carry = (current < ai) || (partial < current) ? one<T>::value : zero<T>::value;
        result[i] = partial;
    }
    return bigint<BITS, T>{ result };
}

/*
 * multiply
 */
template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr auto multiply(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    using OverflowType = typename impl::bigint_datatype<T>::overflow_type;
    bigint<impl::required_bits_multiplication<T, A_BITS, B_BITS>::value, T> result = { zero<T>::value };
    for (auto bi = 0; bi < b.count; ++bi) {
        T k = 0;
        const auto count = std::min(a.count, result.count - bi);
        for (auto ai = 0; ai < count; ++ai) {
            OverflowType product = multiply(
            static_cast<OverflowType>(a[ai]),
            static_cast<OverflowType>(b[bi]));
            OverflowType partial = add(
            static_cast<OverflowType>(result[ai + bi]),
            static_cast<OverflowType>(k));
            OverflowType sum = add(partial, product);
            result[ai + bi] = static_cast<T>(sum);
            k = sum >> static_cast<T>(std::numeric_limits<T>::digits);
        }
        if (bi + count < result.count) {
            result[bi + count] = k;
        }
    }
    return result;
}

/*
 * multiply_in_place
 */
template<typename T, std::size_t MAX_BITS, std::size_t OTHER_BITS>
constexpr auto multiply_in_place(bigint<MAX_BITS, T> a, bigint<OTHER_BITS, T> b) {
    using OverflowType = typename impl::bigint_datatype<T>::overflow_type;
    bigint<MAX_BITS, T> result = { zero<T>::value };
    for (auto bi = 0; bi < b.count; ++bi) {
        T k = 0;
        const auto count = std::min(a.count, result.count - bi);
        for (auto ai = 0; ai < count; ++ai) {
            OverflowType product = multiply(
            static_cast<OverflowType>(a[ai]),
            static_cast<OverflowType>(b[bi]));
            OverflowType partial = add(
            static_cast<OverflowType>(result[ai + bi]),
            static_cast<OverflowType>(k));
            OverflowType sum = add(partial, product);
            result[ai + bi] = static_cast<T>(sum);
            k = sum >> static_cast<T>(std::numeric_limits<T>::digits);
        }
        if (bi + count < result.count) {
            result[bi + count] = k;
        }
    }
    return result;
}

/*
 * '+' (Addition Operators)
 */
template<std::size_t BITS, typename T>
constexpr auto operator+(bigint<BITS, T> a, T b) {
    bigint<std::numeric_limits<T>::digits, T> bb = {b };
    return add(a, bb);
}

template<std::size_t BITS, typename T>
constexpr auto operator+(bigint<BITS, T> a, bigint<BITS, T> b) {
    return add(a, b);
}

/*
 * '*' (Multiplication Operators)
 */
template<std::size_t BITS, typename T>
constexpr auto operator*(bigint<BITS, T> a, T b) {
    bigint<std::numeric_limits<T>::digits, T> bb = {b };
    return multiply(a, bb);
}

template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr auto operator*(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    return multiply(a, b);
}

/*
 * ++ (Increment Operator)
 */
// TODO

/*
 * zero
 */
template<std::size_t BITS, typename V>
struct zero<bigint<BITS, V>> {
    static constexpr auto value = bigint<BITS, V>{ zero<V>::value };
};

/*
 * one
 */
template<std::size_t BITS, typename V>
struct one<bigint<BITS, V>> {
    static constexpr auto value = bigint<BITS, V>{ one<V>::value };
};

/*
 * two
 */
template<std::size_t BITS, typename V>
struct two<bigint<BITS, V>> {
    static constexpr auto value = bigint<BITS, V>{ two<V>::value };
};

/*
 * three
 */
template<std::size_t BITS, typename V>
struct three<bigint<BITS, V>> {
    static constexpr auto value = bigint<BITS, V>{ three<V>::value };
};

/*
 * ten
 */
template<std::size_t BITS, typename V>
struct ten<bigint<BITS, V>> {
    static constexpr auto value = bigint<BITS, V>{ ten<V>::value };
};

/*
 * of
 */
template<typename T, std::size_t INPUT_BITS, std::size_t OUTPUT_BITS>
constexpr bigint<OUTPUT_BITS, T> of(bigint<INPUT_BITS, T> a) {
    bigint<OUTPUT_BITS, T> value = {  };
    for (std::size_t i = 0; i < a.count; i++) {
        value[i] = a[i];
    }
    return value;
}

/*
 * '==' (Equality Operator)
 */
template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr bool operator==(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    constexpr auto count = std::max(a.count, b.count);
    for (std::size_t i = 0; i < count; ++i) {
        T ai = i < a.count ? a[i] : zero<T>::value;
        T bi = i < b.count ? b[i] : zero<T>::value;
        if (is_not_equal(ai, bi)) {
            return false;
        }
    }
    return true;
}

template<std::size_t BITS, typename T>
constexpr bool operator==(bigint<BITS, T> a, T b) {
    if (is_not_equal<T>(a[0], b)) {
        return false;
    }
    for (std::size_t i = 1; i < a.count; ++i) {
        if (is_not_equal(a[i], zero<T>::value)) {
            return false;
        }
    }
    return true;
}

/*
 * '<=' (Less Than or Equal Operator)
 */
template<typename T, std::size_t A_BITS, std::size_t B_BITS>
constexpr bool operator<=(bigint<A_BITS, T> a, bigint<B_BITS, T> b) {
    constexpr auto count = std::max(a.count, b.count);
    for (std::size_t i = count; i > 0; --i) {
        std::size_t j = i - 1;
        T aj = j < a.count ? a[j] : zero<T>::value;
        T bj = j < b.count ? b[j] : zero<T>::value;
        if (is_equal(aj, bj)) {
            continue;
        } else if (is_less(aj, bj)) {
            return true;
        } else {
            return false;
        }
    }
    return true;
}

/*
 * from_digits
 */
template<typename T, std::uint8_t RADIX = 10, typename DIGIT = std::uint8_t, std::size_t INPUT_LENGTH>
constexpr auto from_digits(const std::array<DIGIT, INPUT_LENGTH> digits) {
    using OverflowType = typename impl::bigint_datatype<T>::overflow_type;
    constexpr auto BITS = impl::required_bits_radix<RADIX, INPUT_LENGTH>::value;

    std::array<T, impl::required_elements<T, BITS>::value> result = {};
    for (std::size_t i = 0; i < INPUT_LENGTH; ++i) {
        OverflowType carry = zero<OverflowType>::value;
        for (std::size_t j = 0; j < result.size(); ++j) {
            OverflowType product = add(multiply(static_cast<OverflowType>(result[j]), static_cast<OverflowType>(RADIX)),
                                       carry);
            OverflowType low = product & std::numeric_limits<T>::max();
            carry = product >> std::numeric_limits<T>::digits;
            result[j] = low;
        }
        carry = digits[i];
        for (std::size_t j = 0; j < result.size(); ++j) {
            OverflowType sum = add(static_cast<OverflowType>(result[j]), carry);
            OverflowType low = sum & std::numeric_limits<T>::max();
            carry = sum >> std::numeric_limits<T>::digits;
            result[j] = low;
        }
    }
    return bigint<BITS, T>{ result };
}

/*
 * logarithm
 */
template<typename T, std::size_t BASE_BITS, std::size_t VALUE_BITS>
constexpr bigint<VALUE_BITS, T> logarithm(bigint<BASE_BITS, T> base, bigint<VALUE_BITS, T> value) {
    if (is_equal(value, one<bigint<VALUE_BITS, T>>::value)) {
        return zero<bigint<VALUE_BITS, T>>::value;
    }
    bigint<VALUE_BITS, T> exponent = zero<bigint<VALUE_BITS, T>>::value;
    for (
        bigint<VALUE_BITS + BASE_BITS, T> result = of<T, BASE_BITS, VALUE_BITS + BASE_BITS>(base);
        result <= value;
        result = multiply_in_place(result, base)
    ) {
        exponent = add_in_place(exponent, one<bigint<VALUE_BITS, T>>::value);
    }
    return exponent;
}

/*
 * _2 (Base 2 User-Defined-Literal)
 */
template<char... DIGITS>
constexpr auto operator "" _2() {
    return from_digits<std::uint32_t, 8>(parse_digits_base8<DIGITS...>());
}

/*
 * _8 (Base 8 User-Defined-Literal)
 */
template<char... DIGITS>
constexpr auto operator "" _8() {
    return from_digits<std::uint32_t, 8>(parse_digits_base8<DIGITS...>());
}

/*
 * _10 (Base 10 User-Defined-Literal)
 */
template<char... DIGITS>
constexpr auto operator "" _10() {
    return from_digits<std::uint32_t, 10>(parse_digits_base10<DIGITS...>());
}

/*
 * _16 (Base 16 User-Defined-Literal)
 */
template<char... DIGITS>
constexpr auto operator "" _16() {
    return from_digits<std::uint32_t, 16>(parse_digits_base16<DIGITS...>());
}

/*
 * _64 (Base 64 User-Defined-Literal)
 */
template<char... DIGITS>
constexpr auto operator "" _64() {
    return from_digits<std::uint32_t, 64>(parse_digits_base64<DIGITS...>());
}

} //namespace numbers

/*
 * bigint
 */

static_assert(numbers::bigint<16, std::uint8_t>::bits == 16);
static_assert(numbers::bigint<32, std::uint8_t>::bits == 32);
static_assert(numbers::bigint<64, std::uint8_t>({ 0 }).highest_bit() == 0);
static_assert(numbers::bigint<64, std::uint8_t>({ 1 }).highest_bit() == 1);
static_assert(numbers::bigint<64, std::uint8_t>({ 9 }).highest_bit() == 4);
static_assert(numbers::bigint<64, std::uint8_t>({ 255 }).highest_bit() == 8);
static_assert(numbers::bigint<64, std::uint8_t>({ 255, 127 }).highest_bit() == 15);
static_assert(numbers::bigint<64, std::uint8_t>({ 255, 255, 255, 1 }).highest_bit() == 25);
static_assert(numbers::bigint<64, std::uint8_t>({ 255, 255, 255, 255, 255, 255, 255, 63 }).highest_bit() == 62);
static_assert(numbers::bigint<8, std::uint8_t>({})[0] == static_cast<std::uint8_t>(0));
static_assert(numbers::bigint<8, std::uint8_t>({}).size() == 1);
static_assert(numbers::bigint<32, std::uint8_t>({}).size() == 4);
static_assert(numbers::bigint<64, std::uint8_t>({}).size() == 8);
static_assert(numbers::bigint<4096, std::uint64_t>({}).size() == 64);

/*
 * add
 */
static_assert(numbers::add<std::uint8_t, 8, 8>({1}, {1})[0] == 2);
static_assert(numbers::add<std::uint8_t, 8, 8>({1}, {1})[1] == 0);
static_assert(numbers::add<std::uint8_t, 8, 8>({1}, {1}).size() == 2);
static_assert(numbers::add<std::uint8_t, 8, 8>({128}, {128})[0] == 0);
static_assert(numbers::add<std::uint8_t, 8, 8>({128}, {128})[1] == 1);
static_assert(numbers::add<std::uint8_t, 8, 8>({128}, {128}).size() == 2);
static_assert(numbers::add<std::uint8_t, 8, 8>({1}, {1})[0] == 2);
static_assert(numbers::add<std::uint8_t, 8, 16>({1}, {1})[1] == 0);
static_assert(numbers::add<std::uint8_t, 8, 16>({1}, {1})[2] == 0);
static_assert(numbers::add<std::uint8_t, 8, 16>({1}, {1}).size() == 3);
static_assert(numbers::add<std::uint8_t, 8, 16>({255}, {0, 255})[0] == 255);
static_assert(numbers::add<std::uint8_t, 8, 16>({255}, {0, 255})[1] == 255);
static_assert(numbers::add<std::uint8_t, 8, 16>({255}, {0, 255})[2] == 0);
static_assert(numbers::add<std::uint8_t, 8, 16>({1}, {1}).size() == 3);
static_assert(numbers::add<std::uint8_t, 8, 16>({255}, {0, 255}).highest_bit() == 16);

/*
 * multiply
 */
static_assert(0 + (64 << 8LL) == 128 * 128);
static_assert(numbers::multiply<std::uint8_t, 8, 8>({128}, {128})[0] == 0);
static_assert(numbers::multiply<std::uint8_t, 8, 8>({128}, {128})[1] == 64);
static_assert(numbers::multiply<std::uint8_t, 8, 8>({128}, {128}).size() == 2);
static_assert(123 + (238 << 8) == 61051);
static_assert(141 + (25 << 8) + (1 << 16) == 72077);
static_assert(191LL + (92LL << 8LL) + (72LL << 16LL) + (6LL << 24LL) + (1LL << 32LL) == 61051LL * 72077LL);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1})[0] == 191);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1})[1] == 92);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1})[2] == 72);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1})[3] == 6);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1})[4] == 1);
static_assert(numbers::multiply<std::uint8_t, 16, 24>({123, 238}, {141, 25, 1}).size() == 5);

namespace numbers {

}

/*
 * zero
 */
static_assert(numbers::zero<numbers::bigint<64, std::uint8_t>>::value.bits == 64);
static_assert(numbers::zero<numbers::bigint<64, std::uint8_t>>::value.count == 8);
static_assert(numbers::zero<numbers::bigint<64, std::uint8_t>>::value.highest_bit() == 0);

/*
 * one
 */
static_assert(numbers::one<numbers::bigint<64, std::uint8_t>>::value.bits == 64);
static_assert(numbers::one<numbers::bigint<64, std::uint8_t>>::value.count == 8);
static_assert(numbers::one<numbers::bigint<64, std::uint8_t>>::value.highest_bit() == 1);

/*
 * two
 */
static_assert(numbers::two<numbers::bigint<64, std::uint8_t>>::value.bits == 64);
static_assert(numbers::two<numbers::bigint<64, std::uint8_t>>::value.count == 8);
static_assert(numbers::two<numbers::bigint<64, std::uint8_t>>::value.highest_bit() == 2);

/*
 * three
 */
static_assert(numbers::three<numbers::bigint<64, std::uint8_t>>::value.bits == 64);
static_assert(numbers::three<numbers::bigint<64, std::uint8_t>>::value.count == 8);
static_assert(numbers::three<numbers::bigint<64, std::uint8_t>>::value.highest_bit() == 2);

/*
 * ten
 */
static_assert(numbers::ten<numbers::bigint<64, std::uint8_t>>::value.bits == 64);
static_assert(numbers::ten<numbers::bigint<64, std::uint8_t>>::value.count == 8);
static_assert(numbers::ten<numbers::bigint<64, std::uint8_t>>::value.highest_bit() == 4);

/*
 * required_bits_radix (Base 8)
 */
static_assert(numbers::impl::required_bits_radix<8, 2>::value == 6);
static_assert(numbers::impl::required_bits_radix<8, 3>::value == 9);
static_assert(numbers::impl::required_bits_radix<8, 8>::value == 24);

/*
 * required_bits_radix (Base 10)
 */
static_assert(numbers::impl::required_bits_radix<10, 2>::value == 7);
static_assert(numbers::impl::required_bits_radix<10, 4>::value == 14);
static_assert(numbers::impl::required_bits_radix<10, 9>::value == 30);

/*
 * required_bits_radix (Base 16)
 */
static_assert(numbers::impl::required_bits_radix<16, 2>::value == 8);
static_assert(numbers::impl::required_bits_radix<16, 8>::value == 32);
static_assert(numbers::impl::required_bits_radix<16, 32>::value == 128);

/*
 * required_bits_radix (Base 64)
 */
static_assert(numbers::impl::required_bits_radix<64, 2>::value == 12);
static_assert(numbers::impl::required_bits_radix<64, 8>::value == 48);
static_assert(numbers::impl::required_bits_radix<64, 64>::value == 384);

/*
 * from_digits
 */
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("1234567890"))[0] == 1234567890);
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("340282366920938463463374607431768211455"))[0] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("340282366920938463463374607431768211455"))[1] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("340282366920938463463374607431768211455"))[2] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("340282366920938463463374607431768211455"))[3] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t>(numbers::parse_digits("340282366920938463463374607431768211455")).size() == 5);
static_assert(numbers::from_digits<std::uint32_t, 16>(numbers::parse_digits<16>("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))[0] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t, 16>(numbers::parse_digits<16>("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))[1] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t, 16>(numbers::parse_digits<16>("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))[2] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t, 16>(numbers::parse_digits<16>("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))[3] == 0xFFFFFFFF);
static_assert(numbers::from_digits<std::uint32_t, 16>(numbers::parse_digits<16>("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")).size() == 4);

namespace numbers {

/*
 * logarithm
 */
static_assert(numbers::logarithm(10_10, 115792089237316195423570985008687907853269984665640564039457584007913129639935_10) == 77_10, "Log10(115792089237316195423570985008687907853269984665640564039457584007913129639935)");

/*
 * _10 (Base 10 User-Defined-Literal)
 */
// 128 bit max
static_assert(340282366920938463463374607431768211455_10[0] == 0xFFFFFFFF);
static_assert(340282366920938463463374607431768211455_10[1] == 0xFFFFFFFF);
static_assert(340282366920938463463374607431768211455_10[2] == 0xFFFFFFFF);
static_assert(340282366920938463463374607431768211455_10[3] == 0xFFFFFFFF);
static_assert((340282366920938463463374607431768211455_10).size() == 5);
static_assert((340282366920938463463374607431768211455_10).bits == 130);
static_assert((340282366920938463463374607431768211455_10).count == 5);
static_assert((340282366920938463463374607431768211455_10).highest_bit() == 128);

// 256 bit max
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[0] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[1] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[2] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[3] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[4] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[5] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[6] == 0xFFFFFFFF);
static_assert(115792089237316195423570985008687907853269984665640564039457584007913129639935_10[7] == 0xFFFFFFFF);
static_assert((115792089237316195423570985008687907853269984665640564039457584007913129639935_10).size() == 9);
static_assert((115792089237316195423570985008687907853269984665640564039457584007913129639935_10).bits == 260);
static_assert((115792089237316195423570985008687907853269984665640564039457584007913129639935_10).count == 9);
static_assert((115792089237316195423570985008687907853269984665640564039457584007913129639935_10).highest_bit() == 256);

/*
 * _16 (Base 16 User-Defined-Literal)
 */
static_assert(0xFFAADF_16[0] == 0x00FFAADF);
static_assert((0xFFAADF_16).size() == 1);
static_assert(0x1234567890ABCDEF1234567890abcdef_16[0] == 0x90ABCDEF);
static_assert(0x1234567890ABCDEF1234567890abcdef_16[1] == 0x12345678);
static_assert(0x1234567890ABCDEF1234567890abcdef_16[2] == 0x90ABCDEF);
static_assert(0x1234567890ABCDEF1234567890abcdef_16[3] == 0x12345678);
static_assert((0x1234567890ABCDEF1234567890abcdef_16).size() == 4);

} //namespace numbers

#endif//NUMBERS_BIGINT_HH