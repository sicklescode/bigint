#ifndef NUMBERS_ARITHMETIC_HH
#define NUMBERS_ARITHMETIC_HH

namespace numbers {

/*
 * Identity
 */
template<typename V, V VALUE>
struct identity {
    static constexpr V value = VALUE;
};

/*
 * Magic Numbers
 */
template<typename V, typename... Args>
struct zero : identity<V, 0> {};

template<typename V, typename... Args>
struct one : identity<V, 1> {};

template<typename V, typename... Args>
struct two : identity<V, 2> {};

template<typename V, typename... Args>
struct three : identity<V, 3> {};

template<typename V, typename... Args>
struct ten : identity<V, 10> {};

/*
 * Equality (A == B)
 */

template<typename V>
constexpr bool is_equal(V A, V B) {
    return A == B;
}

/*
 * Inequality (A != B)
 */
template<typename V>
constexpr bool is_not_equal(V A, V B) {
    return A != B;
}

/*
 * Less Than (A < B)
 */
template<typename V>
constexpr bool is_less(V A, V B) {
    return A < B;
}

/*
 * Less Than Or Equal To (A <= B)
 */
template<typename V>
constexpr bool is_less_or_equal(V A, V B) {
    return A <= B;
}

/*
 * Greater Than  (A > B)
 */
template<typename V>
constexpr bool is_greater(V A, V B) {
    return A > B;
}

/*
 * Less Than Or Equal To (A <= B)
 */

template<typename V>
constexpr bool is_greater_or_equal(V A, V B) {
    return A >= B;
}

/*
 * negate (-)
 */
template<typename V>
constexpr auto negate(V value) {
    return -value;
}

/*
 * absolute_value (||)
 */
template<typename V>
constexpr V absolute_value(V value) {
    return is_greater(value, zero<V>::value) ? value : negate(value);
}

/*
 * Mod (%)
 */
template<typename A, typename B>
constexpr auto modulo(A a, B b) {
    return a % b;
}

/*
 * add (a + b)
 */
template<typename A, typename B>
constexpr auto add(A a, B b) {
    return a + b;
}

/*
 * subtract (a - b)
 */
template<typename A, typename B>
constexpr auto subtract(A a, B b) {
    return a - b;
}

/*
 * multiply (a * b)
 */
template<typename A, typename B>
constexpr auto multiply(A a, B b) {
    return a * b;
}

/*
 * divide (a / b)
 */
template<typename A, typename B>
constexpr auto divide(A a, B b) {
    return a / b;
}

/*
 * logarithm
 */
template<typename V>
constexpr V logarithm(V base, V value) {
    if (is_equal(value, one<V>::value)) {
        return zero<V>::value;
    }
    V exponent = zero<V>::value;
    for (V result = base; is_less_or_equal(result, value); result = multiply(result, base)) {
        exponent = add(exponent, 1);
    }
    return exponent;
}

}

/*
 * logarithm
 */
static_assert(numbers::logarithm(2, 1) == 0, "Log2(1)");
static_assert(numbers::logarithm(2, 63) == 5, "Log2(63)");
static_assert(numbers::logarithm(2, 64) == 6, "Log2(64)");
static_assert(numbers::logarithm(2, 127) == 6, "Log2(127)");
static_assert(numbers::logarithm(2, 128) == 7, "Log2(128)");
static_assert(numbers::logarithm(5, 625) == 4, "Log5(625)");
static_assert(numbers::logarithm(16, 4096) == 3, "Log16(4096)");
static_assert(numbers::logarithm(10, 100000) == 5, "Log10(100000)");

#endif//NUMBERS_ARITHMETIC_HH