//          Copyright Serge Aleynikov 2023.
//          Copyright Daniel Goertzen 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

//
// nifpp is a C++ Wrapper for the Erlang NIF API
//

//
// Boost license was chosen for nifpp because resource_ptr is derived
// from boost::intrusive_ptr.  License header from intrusive_ptr.hpp is...
//
//  intrusive_ptr.hpp
//
//  Copyright (c) 2001, 2002 Peter Dimov
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  See http://www.boost.org/libs/smart_ptr/intrusive_ptr.html for documentation.
//

#pragma once

// If you are getting the following warning, add "-std=C99" to CFLAGS
// erlang/26.0/erts-14.0/include/erl_nif.h:192:21: warning: comma at end of enumerator list [-Wpedantic]
//  192 |     ERL_NIF_UTF8 = 2,
#include <erl_nif.h>

// Only define map functions if they are available
#define NIFPP_HAS_MAPS ((ERL_NIF_MAJOR_VERSION > 2) || (ERL_NIF_MAJOR_VERSION==2 && ERL_NIF_MINOR_VERSION >= 6))

#include <string>
#include <tuple>
#include <array>
#include <vector>
#include <type_traits>
#include <list>
#include <deque>
#include <set>
#include <unordered_set>
#if NIFPP_HAS_MAPS
#include <map>
#include <unordered_map>
#endif
#include <cassert>
#include <cstring>
#include <functional>
#include <stdexcept>
#include <typeinfo>
#include <iostream>

namespace nifpp
{

// Library version

static constexpr const int NIFPP_MAJOR_VSN = 3;
static constexpr const int NIFPP_MINOR_VSN = 0;

struct TERM
{
    ERL_NIF_TERM v;

    TERM() {}
    explicit TERM(ERL_NIF_TERM x):v(x){}

    inline operator ERL_NIF_TERM() const { return v; }

    // FIXME: should we use enif_compare() instead???
    bool operator<(const TERM rhs) const { return v < rhs.v; }

    // Three-way compare. Corresponds to the Erlang operators ==, /=, =<, <, >=, and >.
    int compare(const TERM rhs) const { return enif_compare(*(TERM*)this, (TERM)rhs); }

    // There's no need to overload operator==, since the TERM has
    // implicit cast to ERL_NIF_TERM, which is long int.
};

static_assert(sizeof(TERM)==sizeof(ERL_NIF_TERM), "TERM size does not match ERL_NIF_TERM");

struct str_atom: public std::string
{
    template<class ... Args>
    str_atom(Args&& ... args) : std::string(std::forward<Args&&>(args)...) { }
};

struct atom
{
    atom() : val(0) {}
    atom(ErlNifEnv* env, const char* val) : val(enif_make_atom(env, val)) {}
    atom(ErlNifEnv* env, ERL_NIF_TERM v)
    {
        if (!enif_is_atom(env, v)) [[unlikely]]
            throw std::invalid_argument("expected atom");
        val = v;
    }

    atom(atom&& v)      : val(v) {}
    atom(const atom& v) : val(v) {}

    bool init(ErlNifEnv* env, const char* v)
    {
        assert(v == 0);
        size_t len = strlen(v);
        val = enif_make_atom_len(env, v, len);
        return len > 255 ? false : true;
    }

    bool init(ErlNifEnv* env, ERL_NIF_TERM v)
    {
        assert(val == 0);
        if (!enif_is_atom(env, v)) [[unlikely]]
            return false;
        val = v;
        return true;
    }

    bool initialized() const { return val != 0; }

    void operator=(atom&&      v) { val = v.val; }
    void operator=(const atom& v) { val = v.val; }

    bool operator==(ERL_NIF_TERM rhs) const { return  enif_is_identical(val, rhs);     }
    bool operator!=(ERL_NIF_TERM rhs) const { return !enif_is_identical(val, rhs);     }
    bool operator==(const atom&  rhs) const { return  enif_is_identical(val, rhs.val); }
    bool operator!=(const atom&  rhs) const { return !enif_is_identical(val, rhs.val); }

    operator ERL_NIF_TERM() const { return val; }
    operator TERM()         const { return TERM(val); }

    std::string to_string(ErlNifEnv* env, ErlNifCharEncoding encoding = ERL_NIF_LATIN1) const
    {
        assert(val);
        char buf[256];
        enif_get_atom(env, val, buf, sizeof(buf), encoding);
        return buf;
    }
private:
    ERL_NIF_TERM val;
};

// User-defined atoms registry
namespace detail {
    struct atom_registration {
        atom* atom_ptr;
        const char* atom_name;
        atom_registration* next;
    };

    static atom_registration* atom_registry = nullptr;

    inline void register_atom(atom* atom_ptr, const char* name) {
        static atom_registration* registry_tail = nullptr;

        auto* reg = new atom_registration;
        reg->atom_ptr = atom_ptr;
        reg->atom_name = name;
        reg->next = nullptr;

        if (!atom_registry) {
            atom_registry = registry_tail = reg;
        } else {
            registry_tail->next = reg;
            registry_tail = reg;
        }
    }
}

/// Macro to add a known atom that will be initialized by initialize_known_atoms()
///
/// Usage: NIFPP_ADD_KNOWN_ATOM(am_my_custom_atom) creates am_my_custom_atom
/// The atom name will be "my_custom_atom" in Erlang (am_ prefix is stripped)
#define NIFPP_ADD_KNOWN_ATOM(am_atom_name) \
    static ::nifpp::atom am_atom_name; \
    namespace { \
        struct atom_init_##am_atom_name { \
            atom_init_##am_atom_name() { \
                ::nifpp::detail::register_atom(&am_atom_name, #am_atom_name + 3); \
            } \
        }; \
        static atom_init_##am_atom_name atom_init_instance_##am_atom_name; \
    }

// Built-in known atoms - explicitly declared for better clarity
static atom am_true;
static atom am_false;
static atom am_ok;
static atom am_error;
static atom am_undefined;
static atom am_nil;

/// Initialize known atoms.
///
/// This function needs to be called once in the `load()` function of the NIF
/// library.
inline void initialize_known_atoms(ErlNifEnv* env)
{
    // Initialize built-in known atoms
    am_true      = atom(env, "true");
    am_false     = atom(env, "false");
    am_ok        = atom(env, "ok");
    am_error     = atom(env, "error");
    am_undefined = atom(env, "undefined");
    am_nil       = atom(env, "nil");

    // Initialize user-registered atoms
    for (auto* reg = detail::atom_registry; reg != nullptr; reg = reg->next) {
        *(reg->atom_ptr) = atom(env, reg->atom_name);
    }
}

} //namespace nifpp

// Add std::hash specializations.
// This allows nifpp types to be used in unordered_xxx containers.
namespace std {

template<> struct hash<nifpp::TERM>
{
  std::size_t operator()(const nifpp::TERM& k) const
  {
      return hash<ERL_NIF_TERM>()(k.v);
  }
};

template<> struct hash<nifpp::str_atom>
{
  std::size_t operator()(const nifpp::str_atom& k) const
  {
      return hash<std::string>()(k);
  }
};

template<> struct hash<nifpp::atom>
{
  std::size_t operator()(const nifpp::atom& k) const
  {
      return hash<ERL_NIF_TERM>()(k);
  }
};

} //namespace std


namespace nifpp
{

struct binary;
TERM make(ErlNifEnv* env, binary& var);
TERM make(ErlNifEnv* env, const binary& var);

struct binary: public ErlNifBinary
{
    //binary(): needs_release(false) {}
    explicit binary(size_t _size)
        : needs_release(enif_alloc_binary(_size, this)), allocated(needs_release)
    {}

#ifdef NIFPP_INTRUSIVE_UNIT_TEST
    static int release_counter;
#endif
    ~binary()
    {
        if (needs_release)
        {
#ifdef NIFPP_INTRUSIVE_UNIT_TEST
            release_counter++;
#endif
            enif_release_binary(this);
        }
    }

    // Whether the underlying enif_alloc_binary() call (made by the
    // constructor) actually succeeded -- unlike needs_release (which make()
    // clears once ownership of the data is transferred to a TERM), this
    // never changes after construction, so it's the only reliable way to
    // detect allocation failure (e.g. out of memory) from outside the class.
    bool ok() const { return allocated; }
    explicit operator bool() const { return allocated; }

    // Reallocate binary size
    bool realloc(size_t _size) { return enif_realloc_binary(this, _size) != 0; }

    friend TERM make(ErlNifEnv* env, binary& var); // make can set owns_data to false
    friend TERM make(ErlNifEnv* env, const binary& var);

protected:
    mutable bool needs_release;

private:
    bool allocated;

    // there's no nice way to keep track of owns_data in copies, so just prevent copying
    binary(const binary &) = delete;
    binary & operator=(const binary &) = delete;
};

#ifdef NIFPP_INTRUSIVE_UNIT_TEST
int binary::release_counter=0;
#endif

// Make an Erlang binary with space allocated for copying data after this call
static std::tuple<TERM, unsigned char*>
make_binary(ErlNifEnv* env, size_t size)
{
    ERL_NIF_TERM term;
    auto   p = enif_make_new_binary(env, size, &term);
    return std::make_tuple(TERM(term), p);
}

// Make an Erlang binary from a C++ string
inline TERM make_binary(ErlNifEnv* env, const std::string_view str)
{
    auto [term, p] = make_binary(env, str.length());
    memcpy(p, str.data(), str.length());
    return term;
}

// Make an Erlang binary from a char buffer
template <int N>
TERM make_binary(ErlNifEnv* env, const char (&str)[N])
{
    auto size = N-1;
    auto [term, p] = make_binary(env, size);
    memcpy(p, str, size);
    return term;
}

// Make an Erlang binary from a C++ string
inline TERM make_binary(ErlNifEnv* env, const char* str)
{
    auto len = strlen(str);
    auto [term, p] = make_binary(env, len);
    memcpy(p, str, len);
    return term;
}

using binary_string = std::string_view;

//using namespace std::string_literals;

inline binary_string operator ""_b(const char* s, size_t sz) { return std::string_view(s, sz); }

inline binary_string operator ""_s(const char* s, size_t sz) { return std::string_view(s, sz); }

//
// get()/make() functions
//

// forward declare all container overloads so they can be used recursively
template<typename ...Ts> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::tuple<Ts...>& var);
template<typename ...Ts> TERM make(ErlNifEnv* env, const std::tuple<Ts...>& var);

// Container get() and make() functions are now handled by the unified templates below
// Keep specialized versions that need forward declarations:
TERM make(ErlNifEnv* env, const std::vector<TERM>& var);
template<typename T, size_t N> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::array<T, N>& var);
template<typename T, size_t N> TERM make(ErlNifEnv* env, const std::array<T, N>& var);
template<size_t N>TERM make(ErlNifEnv* env, const std::array<TERM, N>& var);

#if NIFPP_HAS_MAPS
//template<typename TK, typename TV> bool add_to_map(ErlNifEnv* env, TERM &map, const std::pair<TK,TV>& var);

template<typename TK, typename TV, typename... Pairs>
bool add_to_map(ErlNifEnv* env, TERM &map, const std::pair<TK,TV>& var, Pairs&&... pairs);

template<typename TK, typename TV> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::map<TK,TV>& var);
template<typename TK, typename TV> TERM make(ErlNifEnv* env, const std::map<TK,TV>& var, TERM* add_to_map = NULL);

template<typename TK, typename TV> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_map<TK,TV>& var);
template<typename TK, typename TV> TERM make(ErlNifEnv* env, const std::unordered_map<TK,TV>& var, TERM* map_to_use = NULL);
#endif

// ERL_NIF_TERM
inline bool get(ErlNifEnv*, ERL_NIF_TERM term, TERM &var)
{
    var = TERM(term);
    return true;
}
inline TERM make(ErlNifEnv*, const TERM term) { return term; }

// str_atom
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, str_atom& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    unsigned len;
    auto ret = enif_get_atom_length(env, term, &len, encoding);
    if (!ret) [[unlikely]] return false;
    var.resize(len+1); // +1 for terminating null
    ret = enif_get_atom(env, term, var.data(), var.size(), encoding);
    if (!ret) [[unlikely]] return false;
    var.resize(len); // trim terminating null
    return true;
}
inline TERM make(ErlNifEnv* env, const str_atom& var)
{
    return TERM(enif_make_atom(env, var.c_str()));
}

// atom
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, atom& var)
{
    return var.init(env, term);
}
inline TERM make(ErlNifEnv*, const atom& var)
{
    assert(var.initialized());
    return TERM(var);
}


// "std::string" and "const char*"
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    // Check if it's a binary
    if (ErlNifBinary bin; enif_inspect_binary(env, term, &bin))
    {
        var = std::string((const char*)bin.data, bin.size);
        return true;
    }

    // The implementation below iterates through the list twice.

    unsigned len;
    auto ret = enif_get_list_length(env, term, &len); // full list iteration
    if (!ret) [[unlikely]]
        return false;

    var.resize(len+1); // +1 for terminating null
    ret = enif_get_string(env, term, var.data(), var.size(), encoding); // full list iteration

    assert(ret == int(len+1));

    var.resize(len);
    return ret;
}

// Create a charlist from string
template <int N>
inline TERM make(ErlNifEnv* env, const char (&var)[N], ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var, N-1, encoding));
}
inline TERM make(ErlNifEnv* env, const char* var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var, strlen(var), encoding));
}
inline TERM make(ErlNifEnv* env, const std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var.data(), var.size(), encoding));
}

// Create a binary from string
inline TERM make(ErlNifEnv* env, const binary_string& v) { return make_binary(env, std::string_view{v}); }

// bool
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, bool& var)
{
    if (!enif_is_atom(env, term)) [[unlikely]]
        return false;

    if (enif_is_identical(term, am_true))
    {
        var = true;
        return true;
    }

    if (enif_is_identical(term, am_false))
    {
        var = false;
        return true;
    }

    return false; // some other atom, return error
}
inline TERM make(ErlNifEnv*, const bool var)
{
    assert(am_true.initialized());
    return var ? am_true : am_false;
}


// Number conversions

inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, double& var)
{
    return enif_get_double(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const double var)
{
    return TERM(enif_make_double(env, var));
}

//------------------------------------------------------------------------------
// Template-based integer handling - consolidates repetitive code
//------------------------------------------------------------------------------

namespace detail {

// Template traits for mapping C++ integer types to NIF functions
template<typename T> struct integer_nif_traits;

// Macro to define traits for each integer type (reduces boilerplate)
#define NIFPP_DEFINE_INTEGER_TRAITS(CppType, GetFunc, MakeFunc) \
    template<> struct integer_nif_traits<CppType> { \
        static constexpr bool is_supported = true; \
        static bool get_value(ErlNifEnv* env, ERL_NIF_TERM term, CppType* var) { \
            return GetFunc(env, term, var); \
        } \
        static ERL_NIF_TERM make_value(ErlNifEnv* env, CppType var) { \
            return MakeFunc(env, var); \
        } \
    };

// Define traits for all supported integer types
NIFPP_DEFINE_INTEGER_TRAITS(int,           enif_get_int,    enif_make_int)
NIFPP_DEFINE_INTEGER_TRAITS(unsigned int,  enif_get_uint,   enif_make_uint)
NIFPP_DEFINE_INTEGER_TRAITS(long,          enif_get_long,   enif_make_long)
NIFPP_DEFINE_INTEGER_TRAITS(unsigned long, enif_get_ulong,  enif_make_ulong)

#if SIZEOF_LONG != 8
NIFPP_DEFINE_INTEGER_TRAITS(ErlNifSInt64,  enif_get_int64,  enif_make_int64)
NIFPP_DEFINE_INTEGER_TRAITS(ErlNifUInt64,  enif_get_uint64, enif_make_uint64)
#endif

#undef NIFPP_DEFINE_INTEGER_TRAITS

// SFINAE helper for template constraints
template<typename T, typename = void>
struct is_nif_integer : std::false_type {};

template<typename T>
struct is_nif_integer<T, typename std::enable_if<integer_nif_traits<T>::is_supported>::type> : std::true_type {};

// Container traits for template-based consolidation
template<typename T> struct container_traits {
    // Default traits for unsupported containers
    static constexpr bool is_sequential         = false;
    static constexpr bool is_set_like           = false;
    static constexpr bool has_reverse_iterators = false;
    static constexpr bool is_fixed_size         = false;
};

// Sequential container traits (vector, list, deque)
#define NIFPP_DEFINE_SEQUENTIAL_CONTAINER_TRAITS(ContainerType) \
    template<typename T> struct container_traits<ContainerType<T>> { \
        using value_type = T; \
        static constexpr bool is_supported          = true; \
        static constexpr bool is_sequential         = true; \
        static constexpr bool is_set_like           = false; \
        static constexpr bool has_reverse_iterators = true; \
        static constexpr bool is_fixed_size         = false; \
        static void insert_item(ContainerType<T>& container, const T& item) { \
            container.push_back(item); \
        } \
    };

NIFPP_DEFINE_SEQUENTIAL_CONTAINER_TRAITS(std::vector)
NIFPP_DEFINE_SEQUENTIAL_CONTAINER_TRAITS(std::list)
NIFPP_DEFINE_SEQUENTIAL_CONTAINER_TRAITS(std::deque)

#undef NIFPP_DEFINE_SEQUENTIAL_CONTAINER_TRAITS

// Set-like container traits (set, unordered_set, multiset)
#define NIFPP_DEFINE_SET_CONTAINER_TRAITS(ContainerType) \
    template<typename T> struct container_traits<ContainerType<T>> { \
        using value_type = T; \
        static constexpr bool is_sequential         = false; \
        static constexpr bool is_set_like           = true;  \
        static constexpr bool has_reverse_iterators = true;  \
        static void insert_item(ContainerType<T>& container, const T& item) { \
            container.insert(item); \
        } \
    };

NIFPP_DEFINE_SET_CONTAINER_TRAITS(std::set)
NIFPP_DEFINE_SET_CONTAINER_TRAITS(std::multiset)

// Special case for unordered_set - uses forward iterators
template<typename T> struct container_traits<std::unordered_set<T>> {
    using value_type = T;
    static constexpr bool is_sequential         = false;
    static constexpr bool is_set_like           = true;
    static constexpr bool has_reverse_iterators = false; // Special case!
    static void insert_item(std::unordered_set<T>& container, const T& item) {
        container.insert(item);
    }
};

#undef NIFPP_DEFINE_SET_CONTAINER_TRAITS

// Array container traits (special case - fixed size)
template<typename T, size_t N> struct container_traits<std::array<T, N>> {
    using value_type = T;
    static constexpr bool is_sequential         = true;
    static constexpr bool is_set_like           = false;
    static constexpr bool has_reverse_iterators = true;
    static constexpr bool is_fixed_size         = true;
};

// SFINAE helpers for container classification
template<typename T, typename = void>
struct is_consolidatable_container : std::false_type {};

template<typename T>
struct is_consolidatable_container<T, typename std::enable_if<
    container_traits<T>::is_sequential || container_traits<T>::is_set_like
>::type> : std::true_type {};

template<typename T, typename = void>
struct has_reverse_iterators : std::false_type {};

template<typename T>
struct has_reverse_iterators<T, typename std::enable_if<
    container_traits<T>::has_reverse_iterators
>::type> : std::true_type {};

} // namespace detail

// Unified template functions that replace ALL the repetitive integer code

// Basic get() function - works for all integer types
template<typename T>
inline typename std::enable_if<detail::is_nif_integer<T>::value, bool>::type
get(ErlNifEnv* env, ERL_NIF_TERM term, T& var)
{
    return detail::integer_nif_traits<T>::get_value(env, term, &var);
}

// Range-checking get() function - works for all integer types
template<typename T, typename I = T>
inline typename std::enable_if<detail::is_nif_integer<T>::value &&
                               detail::is_nif_integer<I>::value, bool>::type
get(ErlNifEnv* env, ERL_NIF_TERM term, T& var, I min, I max)
{
    return detail::integer_nif_traits<T>::get_value(env, term, &var) &&
           var >= T(min) && var <= T(max);
}

// Range-checking get() function - works for all integer types
template<typename T, typename I = T>
inline typename std::enable_if<detail::is_nif_integer<T>::value &&
                               detail::is_nif_integer<I>::value &&
                               std::is_unsigned<T>::value, bool>::type
get(ErlNifEnv* env, ERL_NIF_TERM term, T& var, I max)
{
    return detail::integer_nif_traits<T>::get_value(env, term, &var) && var <= T(max);
}

// make() function - works for all integer types
template<typename T>
inline typename std::enable_if<detail::is_nif_integer<T>::value, TERM>::type
make(ErlNifEnv* env, const T var)
{
    return TERM(detail::integer_nif_traits<T>::make_value(env, var));
}

//------------------------------------------------------------------------------
// Template-based container handling - consolidates repetitive container code
//------------------------------------------------------------------------------

// Unified get() function for sequential containers (vector, list, deque)
template<typename Container>
inline typename std::enable_if<
    detail::container_traits<Container>::is_sequential &&
    !detail::container_traits<Container>::is_fixed_size, bool
>::type get(ErlNifEnv* env, ERL_NIF_TERM term, Container& var)
{
    var.clear();
    using value_type = typename detail::container_traits<Container>::value_type;
    return list_for_each<value_type>(env, term, [&var](value_type item) {
        detail::container_traits<Container>::insert_item(var, item);
    });
}

// Unified get() function for set-like containers (set, unordered_set, multiset)
template<typename Container>
inline typename std::enable_if<
    detail::container_traits<Container>::is_set_like, bool
>::type get(ErlNifEnv* env, ERL_NIF_TERM term, Container& var)
{
    var.clear();
    using value_type = typename detail::container_traits<Container>::value_type;
    return list_for_each<value_type>(env, term, [&var](value_type item) {
        detail::container_traits<Container>::insert_item(var, item);
    });
}

// Unified make() function for all containers with reverse iterators
template<typename Container>
inline typename std::enable_if<
    detail::is_consolidatable_container<Container>::value &&
    detail::has_reverse_iterators<Container>::value, TERM
>::type make(ErlNifEnv* env, const Container& var)
{
    ERL_NIF_TERM tail = enif_make_list(env, 0);
    for(auto i = var.rbegin(); i != var.rend(); ++i) {
        tail = enif_make_list_cell(env, make(env, *i), tail);
    }
    return TERM(tail);
}

// Special make() function for containers that only have forward iterators (like unordered_set)
template<typename Container>
inline typename std::enable_if<
    detail::is_consolidatable_container<Container>::value &&
    !detail::has_reverse_iterators<Container>::value, TERM
>::type make(ErlNifEnv* env, const Container& var)
{
    ERL_NIF_TERM tail = enif_make_list(env, 0);
    for(auto i = var.begin(); i != var.end(); ++i) {
        tail = enif_make_list_cell(env, make(env, *i), tail);
    }
    return TERM(tail);
}


// binary and ErlNifBinary

inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary& var)
{
    return enif_inspect_binary(env, term, &var);
}
inline TERM make(ErlNifEnv* env, ErlNifBinary &var)
{
    return TERM(enif_make_binary(env, &var));
}
inline TERM make(ErlNifEnv* env, binary& var)
{
    var.needs_release = false;
    return TERM(enif_make_binary(env, &var));
}

inline TERM make(ErlNifEnv* env, const binary& var)
{
    var.needs_release = false;
    return TERM(enif_make_binary(env, const_cast<binary*>(&var)));
}

// ErlNifPid

inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifPid& var)
{
    return TERM(enif_get_local_pid(env, term, &var));
}

inline TERM make(ErlNifEnv* env, const ErlNifPid &var)
{
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wignored-qualifiers"
    return TERM(enif_make_pid(env, &var));
    #pragma GCC diagnostic pop
}


//
// resource wrappers
//

// forward declarations for friend statements
template<class T> class resource_ptr;
template<typename T>
bool  get(ErlNifEnv* env, ERL_NIF_TERM term, resource_ptr<T>& var);

template <typename T>
using ResourceDownEvent = std::function<void(T*, ErlNifEnv*, ErlNifPid*, ErlNifMonitor*)>;
template <typename T>
using ResourceStopEvent = std::function<void(T*, ErlNifEnv*, ErlNifEvent event, int is_direct_call)>;
template <typename T>
using ResourceDynCallEvent = std::function<void(T*, ErlNifEnv*, void* call_data)>;

template<typename T>
struct resource_events {
    resource_events() : resource_events(nullptr) {}
    explicit resource_events(
        ResourceDownEvent<T> down,
        ResourceStopEvent<T> stop = nullptr,
        ResourceDynCallEvent<T> dyncall = nullptr
    ) : on_down(down), on_stop(stop), on_dyncall(dyncall) {}

    ResourceDownEvent<T>    on_down;
    ResourceStopEvent<T>    on_stop;
    ResourceDynCallEvent<T> on_dyncall;
};

template<typename T, typename... Args>
resource_ptr<T> construct_resource_with_events(resource_events<T> const&, Args&&... args);

template<typename T, typename... Args>
resource_ptr<T> construct_resource(Args&&... args);


template<class T>
class resource_ptr
{
private:
    using this_type = resource_ptr;

public:
    using element_type = T;

    resource_ptr(): px(0) {}

private:
    resource_ptr(T* p, bool add_ref): px(p)
    {
        if (px != 0 && add_ref) enif_keep_resource((void*)px);
    }

    // construction only permitted from these functions:
    template<typename U, typename... Args>
    friend resource_ptr<U> construct_resource(Args&&... args);
    template<typename U, typename... Args>
    friend resource_ptr<U>
    construct_resource_with_events(resource_events<U> const&, Args&&... args);
    template<typename U>
    friend bool get(ErlNifEnv* env, ERL_NIF_TERM term, resource_ptr<U>& var);
    // I would have liked to specialize these to T instead of granting access
    // to all U, but this is just simpler.

public:

    resource_ptr(resource_ptr const& rhs): px(rhs.px)
    {
        if (px != 0) enif_keep_resource((void*)px);
    }

    ~resource_ptr()
    {
        if (px != 0) enif_release_resource((void*)px);
    }

    resource_ptr(resource_ptr&& rhs): px(rhs.px) { rhs.px = 0; }

    resource_ptr & operator=(resource_ptr&& rhs)
    {
        this_type(static_cast<resource_ptr&&>(rhs)).swap(*this);
        return *this;
    }

    resource_ptr & operator=(resource_ptr const& rhs)
    {
        this_type(rhs).swap(*this);
        return *this;
    }

    resource_ptr& operator=(T* rhs)
    {
        this_type(rhs).swap(*this);
        return *this;
    }

    void reset()       { this_type().swap(*this);    }
    void reset(T* rhs) { this_type(rhs).swap(*this); }

    T const* get() const { return px; }
    T*       get()       { return px; }

    T&       operator*()        { assert(px != 0); return *px; }
    const T& operator*()  const { assert(px != 0); return *px; }
    T*       operator->() const { assert(px != 0); return  px; }
    T*       operator&()  const { assert(px != 0); return  px; }

    operator bool () const { return px != 0; }

    void swap(resource_ptr & rhs)
    {
        T* tmp = px;
        px = rhs.px;
        rhs.px = tmp;
    }

private:
    T* px;
};

template<class T, class U>
inline bool operator==(resource_ptr<T> const& a, resource_ptr<U> const& b)
{
    return a.get() == b.get();
}

template<class T, class U>
inline bool operator!=(resource_ptr<T> const& a, resource_ptr<U> const& b)
{
    return a.get() != b.get();
}

template<class T, class U>
inline bool operator==(resource_ptr<T> const& a, U * b)
{
    return a.get() == b;
}

template<class T, class U>
inline bool operator!=(resource_ptr<T> const& a, U * b)
{
    return a.get() != b;
}

template<class T, class U>
inline bool operator==(T* a, resource_ptr<U> const& b)
{
    return a == b.get();
}

template<class T, class U>
inline bool operator!=(T* a, resource_ptr<U> const& b)
{
    return a != b.get();
}

template<class T>
inline bool operator<(resource_ptr<T> const& a, resource_ptr<T> const& b)
{
    return std::less<T*>()(a.get(), b.get());
}

template<class T>
void swap(resource_ptr<T>& lhs, resource_ptr<T>& rhs) { lhs.swap(rhs); }

// mem_fn support

template<class T>
T* get_pointer(resource_ptr<T> const& p) { return p.get(); }

template<class T, class U>
resource_ptr<T> static_pointer_cast(resource_ptr<U> const& p)
{
    return static_cast<T*>(p.get());
}

template<class T, class U>
resource_ptr<T> const_pointer_cast(resource_ptr<U> const& p)
{
    return const_cast<T*>(p.get());
}

template<class T, class U>
resource_ptr<T> dynamic_pointer_cast(resource_ptr<U> const& p)
{
    return dynamic_cast<T*>(p.get());
}


namespace detail //(resource detail)
{

template<typename T>
struct resource_wrapper
{
    T obj;
    bool constructed;
    resource_events<T> events;
};

template<typename T>
void resource_dtor(ErlNifEnv*, void* obj)
{
    auto p = reinterpret_cast<resource_wrapper<T>*>(obj);
    // invoke destructor only if object was successfully constructed
    if (p->constructed)
        p->obj.~T();
}

// ErlNifResourceDown
template<typename T>
void resource_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon)
{
    auto p = reinterpret_cast<resource_wrapper<T>*>(obj);
    if (p->events.on_down)
        p->events.on_down(&p->obj, env, pid, mon);
}

// ErlNifResourceStop
template<typename T>
void resource_stop(ErlNifEnv* env, void* obj, ErlNifEvent event, int is_direct_call)
{
    auto p = reinterpret_cast<resource_wrapper<T>*>(obj);
    if (p->events.on_stop)
        p->events.on_stop(&p->obj, env, event, is_direct_call);
}

// ErlNifResourceDynCall
template<typename T>
void resource_dyncall(ErlNifEnv* env, void* obj, void* call_data)
{
    auto p = reinterpret_cast<resource_wrapper<T>*>(obj);
    if (p->events.on_dyncall)
        p->events.on_dyncall(&p->obj, env, call_data);
}

template<typename T>
struct resource_data
{
    static ErlNifResourceType* type;
};

template<typename T> ErlNifResourceType* resource_data<T>::type=0;
/*
The above definition deserves some explanation:

As the compiler sees usages of register_resource<T>() and get(..., resource_ptr<T>),
instances of the above variable will pop into existance to hold the Erlang
resource type (ErlNifResourceType*).  register_resource<T>() initializes the
value, and get(..., resource_ptr<T>) uses it.

Definitions of static data members have external linkage, so if you are using
the same resource type in multiple source files, each compiled object will
have a duplicate instance of resource_data<T>::type.  For non-template static
data members you will get duplicate symbol errors at link-time.  For *template*
static data members, the consensus seems to be that the linker should eliminate
all duplicates (which is what we want).  The C++ standard isn't very explicit
about this (at least to me), so if you do run into duplicate symbol issues with
the above definition, simply move the above def into your source file where all
your register_resource<T>() calls are.  This will ensure that the above
definition appears only once in the final binary.

References:
http://stackoverflow.com/questions/19366615/static-member-variable-in-class-template

*/

} // namespace detail (resource detail)

//------------------------------------------------------------------------------
// Pid comparisons
//------------------------------------------------------------------------------
inline bool operator==(ErlNifPid const& a, ErlNifPid const& b)
{
    return enif_compare_pids(&a, &b) == 0;
}

inline bool operator!=(ErlNifPid const& a, ErlNifPid const& b)
{
    return enif_compare_pids(&a, &b) != 0;
}

inline bool operator<(ErlNifPid const& a, ErlNifPid const& b)
{
    return enif_compare_pids(&a, &b) < 0;
}

inline bool operator>(ErlNifPid const& a, ErlNifPid const& b)
{
    return enif_compare_pids(&a, &b) > 0;
}

//------------------------------------------------------------------------------
// Monitor comparisons
//------------------------------------------------------------------------------
inline bool operator==(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) == 0;
}

inline bool operator!=(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) != 0;
}

inline bool operator<(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) < 0;
}

inline bool operator>(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) > 0;
}

//------------------------------------------------------------------------------
// Process identity / messaging
//------------------------------------------------------------------------------

// The calling process's pid (only meaningful from a non-dirty NIF call,
// per enif_self()'s own restriction).
inline ErlNifPid self(ErlNifEnv* env)
{
    ErlNifPid pid;
    enif_self(env, &pid);
    return pid;
}

// enif_send() never takes ownership of msg_env (unlike enif_select(),
// see msg_env below) -- the caller must still free it (or let a
// nifpp::msg_env do so) after this call, success or not.
inline bool send(ErlNifEnv* env, ErlNifPid* to, ErlNifEnv* msg_env, TERM msg)
{
    return enif_send(env, to, msg_env, msg) != 0;
}

// RAII wrapper for a message environment (enif_alloc_env()/enif_free_env()).
//
// Two different ownership-transfer conventions meet here: enif_send() never
// takes ownership of msg_env (the caller must always free it, success or
// failure), while enif_select()/enif_select_read()/enif_select_write()
// *always* take ownership of msg_env, regardless of outcome -- the caller
// must never free it after passing it in. A plain msg_env used as an
// ErlNifEnv* (implicit conversion below) covers the first case, since the
// destructor frees it once the call site goes out of scope; release()
// covers the second, handing the raw pointer to the select call and
// disowning it here so the destructor becomes a no-op.
class msg_env
{
public:
    msg_env() : m_env(enif_alloc_env()) {}
    ~msg_env() { if (m_env) enif_free_env(m_env); }

    msg_env(msg_env const&) = delete;
    msg_env& operator=(msg_env const&) = delete;

    msg_env(msg_env&& rhs) noexcept : m_env(rhs.m_env) { rhs.m_env = nullptr; }
    msg_env& operator=(msg_env&& rhs) noexcept
    {
        if (this != &rhs) {
            if (m_env) enif_free_env(m_env);
            m_env = rhs.m_env;
            rhs.m_env = nullptr;
        }
        return *this;
    }

    operator ErlNifEnv*() const { return m_env; }

    // Relinquish ownership for APIs (enif_select and friends) that always
    // consume/free the environment themselves.
    ErlNifEnv* release() { auto* e = m_env; m_env = nullptr; return e; }

private:
    ErlNifEnv* m_env;
};

//------------------------------------------------------------------------------
// enif_select wrappers
//------------------------------------------------------------------------------

// `obj` is whatever resource pointer (or other registered identity) was
// associated with `event` via enif_select; passed through as void* exactly
// like the raw API, just typed at the call site instead of cast manually.
template<typename T>
inline int select_read(ErlNifEnv* env, ErlNifEvent event, T* obj, const ErlNifPid* pid,
                        TERM msg, msg_env& env_to_consume)
{
    return enif_select_read(env, event, (void*)obj, pid, msg, env_to_consume.release());
}

template<typename T>
inline int select_write(ErlNifEnv* env, ErlNifEvent event, T* obj, const ErlNifPid* pid,
                         TERM msg, msg_env& env_to_consume)
{
    return enif_select_write(env, event, (void*)obj, pid, msg, env_to_consume.release());
}

// No message is constructed for ERL_NIF_SELECT_STOP, so there's no
// msg_env to consume here -- enif_select() ignores the msg argument for
// this mode.
template<typename T>
inline int select_stop(ErlNifEnv* env, ErlNifEvent event, T* obj)
{
    return enif_select(env, event, ERL_NIF_SELECT_STOP, (void*)obj, nullptr, ERL_NIF_TERM(0));
}



//------------------------------------------------------------------------------
// Type checking functions
//------------------------------------------------------------------------------

// Check if term is an atom
inline bool is_atom(ErlNifEnv* env, TERM term) { return enif_is_atom(env, term) != 0; }

// Check if term is a binary
inline bool is_binary(ErlNifEnv* env, TERM term) { return enif_is_binary(env, term) != 0; }

// Check if term is a reference
inline bool is_ref(ErlNifEnv* env, TERM term) { return enif_is_ref(env, term) != 0; }

// Check if term is a function
inline bool is_fun(ErlNifEnv* env, TERM term) { return enif_is_fun(env, term) != 0; }

// Check if term is a PID
inline bool is_pid(ErlNifEnv* env, TERM term) { return enif_is_pid(env, term) != 0; }

// Check if term is a port
inline bool is_port(ErlNifEnv* env, TERM term) { return enif_is_port(env, term) != 0; }

// Check if term is an empty list
inline bool is_empty_list(ErlNifEnv* env, TERM term) { return enif_is_empty_list(env, term) != 0; }

// Check if term is a list
inline bool is_list(ErlNifEnv* env, TERM term) { return enif_is_list(env, term) != 0; }

// Check if term is a tuple
inline bool is_tuple(ErlNifEnv* env, TERM term) { return enif_is_tuple(env, term) != 0; }

// Check if term is a number (integer or float)
inline bool is_number(ErlNifEnv* env, TERM term) { return enif_is_number(env, term) != 0; }

// Check if term is a map
inline bool is_map(ErlNifEnv* env, TERM term) { return enif_is_map(env, term) != 0; }

// Check if term is an exception
inline bool is_exception(ErlNifEnv* env, TERM term) { return enif_is_exception(env, term) != 0; }

// Check if two terms are identical (same object)
inline bool is_identical(TERM lhs, TERM rhs) { return enif_is_identical(lhs, rhs) != 0; }



//------------------------------------------------------------------------------
// get/make functions
//------------------------------------------------------------------------------
template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, resource_ptr<T>& var)
{
    void* rawptr;
    if (!enif_get_resource(env, term, detail::resource_data<T>::type, &rawptr)) [[unlikely]]
        return false;
    var=resource_ptr<T>((T*)rawptr, true);
    return true;
}
template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, T*& var)
{
    return enif_get_resource(env, term, detail::resource_data<T>::type, (void**)&var);
}
template<typename T>
TERM make(ErlNifEnv* env, const resource_ptr<T>& var)
{
    return TERM(enif_make_resource(env, (void*)var.get()));
}
template<typename T>
TERM make_resource_binary(ErlNifEnv* env, const resource_ptr<T>& var, const void* data, size_t size)
{
    return TERM(enif_make_resource_binary(env, (void*)var.get(), data, size));
}


template<typename T>
bool register_resource(ErlNifEnv* env,
                      const char* name,
                      ErlNifResourceFlags flags = ErlNifResourceFlags(ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER),
                      ErlNifResourceFlags* tried = nullptr)
{
    ErlNifResourceTypeInit init{
      .dtor    = &detail::resource_dtor<T>,
      .stop    = &detail::resource_stop<T>,
      .down    = &detail::resource_down<T>,
      .members = 4,
      .dyncall = &detail::resource_dyncall<T>
    };

    detail::resource_data<T>::type =
        enif_open_resource_type_x(env, name, &init, flags, tried);

    return !!detail::resource_data<T>::type;
}

template<typename T, typename... Args>
resource_ptr<T> construct_resource_with_events(resource_events<T> const& events, Args&&... args)
{
    ErlNifResourceType* type = detail::resource_data<T>::type;

    if (type == 0) {
        std::cerr << "Resource type '" << typeid(T).name() << "' hasn't been registered!\r\n";
        throw std::invalid_argument("unregistered resource");
    }

    void* mem = enif_alloc_resource(type, sizeof(detail::resource_wrapper<T>));

    assert(mem);

    // inhibit destructor in case ctor fails
    auto p = reinterpret_cast<detail::resource_wrapper<T>*>(mem);
    // immediately assign to resource pointer so that release will be called if construction fails
    resource_ptr<T> rptr(&p->obj, false); //note: private ctor

    p->constructed = false;

    // copy the events
    new (&p->events) resource_events<T>(events);
    // invoke constructors with "placement new"
    new (&p->obj) T(std::forward<Args&&>(args)...);

    // ctor succeeded, enable dtor
    p->constructed = true;
    return rptr;
}

template<typename T, typename... Args>
resource_ptr<T> construct_resource(Args&&... args)
{
    ErlNifResourceType* type = detail::resource_data<T>::type;

    if (type == 0) {
        std::cerr << "Resource type '" << typeid(T).name() << "' hasn't been registered!\r\n";
        throw std::invalid_argument("unregistered resource");
    }

    void* mem = enif_alloc_resource(type, sizeof(detail::resource_wrapper<T>));

    // inhibit destructor in case ctor fails
    auto p = reinterpret_cast<detail::resource_wrapper<T>*>(mem);
    // immediately assign to resource pointer so that release will be called if construction fails
    resource_ptr<T> rptr(&p->obj, false); //note: private ctor

    p->constructed = false;

    // copy the events
    new (&p->events) resource_events<T>();
    // invoke constructors with "placement new"
    new (&p->obj) T(std::forward<Args&&>(args)...);

    // ctor succeeded, enable dtor
    p->constructed = true;
    return rptr;
}

//
// container get()/make()
//

// tuple

namespace detail
{

    template<int I>
    struct array_to_tupler
    {
        template<typename ...Ts>
        static bool go(ErlNifEnv *env, std::tuple<Ts...> &t, const ERL_NIF_TERM *end)
        {
            return array_to_tupler<I-1>::go(env, t, --end)
                && get(env, *end, std::get<I-1>(t));
        }
    };

    template<>
    struct array_to_tupler<0>
    {
        template<typename ...Ts>
        static bool go(ErlNifEnv *, std::tuple<Ts...> &, const ERL_NIF_TERM *)
        {
            return 1;
        }
    };
} // namespace detail

template<typename ...Ts>
bool get(ErlNifEnv *env, ERL_NIF_TERM term, std::tuple<Ts...> &var)
{
    int arity;
    const ERL_NIF_TERM *array;
    int ret = enif_get_tuple(env, term, &arity, &array);

    // check if tuple
    if(!ret) [[unlikely]]
        return ret;

    // check for matching arity
    if (size_t(arity) != sizeof...(Ts)) [[unlikely]]
        return 0;

    // invoke recursive template to convert all items of tuple
    return detail::array_to_tupler<std::tuple_size<std::tuple<Ts...>>::value>::go(env, var, array+arity);
}
/*
template<typename ...Ts>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::tuple<Ts...>& var)
{
    int arity;
    const ERL_NIF_TERM* array;
    auto ret = enif_get_tuple(env, term, &arity, &array);

    // check if tuple
    if (!ret) [[unlikely]]
        return ret;

    // check for matching arity
    if (size_t(arity) != sizeof...(Ts)) [[unlikely]]
        return false;

    auto res = true;
    auto set = [env, &res, &array](auto&& x) { res &= get(env, *array++, x); };
    std::apply([&set](auto&&... arg) { (set(std::forward<decltype(arg)>(arg)), ...); }, var);
    return res;
}
*/

template<typename T1, typename T2>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::pair<T1, T2>& var)
{
    int arity;
    const ERL_NIF_TERM* array;
    auto ret = enif_get_tuple(env, term, &arity, &array);

    // check if tuple of size 2
    if (!ret || arity != 2) [[unlikely]]
        return false;
    return get(env, array[0], var.first) && get(env, array[1], var.second);
}

template<typename T1, typename T2>
TERM make(ErlNifEnv* env, const std::pair<T1, T2>& var)
{
    std::array<ERL_NIF_TERM, 2> array{ make(env, var.first), make(env, var.second)};
    return TERM(enif_make_tuple_from_array(env, array.begin(), array.size()));
}

// Alternative tuple to array implementation
namespace {
    // Function to iterate through all values, where I = size of tuple
    template <size_t I = 0, typename... Ts>
    typename std::enable_if_t<I == sizeof...(Ts), void>
    to_array(ErlNifEnv*, ERL_NIF_TERM*, const std::tuple<Ts...>&)
    {
        // If iterated through all values of tuple, then simply return.
        return;
    }

    template <size_t I = 0, typename... Ts>
    typename std::enable_if_t<(I < sizeof...(Ts)), void>
    to_array(ErlNifEnv* env, ERL_NIF_TERM* a, const std::tuple<Ts...>& tup)
    {
        *a = make(env, get<I>(tup));
        // Go to next element
        to_array<I + 1>(env, ++a, tup);
    }
}

template<typename... Ts>
TERM make1(ErlNifEnv* env, const std::tuple<Ts...>& var)
{
    std::array<ERL_NIF_TERM, sizeof...(Ts)> array;
    to_array(env, array.data(), var);
    return TERM(enif_make_tuple_from_array(env, array.begin(), array.size()));
}

template<typename... Ts>
TERM make(ErlNifEnv* env, const std::tuple<Ts...>& var)
{
    std::array<ERL_NIF_TERM, sizeof...(Ts)> array;
    auto it = array.begin();
    std::apply([env, &it](auto&&... x) { ((*it++ = make(env, x)), ...); }, var);
    return TERM(enif_make_tuple_from_array(env, array.begin(), array.size()));
}

template<typename... Args>
TERM make_tuple(ErlNifEnv* env, Args&&... args)
{
    return make(env, std::make_tuple(std::forward<Args&&>(args)...));
}

/*
  Disabling for now.  These feel too "loose".  Just use an explicit tuple
template<typename T0, typename T1, typename ...Ts>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, T0 &t0, T1 &t1, Ts&... ts)
{
    auto tup = std::tie(t0, t1, ts...);
    return get(env, term, tup);
}

template<typename T0, typename T1, typename ...Ts>
ERL_NIF_TERM make(ErlNifEnv* env, const T0 &t0, const T1 &t1, const Ts&... ts)
{
    return make(env, std::make_tuple(t0, t1, ts...));
}
*/

/*
template<typename T=ERL_NIF_TERM, typename F>
bool niftuple_for_each(ErlNifEnv* env, ERL_NIF_TERM term, const F &f)
{
    int arity;
    ERL_NIF_TERM* array;
    if (!enif_get_tuple(env, term, &arity, &array)) return 0;
    for(int i=0; i<arity; i++)
    {
        T var;
        if (!get(env, array[i], var)) return 0; // conversion failure
        f(std::move(var));
    }
    return 1;
}
*/

// list

template<typename T=ERL_NIF_TERM, typename F>
bool list_for_each(ErlNifEnv* env, ERL_NIF_TERM term, const F& f)
{
    if (!enif_is_list(env, term)) [[unlikely]] return false;
    ERL_NIF_TERM head, tail = term;
    while(enif_get_list_cell(env, tail, &head, &tail))
    {
        T var;
        if (!get(env, head, var)) [[unlikely]] return false; // conversion failure
        f(std::move(var));
    }
    return true;
}

// vector<T> get() and make() are now handled by the unified template functions above
inline TERM make(ErlNifEnv* env, const std::vector<TERM>& var)
{
    return TERM(enif_make_list_from_array(env, (ERL_NIF_TERM*)&var[0], var.size()));
}


template<typename T, size_t N>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::array<T, N>& var)
{
    unsigned len;
    auto ret = enif_get_list_length(env, term, &len);
    if (!ret) [[unlikely]] return false;

    // arrays are statically sized so size must match.
    if (size_t(len) != var.size()) [[unlikely]] return false;

    int i=0;
    return list_for_each<T>(env, term, [&var, &i](T item){var[i++] = item;});
}
template<typename T, size_t N>
TERM make(ErlNifEnv* env, const std::array<T, N>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}
template<size_t N>
TERM make(ErlNifEnv* env, const std::array<TERM, N>& var)
{
    return TERM(enif_make_list_from_array(env, (ERL_NIF_TERM*)&var[0], var.size()));
}


// list<T>  get() and make() are now handled by the unified template functions above
// deque<T> get() and make() are now handled by the unified template functions above
// set<T>   get() and make() are now handled by the unified template functions above

template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_set<T>& var)
{
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.insert(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::unordered_set<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.begin(); i!=var.end(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}

template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::multiset<T>& var)
{
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.insert(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::multiset<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}

#if NIFPP_HAS_MAPS
template<typename TK, typename TV, typename F>
bool map_for_each(ErlNifEnv* env, ERL_NIF_TERM term, const F& f)
{
    ErlNifMapIterator iter;

    if (!enif_map_iterator_create(env, term, &iter, ERL_NIF_MAP_ITERATOR_HEAD)) return 0;

    TERM key_term, value_term;
    TK key;
    TV value;
    while(enif_map_iterator_get_pair(env, &iter, (ERL_NIF_TERM *)&key_term, (ERL_NIF_TERM *)&value_term))
    {
        if (!get(env, key_term, key)) goto error; // conversion failure
        if (!get(env, value_term, value)) goto error; // conversion failure
        f(std::move(key), std::move(value));

        enif_map_iterator_next(env, &iter);
    }
    enif_map_iterator_destroy(env, &iter);
    return true;

    error:
    enif_map_iterator_destroy(env, &iter);
    return false;
}

template<typename TK, typename TV>
bool add_to_map(ErlNifEnv* env, TERM &map, const std::pair<TK,TV>& var)
{
    return enif_make_map_put(env, map,
            make(env, var.first), make(env, var.second), (ERL_NIF_TERM *)&map);
}

template<typename TK, typename TV, typename... Pairs>
bool add_to_map(ErlNifEnv* env, TERM &map, const std::pair<TK,TV>& var, Pairs&&... pairs)
{
    return enif_make_map_put(env, map,
            make(env, var.first), make(env, var.second), (ERL_NIF_TERM *)&map)
        && add_to_map(env, map, std::forward<Pairs>(pairs)...);
}

template<typename TK, typename TV>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::map<TK,TV>& var)
{
    var.clear();
    return map_for_each<TK,TV>(env, term, [&var](TK key, TV value){var[key]=value;});
}

template<typename TK, typename TV>
TERM make(ErlNifEnv* env, const std::map<TK,TV>& var, TERM* map_to_use)
{
    TERM map(map_to_use ? map_to_use->v : enif_make_new_map(env));
    for(auto& kv : var)
        add_to_map(env, map, std::make_pair(kv.first, kv.second));
    return map;
}

template<typename TK, typename TV>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_map<TK,TV>& var)
{
    var.clear();
    return map_for_each<TK,TV>(env, term, [&var](TK key, TV value){var[key]=value;});
}

template<typename TK, typename TV>
TERM make(ErlNifEnv* env, const std::unordered_map<TK,TV>& var, TERM* map_to_use)
{
    TERM map(map_to_use ? map_to_use->v : enif_make_new_map(env));
    for(auto& kv : var)
        add_to_map(env, map, std::make_pair(kv.first, kv.second));
    return map;
}
#endif //NIFPP_HAS_MAPS

// convenience wrappers for get()

template<typename T>
T get(ErlNifEnv* env, ERL_NIF_TERM term)
{
    T temp;
    if (get(env, term, temp))
    {
        return temp;
    }
    throw std::invalid_argument("term");
}

template<typename T>
void get_throws(ErlNifEnv* env, ERL_NIF_TERM term, T &t)
{
    if (!get(env, term, t))
    {
        throw std::invalid_argument("t");
    }
}


//------------------------------------------------------------------------------
// Exceptions
//------------------------------------------------------------------------------
inline TERM badarg(ErlNifEnv* env) { return TERM(enif_make_badarg(env)); }

template <typename T>
inline TERM raise_exception(ErlNifEnv* env, T&& arg) {
    return TERM(enif_raise_exception(env, make(env, arg)));
}

template <typename T, typename... Args>
inline TERM raise_exception(ErlNifEnv* env, T&& arg, Args&&... args) {
    return TERM(enif_raise_exception(
        env, make(env, std::make_tuple(std::forward<T&&>(arg),
                                       std::forward<Args&&>(args)...))));
}

} // namespace nifpp
