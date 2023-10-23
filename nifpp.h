
//          Copyright Daniel Goertzen 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

//
// nifpp is a C++11 Wrapper for the Erlang NIF API
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


#ifndef NIFPP_H
#define NIFPP_H

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

static constexpr const int NIFPP_MAJOR_VSN = 2;
static constexpr const int NIFPP_MINOR_VSN = 1;

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
    am_true      = atom(env, "true");
    am_false     = atom(env, "false");
    am_ok        = atom(env, "ok");
    am_error     = atom(env, "error");
    am_undefined = atom(env, "undefined");
    am_nil       = atom(env, "nil");
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
TERM make(ErlNifEnv* env, binary &var);
struct binary: public ErlNifBinary
{
    //binary(): needs_release(false) {}
    explicit binary(size_t _size)
        : needs_release(enif_alloc_binary(_size, this))
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

    friend TERM make(ErlNifEnv* env, binary &var); // make can set owns_data to false

protected:
    bool needs_release;

private:
    // there's no nice way to keep track of owns_data in copies, so just prevent copying
    binary(const binary &) = delete;
    binary & operator=(const binary &) = delete;
};

#ifdef NIFPP_INTRUSIVE_UNIT_TEST
int binary::release_counter=0;
#endif

//
// get()/make() functions
//

// forward declare all container overloads so they can be used recursively
template<typename ...Ts> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::tuple<Ts...>& var);
template<typename ...Ts> TERM make(ErlNifEnv* env, const std::tuple<Ts...>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::vector<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::vector<T>& var);
TERM make(ErlNifEnv* env, const std::vector<TERM>& var);

template<typename T, size_t N> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::array<T, N>& var);
template<typename T, size_t N> TERM make(ErlNifEnv* env, const std::array<T, N>& var);
template<size_t N>TERM make(ErlNifEnv* env, const std::array<TERM, N>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::list<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::list<T>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::deque<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::deque<T>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::set<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::set<T>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_set<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::unordered_set<T>& var);

template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::multiset<T>& var);
template<typename T> TERM make(ErlNifEnv* env, const std::multiset<T>& var);

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
inline TERM make(ErlNifEnv* env, const str_atom &var)
{
    return TERM(enif_make_atom(env, var.c_str()));
}

// atom
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, atom &var)
{
    return var.init(env, term);
}
inline TERM make(ErlNifEnv *, const atom &var)
{
    assert(var.initialized());
    return TERM(var);
}


// "std::string" and "const char*"
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    // The implementation below iterates through the list twice.  It may
    // be faster to iterate through the list and append bytes one at a time.

    unsigned len;
    auto ret = enif_get_list_length(env, term, &len); // full list iteration
    if (!ret)
    {
        // not a list, try as binary
        ErlNifBinary bin;
        ret = enif_inspect_binary(env, term, &bin);
        if (!ret) [[unlikely]]
        {
            // not a binary either, so fail.
            return false;
        }
        var = std::string((const char*)bin.data, bin.size);
        return ret;
    }
    var.resize(len+1); // +1 for terminating null
    ret = enif_get_string(env, term, var.data(), var.size(), encoding); // full list iteration
    if (ret > 0)
        var.resize(ret-1); // trim terminating null
    else if (ret==0)
        var.resize(0);
    else
    {
        // oops string somehow got truncated
        // var is correct size so do nothing
    }
    return ret;
}
inline TERM make(ErlNifEnv* env, const std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var.data(), var.size(), encoding));
}
template <int N>
inline TERM make(ErlNifEnv* env, const char (&var)[N], ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var, N-1, encoding));
}
inline TERM make(ErlNifEnv* env, const char* var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
{
    return TERM(enif_make_string_len(env, var, strlen(var), encoding));
}


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


inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, int& var)
{
    return enif_get_int(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const int var)
{
    return TERM(enif_make_int(env, var));
}


inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, unsigned int& var)
{
    return enif_get_uint(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const unsigned int var)
{
    return TERM(enif_make_uint(env, var));
}

#if SIZEOF_LONG != 8
inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifSInt64& var)
{
    return enif_get_int64(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const ErlNifSInt64 var)
{
    return TERM(enif_make_int64(env, var));
}

inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifUInt64& var)
{
    return enif_get_uint64(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const ErlNifUInt64 var)
{
    return TERM(enif_make_uint64(env, var));
}
#endif


inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, long& var)
{
    return enif_get_long(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const long var)
{
    return TERM(enif_make_long(env, var));
}

inline bool get(ErlNifEnv* env, ERL_NIF_TERM term, unsigned long& var)
{
    return enif_get_ulong(env, term, &var);
}
inline TERM make(ErlNifEnv* env, const unsigned long var)
{
    return TERM(enif_make_ulong(env, var));
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
inline TERM make(ErlNifEnv* env, binary &var)
{
    var.needs_release = false;
    return TERM(enif_make_binary(env, &var));
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
using ResourceDownEvent = void (*)(T*, ErlNifEnv*, ErlNifPid*, ErlNifMonitor*);
template <typename T>
using ResourceStopEvent = void (*)(T*, ErlNifEnv*, ErlNifEvent event, int is_direct_call);
template <typename T>
using ResourceDynCallEvent = void (*)(T*, ErlNifEnv*, void* call_data);

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

    void reset() { this_type().swap(*this); }

    void reset(T* rhs) { this_type(rhs).swap(*this); }

    T const* get() const { return px; }
    T*       get()       { return px; }

    T& operator*() const
    {
        assert(px != 0);
        return *px;
    }

    T* operator->() const
    {
        assert(px != 0);
        return px;
    }

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

inline bool operator<(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) < 0;
}

inline bool operator>(ErlNifMonitor const& a, ErlNifMonitor const& b)
{
    return enif_compare_monitors(&a, &b) > 0;
}

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

template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::vector<T>& var)
{
    unsigned len;
    auto ret = enif_get_list_length(env, term, &len);
    if (!ret) [[unlikely]] return false;
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.push_back(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::vector<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}
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


template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::list<T>& var)
{
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.push_back(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::list<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}


template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::deque<T>& var)
{
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.push_back(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::deque<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}

template<typename T>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::set<T>& var)
{
    var.clear();
    return list_for_each<T>(env, term, [&var](T item){var.insert(item);});
}
template<typename T>
TERM make(ErlNifEnv* env, const std::set<T>& var)
{
    ERL_NIF_TERM tail;
    tail = enif_make_list(env, 0);
    for(auto i=var.rbegin(); i!=var.rend(); i++)
    {
        tail = enif_make_list_cell(env, make(env,*i), tail);
    }
    return TERM(tail);
}

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

#endif // NIFPP_H
