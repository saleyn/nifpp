# NIFPP Documentation

NIFPP is a C++ wrapper for the Erlang NIF API that provides type-safe, RAII-compliant interfaces for working with Erlang terms from C++.

## Version

- **Major Version**: 3
- **Minor Version**: 0

## Table of Contents

- [Core Types](#core-types)
  - [TERM](#term)
  - [str_atom](#str_atom)
  - [atom](#atom)
  - [binary](#binary)
- [Type Conversion Functions](#type-conversion-functions)
- [Container Operations](#container-operations)
- [Resource Management](#resource-management)
- [Process and Messaging](#process-and-messaging)
- [Type Checking Functions](#type-checking-functions)
- [Exception Handling](#exception-handling)

## Core Types

### 🡅[TERM](#table-of-contents)

A wrapper around `ERL_NIF_TERM` that provides additional functionality.

```cpp
struct TERM {
    ERL_NIF_TERM v;

    TERM() {}
    explicit TERM(ERL_NIF_TERM x) : v(x) {}

    operator ERL_NIF_TERM() const { return v; }
    bool operator<(const TERM rhs) const { return v < rhs.v; }
    int compare(const TERM rhs) const { return enif_compare(*(TERM*)this, (TERM)rhs); }
};
```

**Usage:**
```cpp
ErlNifEnv* env = /* ... */;
ERL_NIF_TERM erlang_term = /* ... */;
nifpp::TERM term(erlang_term);

// Compare terms
if (term.compare(other_term) == 0) {
    // Terms are equal
}
```

### 🡅[str_atom](#table-of-contents)

A string-based atom type that inherits from `std::string`.

```cpp
struct str_atom : public std::string {
    template<class ... Args>
    str_atom(Args&& ... args) : std::string(std::forward<Args&&>(args)...) { }
};
```

**Usage:**
```cpp
nifpp::str_atom atom_str;
if (nifpp::get(env, term, atom_str)) {
    std::cout << "Atom value: " << atom_str << std::endl;
}
```

### 🡅[atom](#table-of-contents)

A more efficient atom type that stores the `ERL_NIF_TERM` directly.

**Usage:**
```cpp
// Create atom from string
nifpp::atom my_atom(env, "hello");

// Check if atom is initialized
if (my_atom.initialized()) {
    std::string atom_str = my_atom.to_string(env);
}

// Compare atoms
if (my_atom == nifpp::am_ok) {
    // Handle ok atom
}
```

#### Built-in Known Atoms

Pre-defined atoms for common values:

```cpp
static atom am_true;
static atom am_false;
static atom am_ok;
static atom am_error;
static atom am_undefined;
static atom am_nil;
```

#### initialize_known_atoms()

Initialize all known atoms. **Must be called in your NIF's `load()` function.**

```cpp
void initialize_known_atoms(ErlNifEnv* env)
```

**Usage:**
```cpp
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    nifpp::initialize_known_atoms(env);
    // ... other initialization
    return 0;
}
```

#### NIFPP_ADD_KNOWN_ATOM Macro

Define custom known atoms that will be initialized automatically.

```cpp
#define NIFPP_ADD_KNOWN_ATOM(am_atom_name)
```

**Usage:**
```cpp
// Define custom atoms (usually in header file)
NIFPP_ADD_KNOWN_ATOM(am_my_custom_atom);  // Creates atom for "my_custom_atom"
NIFPP_ADD_KNOWN_ATOM(am_success);         // Creates atom for "success"

// Use after initialize_known_atoms() is called
nifpp::TERM result = nifpp::make_tuple(env, am_success, data);
```

### 🡅[binary](#table-of-contents)

RAII wrapper for Erlang binaries that automatically manages memory.

**Constructors:**
```cpp
binary(size_t size)                 // Allocate binary of given size
binary(const char* str)             // From C string
binary(const char (&str)[N])        // From C string array
binary(const std::string& str)      // From std::string
binary(std::string_view str)        // From std::string_view
```

**Usage:**
```cpp
// Create binary from string
nifpp::binary bin("Hello, World!");

// Create empty binary with size
nifpp::binary sized_bin(100);

// Check if binary is valid
if (sized_bin.ok()) {
    // Fill with data
    memcpy(sized_bin.data, "data", 4);

    // Convert to Erlang term
    nifpp::TERM result = nifpp::make(env, sized_bin);
}

// Reallocate binary
if (sized_bin.realloc(200)) {
    // Successfully resized
}
```

#### make_binary()

Create Erlang binaries from various C++ types.

```cpp
static std::tuple<TERM, unsigned char*> make_binary(ErlNifEnv* env, size_t size)
TERM make_binary(ErlNifEnv* env, const std::string_view str)
TERM make_binary(ErlNifEnv* env, const char* str)
template <int N> TERM make_binary(ErlNifEnv* env, const char (&str)[N])
```

**Usage:**
```cpp
// Create binary with allocated space
auto [term, ptr] = nifpp::make_binary(env, 100);
memcpy(ptr, "Hello", 5);

// Create binary from string
std::string data        = "Binary data";
nifpp::TERM binary_term = nifpp::make_binary(env, data);

// Create binary from C string
const char* str         = "C string";
nifpp::TERM c_binary    = nifpp::make_binary(env, str);

// Create binary from string literal
nifpp::TERM binary      = nifpp::make_binary(env, "literal");
```

Convenient string literal operators for creating binary strings.

```cpp
using binary_string = std::string_view;

binary_string operator ""_b(const char* s, size_t sz)  // Binary literal
binary_string operator ""_s(const char* s, size_t sz)  // String literal
```

**Usage:**
```cpp
// Using binary literals
auto bin_data = "Hello"_b;
nifpp::TERM binary = nifpp::make(env, bin_data);   // Erlang binary term

auto str_data = "World"_s;
nifpp::TERM string = nifpp::make(env, str_data);   // Erlang charlist
```

## 🡅[Type Conversion Functions](#table-of-contents)

### get() Functions

Convert Erlang terms to C++ types. Returns `true` on success, `false` on failure.

#### Basic Types

```cpp
// Integers (all integer types supported)
bool get(ErlNifEnv* env, ERL_NIF_TERM term, int& var)
bool get(ErlNifEnv* env, ERL_NIF_TERM term, unsigned int& var)
bool get(ErlNifEnv* env, ERL_NIF_TERM term, long& var)
bool get(ErlNifEnv* env, ERL_NIF_TERM term, unsigned long& var)

// With range checking
template<typename T, typename I>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, T& var, I min, I max)

template<typename T, typename I>
bool get(ErlNifEnv* env, ERL_NIF_TERM term, T& var, I max) // For unsigned

// Floating point
bool get(ErlNifEnv* env, ERL_NIF_TERM term, double& var)

// Boolean
bool get(ErlNifEnv* env, ERL_NIF_TERM term, bool& var)

// atom
bool get(ErlNifEnv* env, ERL_NIF_TERM term, atom& var)

// Strings
bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
```

**Usage:**
```cpp
// Integer conversion
int value;
if (nifpp::get(env, term, value)) {
    std::cout << "Got integer: " << value << std::endl;
}

// Range-checked integer
unsigned int port;
if (nifpp::get(env, term, port, 1, 65535)) {
    std::cout << "Valid port: " << port << std::endl;
}

// String conversion
std::string str;
if (nifpp::get(env, term, str)) {
    std::cout << "Got string: " << str << std::endl;
}

// Boolean conversion
bool flag;
if (nifpp::get(env, term, flag)) {
    std::cout << "Flag is: " << (flag ? "true" : "false") << std::endl;
}
```

#### Container Types

```cpp
// Vectors, lists, deques, sets
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::vector<T>&        var)
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::list<T>&          var)
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::deque<T>&         var)
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::set<T>&           var)
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_set<T>& var)

// Arrays (fixed size)
template<typename T, size_t N> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::array<T, N>& var)

// Tuples
template<typename ...Ts> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::tuple<Ts...>& var)
template<typename T1, typename T2> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::pair<T1, T2>& var)

// Maps (if supported)
#if NIFPP_HAS_MAPS
template<typename TK, typename TV> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::map<TK,TV>& var)
template<typename TK, typename TV> bool get(ErlNifEnv* env, ERL_NIF_TERM term, std::unordered_map<TK,TV>& var)
#endif
```

**Usage:**
```cpp
// Vector of integers
std::vector<int> numbers;
if (nifpp::get(env, term, numbers))
    for (int n : numbers)
        std::cout << n << " ";

// Tuple conversion
std::tuple<std::string, int, bool> data;
if (nifpp::get(env, term, data)) {
    auto [name, count, active] = data;
    std::cout << name << ": " << count << " (active: " << active << ")" << std::endl;
}

// Map conversion
std::map<std::string, int> config;
if (nifpp::get(env, term, config))
    for (const auto& [key, value] : config)
        std::cout << key << " = " << value << std::endl;
```

### make() Functions

Convert C++ types to Erlang terms.

#### Basic Types

```cpp
// Integers
template<typename T> TERM make(ErlNifEnv* env, const T var)  // For all integer types

// Floating point
TERM make(ErlNifEnv* env, const double var)

// Boolean
TERM make(ErlNifEnv* env, const bool var)

// Strings (creates charlists)
TERM make(ErlNifEnv* env, const char*        var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)
TERM make(ErlNifEnv* env, const std::string& var, ErlNifCharEncoding encoding = ERL_NIF_LATIN1)

// Binary strings
TERM make(ErlNifEnv* env, const binary_string& v)
```

**Usage:**
```cpp
// Create integer term
nifpp::TERM int_term = nifpp::make(env, 42);

// Create string term (charlist)
nifpp::TERM str_term = nifpp::make(env, std::string("Hello"));

// Create binary term
nifpp::TERM bin_term = nifpp::make(env, "Hello"_b);

// Create boolean term
nifpp::TERM bool_term = nifpp::make(env, true);
```

#### Container Types

```cpp
// All container types supported by get() are also supported by make()
template<typename Container> TERM make(ErlNifEnv* env, const Container& var)
```

**Usage:**
```cpp
// Vector to list
std::vector<int> numbers = {1, 2, 3, 4, 5};
nifpp::TERM list_term = nifpp::make(env, numbers);

// Tuple creation
auto data = std::make_tuple("user", 123, true);
nifpp::TERM tuple_term = nifpp::make(env, data);

// Map creation
std::map<std::string, int> config = {{"port", 8080}, {"timeout", 5000}};
nifpp::TERM map_term = nifpp::make(env, config);
```

### make_tuple() Function

Convenient function to create tuples directly from arguments.

```cpp
template<typename... Args>
TERM make_tuple(ErlNifEnv* env, Args&&... args)
```

> NOTE: Implication for binaries - binaries need to be moved into terms to
> transfer the ownership either by using the move constructor directly, or
> via explicit `std::move()` call.

**Usage:**
```cpp
// Create {ok, 42, "success"} tuple directly
nifpp::TERM result = nifpp::make_tuple(env, nifpp::am_ok, 42, "success");

// Tuple {ok, ~"binary"} containing binary (must use std::move or convert to TERM first)
nifpp::binary data("binary");

// Explicit binary std::move()
nifpp::TERM result1 = nifpp::make_tuple(env, nifpp::am_ok, std::move(data));
// Implicit binary move constructor
nifpp::TERM result2 = nifpp::make_tuple(env, nifpp::am_ok, binary("binary"));

// Or convert binary to TERM first
nifpp::binary bin("more_data");
nifpp::TERM bin_term = nifpp::make(env, bin);
nifpp::TERM result2  = nifpp::make_tuple(env, nifpp::am_ok, bin_term);
```

### Convenience get() Functions

**Throwing versions of get() for cleaner code.**

```cpp
template<typename T> T    get(ErlNifEnv* env, ERL_NIF_TERM term)               // Returns value or throws
template<typename T> void get_throws(ErlNifEnv* env, ERL_NIF_TERM term, T &t)  // Throws on failure
```

**Usage:**
```cpp
try {
    // Get value or throw exception
    int value        = nifpp::get<int>(env, term);
    std::string name = nifpp::get<std::string>(env, name_term);

    // Alternative syntax
    double price;
    nifpp::get_throws(env, price_term, price);

} catch (const std::invalid_argument& e) {
    // Handle conversion error
    return nifpp::make(env, nifpp::am_error);
}
```

**Non-throwing versions of get()**

```cpp
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, T& value) // Doesn't throw
```

**Usage:**
```cpp
int value;
if (!get(env, term, value))
    return enif_make_badarg(env);
```

## 🡅[Container Operations](#table-of-contents)

### list_for_each()

Iterate over Erlang lists with a callback function.

```cpp
// `T` is the type of item in the list, which can be a polymorphic TERM
template<typename T, typename F>
bool list_for_each(ErlNifEnv* env, ERL_NIF_TERM term, const F& f)
```

**Usage:**
```cpp
// Process each element in a list
bool success = nifpp::list_for_each<int>(env, list_term,
    [&](int item) {
        std::cout << "Item: " << item << std::endl;
        // Process item
    });

if (!success) {
    // Handle error (not a list or conversion failed)
}
```

### map_for_each()

Iterate over Erlang maps with a callback function (if maps are supported).

```cpp
#if NIFPP_HAS_MAPS
template<typename TK, typename TV, typename F>
bool map_for_each(ErlNifEnv* env, ERL_NIF_TERM term, const F& f)
#endif
```

**Usage:**
```cpp
#if NIFPP_HAS_MAPS
// Process each key-value pair in a map
bool success = nifpp::map_for_each<std::string, int>(env, map_term,
    [&](std::string key, int value) {
        std::cout << key << " -> " << value << std::endl;
        // Process key-value pair
    });
#endif
```

### add_to_map()

Add key-value pairs to an existing map.

```cpp
#if NIFPP_HAS_MAPS
template<typename TK, typename TV, typename... Pairs>
bool add_to_map(ErlNifEnv* env, TERM &map, const std::pair<TK,TV>& var, Pairs&&... pairs)
#endif
```

**Usage:**
```cpp
#if NIFPP_HAS_MAPS
// Start with empty map
nifpp::TERM map(enif_make_new_map(env));

// Add pairs
auto pair1 = std::make_pair(std::string("key1"), 10);
auto pair2 = std::make_pair(std::string("key2"), 20);

if (nifpp::add_to_map(env, map, pair1, pair2)) {
    // Successfully added pairs to map
}

nifpp::TERM another_map;
nifpp::add_to_map(env, another_map, std::map<nifpp::str_atom, int>{
    {"a1" , 1},
    {"b1" , 2}
});

#endif
```

## 🡅[Resource Management](#table-of-contents)

### resource_ptr<T>

Smart pointer for Erlang resources with automatic reference counting.

```cpp
template<class T>
class resource_ptr {
public:
    T* get() const;
    T& operator*();
    T* operator->();
    operator bool() const;
    void reset();
    void reset(T* rhs);
};
```

### register_resource<T>()

Register a C++ type as an Erlang resource.

```cpp
template<typename T>
bool register_resource(ErlNifEnv* env,
                       const char* name,
                       ErlNifResourceFlags flags =
                         ErlNifResourceFlags(ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER),
                       ErlNifResourceFlags* tried = nullptr)
```

**Usage:**
```cpp
// Define your resource class
class MyResource {
public:
    MyResource(int value) : value_(value) {}
    int get_value() const { return value_; }
private:
    int value_;
};

// In your NIF load function
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    nifpp::initialize_known_atoms(env);

    return nifpp::register_resource<MyResource>(env, "MyResource") ? 0 : 1;
}
```

### construct_resource<T>()

Create a new resource instance.

```cpp
template<typename T, typename... Args>
resource_ptr<T> construct_resource(Args&&... args)
```

**Usage:**
```cpp
// Create resource
auto resource = nifpp::construct_resource<MyResource>(42);

// Convert to Erlang term
nifpp::TERM resource_term = nifpp::make(env, resource);

// Use resource
if (resource) {
    int value = resource->get_value();
}
```

### construct_resource_with_events<T>()

Create a resource with event handlers.

```cpp
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
    );
    ...
};

// Create a resource with std::function-based callbacks
template<typename T, typename... Args>
resource_ptr<T> construct_resource_with_events(resource_events<T> const& events, Args&&... args)
```

**Usage:**
```cpp
// Define event handlers
auto events = nifpp::resource_events<MyResource>(
    // On down event
    [](tracetype* obj, ErlNifEnv*, ErlNifPid* down_pid, ErlNifMonitor*) {
        obj->monitor_triggered(down_pid);
    }
);

// Create resource with events
auto resource = nifpp::construct_resource_with_events<MyResource>(events, 42);
```

Registration of a resource with events calls the `enif_open_resource_type_x`
with passed callbacks:

```cpp
ErlNifResourceTypeInit init{
    .dtor    = &detail::resource_dtor<T>,
    .stop    = &detail::resource_stop<T>,
    .down    = &detail::resource_down<T>,
    .members = 4,
    .dyncall = &detail::resource_dyncall<T>
};

return enif_open_resource_type_x(env, name, &init, flags, tried);
```


### Resource get() and make()

```cpp
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, resource_ptr<T>& var)
template<typename T> bool get(ErlNifEnv* env, ERL_NIF_TERM term, T*& var)
template<typename T> TERM make(ErlNifEnv* env, const resource_ptr<T>& var)
```

**Usage:**
```cpp
// Extract resource from term
nifpp::resource_ptr<MyResource> resource;
if (nifpp::get(env, term, resource)) {
    int value = resource->get_value();
}

// Raw pointer extraction
MyResource* raw_ptr;
if (nifpp::get(env, term, raw_ptr)) {
    int value = raw_ptr->get_value();
}
```

### make_resource_binary()

Create a binary backed by a resource.

```cpp
template<typename T>
TERM make_resource_binary(ErlNifEnv* env, const resource_ptr<T>& var, const void* data, size_t size)
```

**Usage:**
```cpp
auto resource = nifpp::construct_resource<MyResource>(42);
const char* data = "binary data";
nifpp::TERM binary_term = nifpp::make_resource_binary(env, resource, data, strlen(data));
```

## 🡅[Process and Messaging](#table-of-contents)

### self()

Get the PID of the calling process.

```cpp
ErlNifPid self(ErlNifEnv* env)
```

**Usage:**
```cpp
ErlNifPid my_pid = nifpp::self(env);
```

### send()

Send a message to a process.

```cpp
bool send(ErlNifEnv* env, ErlNifPid* to, ErlNifEnv* msg_env, TERM msg)
```

**Usage:**
```cpp
ErlNifPid target_pid = /* ... */;
nifpp::msg_env msg_environment;
nifpp::TERM message = nifpp::make_tuple(msg_environment, nifpp::am_ok, "Hello");

if (nifpp::send(env, &target_pid, msg_environment, message)) {
    // Message sent successfully
}
```

### msg_env

RAII wrapper for message environments.

```cpp
class msg_env {
public:
    msg_env();
    ~msg_env();

    operator ErlNifEnv*() const;
    ErlNifEnv* release();  // For APIs that consume the environment
};
```

**Usage:**
```cpp
// Create a new message environment
nifpp::msg_env msg_env;

// Create terms in message environment
nifpp::TERM message = nifpp::make_tuple(msg_env, nifpp::am_notification, data);

// Send message (msg_env is automatically freed after this scope)
nifpp::send(env, &pid, msg_env, message);
```

### enif_select Wrappers

Type-safe wrappers for enif_select functions.

```cpp
template<typename T>
int select_read(ErlNifEnv* env, ErlNifEvent event, T* obj, const ErlNifPid* pid, TERM msg, msg_env& env_to_consume)

template<typename T>
int select_write(ErlNifEnv* env, ErlNifEvent event, T* obj, const ErlNifPid* pid, TERM msg, msg_env& env_to_consume)

template<typename T>
int select_stop(ErlNifEnv* env, ErlNifEvent event, T* obj)
```

**Usage:**
```cpp
// Select for read events
MyResource* resource = /* ... */;
ErlNifEvent fd = /* file descriptor */;
ErlNifPid pid = nifpp::self(env);
nifpp::msg_env msg_env;
nifpp::TERM msg = nifpp::make_tuple(msg_env, nifpp::am_ready, fd);

int result = nifpp::select_read(env, fd, resource, &pid, msg, msg_env);
```

## 🡅[Type Checking Functions](#table-of-contents)

Convenient wrappers for Erlang type checking functions.

```cpp
bool is_atom(ErlNifEnv* env, TERM term)
bool is_binary(ErlNifEnv* env, TERM term)
bool is_ref(ErlNifEnv* env, TERM term)
bool is_fun(ErlNifEnv* env, TERM term)
bool is_pid(ErlNifEnv* env, TERM term)
bool is_port(ErlNifEnv* env, TERM term)
bool is_empty_list(ErlNifEnv* env, TERM term)
bool is_list(ErlNifEnv* env, TERM term)
bool is_tuple(ErlNifEnv* env, TERM term)
bool is_number(ErlNifEnv* env, TERM term)
bool is_map(ErlNifEnv* env, TERM term)
bool is_exception(ErlNifEnv* env, TERM term)
bool is_identical(TERM lhs, TERM rhs)
```

**Usage:**
```cpp
if (nifpp::is_atom(env, term)) {
    nifpp::str_atom atom_val;
    nifpp::get(env, term, atom_val);
    std::cout << "Got atom: " << atom_val << std::endl;
}

if (nifpp::is_list(env, term)) {
    std::vector<int> list_val;
    if (nifpp::get(env, term, list_val)) {
        std::cout << "List has " << list_val.size() << " elements" << std::endl;
    }
}

if (nifpp::is_tuple(env, term)) {
    std::tuple<std::string, int> tuple_val;
    if (nifpp::get(env, term, tuple_val)) {
        auto [name, value] = tuple_val;
        std::cout << "Tuple: " << name << " = " << value << std::endl;
    }
}
```

## 🡅[Exception Handling](#table-of-contents)

### badarg()

Create a badarg exception term.

```cpp
TERM badarg(ErlNifEnv* env)
```

**Usage:**
```cpp
// Return badarg on invalid input
if (!nifpp::get(env, argv[0], input_value)) {
    return nifpp::badarg(env);
}
```

### raise_exception()

Create custom exception terms.

```cpp
template <typename T>
TERM raise_exception(ErlNifEnv* env, T&& arg)

template <typename T, typename... Args>
TERM raise_exception(ErlNifEnv* env, T&& arg, Args&&... args)
```

**Usage:**
```cpp
// Raise simple exception
return nifpp::raise_exception(env, "Invalid input format");

// Raise structured exception
return nifpp::raise_exception(env, nifpp::am_error, "file_not_found", filename);

// This creates: {error, file_not_found, Filename}
```

## Complete NIF Example

Here's a complete example showing how to use NIFPP in a real NIF:

```cpp
#include "nifpp.h"

// Define custom atoms
NIFPP_ADD_KNOWN_ATOM(am_multiply);
NIFPP_ADD_KNOWN_ATOM(am_add);

// Resource example
class Calculator {
public:
    Calculator(double initial = 0.0) : value_(initial) {}

    void add(double x) { value_ += x; }
    void multiply(double x) { value_ *= x; }
    double get() const { return value_; }

private:
    double value_;
};

// NIF functions
static ERL_NIF_TERM create_calculator_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        double initial = 0.0;
        if (argc > 0) {
            if (!nifpp::get(env, argv[0], initial)) {
                return nifpp::badarg(env);
            }
        }

        auto calc = nifpp::construct_resource<Calculator>(initial);
        return nifpp::make(env, calc);

    } catch (const std::exception& e) {
        return nifpp::raise_exception(env, "Failed to create calculator");
    }
}

static ERL_NIF_TERM calculate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        // Extract calculator resource
        nifpp::resource_ptr<Calculator> calc;
        if (!nifpp::get(env, argv[0], calc)) {
            return nifpp::badarg(env);
        }

        // Extract operation and operand
        nifpp::str_atom operation;
        double operand;

        if (!nifpp::get(env, argv[1], operation) ||
            !nifpp::get(env, argv[2], operand)) {
            return nifpp::badarg(env);
        }

        // Perform operation
        if (operation == "add") {
            calc->add(operand);
        } else if (operation == "multiply") {
            calc->multiply(operand);
        } else {
            return nifpp::raise_exception(env, "Unknown operation", operation);
        }

        // Return new value
        return nifpp::make_tuple(env, nifpp::am_ok, calc->get());

    } catch (const std::exception& e) {
        return nifpp::raise_exception(env, "Calculation failed");
    }
}

static ERL_NIF_TERM process_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        std::vector<int> input_list;
        if (!nifpp::get(env, argv[0], input_list)) {
            return nifpp::badarg(env);
        }

        // Process list - double each element
        std::vector<int> result_list;
        for (int value : input_list) {
            result_list.push_back(value * 2);
        }

        return nifpp::make_tuple(env, nifpp::am_ok, nifpp::make(env, result_list));

    } catch (const std::exception& e) {
        return nifpp::raise_exception(env, "List processing failed");
    }
}

// NIF function array
static ErlNifFunc nif_funcs[] = {
    {"create_calculator", 0, create_calculator_nif, 0},
    {"create_calculator", 1, create_calculator_nif, 0},
    {"calculate", 3, calculate_nif, 0},
    {"process_list", 1, process_list_nif, 0}
};

// NIF initialization
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    // Initialize known atoms
    nifpp::initialize_known_atoms(env);

    // Register resource types
    if (!nifpp::register_resource<Calculator>(env, "Calculator")) {
        return 1;
    }

    return 0;
}

ERL_NIF_INIT(my_nif, nif_funcs, load, NULL, NULL, NULL)
```

## Best Practices

1. **Always initialize known atoms** in your NIF's `load()` function
2. **Use RAII types** like `binary` and `msg_env` for automatic memory management
3. **Check return values** of `get()` functions and return `badarg()` on failure
4. **Use exceptions** for error handling in complex operations
5. **Register resources** in the `load()` function
6. **Use custom atoms** with `NIFPP_ADD_KNOWN_ATOM` for better performance
7. **Handle binary ownership** carefully with `make_tuple()` - use `std::move()` or convert to `TERM` first

## Thread Safety

NIFPP follows the same thread safety rules as the Erlang NIF API:
- Each NIF call gets its own `ErlNifEnv*` that should not be shared between threads
- Resource objects can be accessed from multiple threads if properly synchronized
- Message environments (`msg_env`) are not thread-safe

## Memory Management

NIFPP uses RAII principles:
- `binary` objects automatically manage their memory
- `resource_ptr<T>` provides reference-counted resource management
- `msg_env` automatically frees message environments
- All containers handle their own memory allocation/deallocation