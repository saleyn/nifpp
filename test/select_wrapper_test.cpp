//
// select_wrapper_test.cpp - Test the new enif_select wrapper functions (Linux-specific)
//
#ifdef __linux__
#include "enif.hpp"
#include <functional>
#include <sys/eventfd.h>
#include <unistd.h>
#include <cstdint>

using std::make_tuple;
using std::ref;

using namespace nifpp;

static atom am_select;
static atom am_test;
static atom am_read;
static atom am_write;
static atom am_stop;

// Test the select_read wrapper
static ERL_NIF_TERM test_select_read_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 1);

    resource_ptr<int> fdptr;
    if (!nifpp::get(env, argv[0], fdptr)) [[unlikely]]
        return enif_make_badarg(env);

    // Create a message environment and message
    msg_env msg_env_obj;
    auto msg = make(msg_env_obj, std::make_pair(am_select, am_read));

    // Test the select_read wrapper
    int result = select_read(env, (ErlNifEvent)((long)*fdptr), fdptr.get(), nullptr, msg, msg_env_obj);

    return make(env, result);
}

// Test the select_write wrapper
static ERL_NIF_TERM test_select_write_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 1);

    resource_ptr<int> fdptr;
    if (!nifpp::get(env, argv[0], fdptr)) [[unlikely]]
        return enif_make_badarg(env);

    // Create a message environment and message
    msg_env msg_env_obj;
    auto msg = make(msg_env_obj, std::make_pair(am_select, am_write));

    // Test the select_write wrapper
    int result = select_write(env, (ErlNifEvent)((long)*fdptr), fdptr.get(), nullptr, msg, msg_env_obj);

    return make(env, result);
}

// Test the select_stop wrapper
static ERL_NIF_TERM test_select_stop_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 1);

    resource_ptr<int> fdptr;
    if (!nifpp::get(env, argv[0], fdptr)) [[unlikely]]
        return enif_make_badarg(env);

    // Test the select_stop wrapper
    int result = select_stop(env, (ErlNifEvent)((long)*fdptr), fdptr.get());

    return make(env, result);
}

// Create an eventfd for testing
static ERL_NIF_TERM create_test_fd_nif(ErlNifEnv* env, [[maybe_unused]] int argc, [[maybe_unused]] const ERL_NIF_TERM argv[])
{
    int fd = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);

    if (fd < 0)
        return nifpp::raise_exception(env, "cannot create eventfd", strerror(errno), __LINE__);

    // Create a resource to hold the file descriptor
    ResourceStopEvent<int> stop = [](int* fd_ptr, ErlNifEnv*, [[maybe_unused]] int event, [[maybe_unused]] int is_direct) {
        if (fd_ptr) {
            close(*fd_ptr);
        }
    };

    auto ptr = construct_resource_with_events<int>(resource_events<int>(ResourceDownEvent<int>(nullptr), stop), fd);

    return make(env, ptr);
}

// Write to eventfd to trigger read events
static ERL_NIF_TERM write_test_fd_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 2);

    resource_ptr<int> fdptr;
    if (!nifpp::get(env, argv[0], fdptr)) [[unlikely]]
        return enif_make_badarg(env);

    uint64_t value;
    if (!nifpp::get(env, argv[1], value)) [[unlikely]]
        return enif_make_badarg(env);

    ssize_t result = write(*fdptr, &value, sizeof(value));

    return result == sizeof(value) ? am_ok : make_tuple(env, am_error, strerror(errno));
}

// Test msg_env move semantics
static ERL_NIF_TERM test_msg_env_move_nif(ErlNifEnv* env, [[maybe_unused]] int argc, [[maybe_unused]] const ERL_NIF_TERM argv[])
{
    // Test move constructor
    msg_env env1;
    TERM term1 = make(env1, 42);

    msg_env env2 = std::move(env1);
    TERM term2 = make(env2, "moved");

    // Test move assignment
    msg_env env3;
    env3 = std::move(env2);
    TERM term3 = make(env3, am_test);

    return make(env, std::make_tuple(term1, term2, term3));
}

static int load(ErlNifEnv* env, [[maybe_unused]] void** priv_data, [[maybe_unused]] ERL_NIF_TERM load_info) {
    nifpp::initialize_known_atoms(env);

    am_select = atom(env, "select");
    am_test   = atom(env, "test");
    am_read   = atom(env, "read");
    am_write  = atom(env, "write");
    am_stop   = atom(env, "stop");

    register_resource<int>(env, "int");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"create_test_fd",      0, create_test_fd_nif,      0},
    {"write_test_fd",       2, write_test_fd_nif,       0},
    {"test_select_read",    1, test_select_read_nif,    0},
    {"test_select_write",   1, test_select_write_nif,   0},
    {"test_select_stop",    1, test_select_stop_nif,    0},
    {"test_msg_env_move",   0, test_msg_env_move_nif,   0},
};

ERL_NIF_INIT(select_wrapper_test, nif_funcs, load, NULL, NULL, NULL)

#else // !__linux__

// Stub implementation for non-Linux platforms
#include "enif.hpp"

static ERL_NIF_TERM unsupported_nif(ErlNifEnv* env, [[maybe_unused]] int argc, [[maybe_unused]] const ERL_NIF_TERM argv[])
{
    return nifpp::make_tuple(env, nifpp::atom(env, "error"), "select_wrapper_test only supported on Linux"_b);
}

static int load(ErlNifEnv* env, [[maybe_unused]] void** priv_data, [[maybe_unused]] ERL_NIF_TERM load_info) {
    nifpp::initialize_known_atoms(env);
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"create_test_fd",      0, unsupported_nif, 0},
    {"write_test_fd",       2, unsupported_nif, 0},
    {"test_select_read",    1, unsupported_nif, 0},
    {"test_select_write",   1, unsupported_nif, 0},
    {"test_select_stop",    1, unsupported_nif, 0},
    {"test_msg_env_move",   0, unsupported_nif, 0},
};

ERL_NIF_INIT(select_wrapper_test, nif_funcs, load, NULL, NULL, NULL)

#endif // __linux__