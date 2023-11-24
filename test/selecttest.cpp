//
// tuple_twiddle_cpp.cpp - Demonstrate nifpp tuple manipulation
//
#include "nifpp.h"
#include <functional>
#include <sys/timerfd.h>
#include <unistd.h>
#include <cstdint>

using std::make_tuple;
using std::ref;

using namespace nifpp;

static atom am_select;
static atom am_timer;

static ERL_NIF_TERM set_timer(ErlNifEnv* env, int64_t timeout, resource_ptr<int>& fdptr)
{
    itimerspec ts{.it_value = timespec{timeout / 1000, (timeout % 1000) * 1'000'000}};

    if (timerfd_settime(*fdptr, 0, &ts, nullptr) < 0)
        return nifpp::raise_exception(env, "cannot set time", strerror(errno), __LINE__);

    auto msg_env = enif_alloc_env();
    auto msg = make(msg_env, std::make_pair(am_select, am_timer));

    int r;

    if ((r = enif_select_read(env, (ErlNifEvent)((long)*fdptr), &fdptr, nullptr, msg, msg_env)) < 0) {
        enif_free_env(msg_env);
        if (r & ERL_NIF_SELECT_INVALID_EVENT)
            return nifpp::make_tuple(env, am_error, "argument event is not a valid OS event object"_b);
        if (r & ERL_NIF_SELECT_FAILED)
            return nifpp::make_tuple(env, am_error, "system call failed to add the event object to the poll set"_b);
        return nifpp::make_tuple(env, am_error, "error in enif_select_read"_b);
    }

    return make_tuple(env, am_ok, fdptr);
}

static ERL_NIF_TERM create_timer_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    int64_t timeout = -1;
    assert(argc <= 1);
    if (argc == 1 && (!nifpp::get(env, argv[0], timeout) || timeout < 0))
        return enif_make_badarg(env);

    int fd = timerfd_create(CLOCK_REALTIME, TFD_NONBLOCK | TFD_CLOEXEC);

    if (fd < 0)
        return nifpp::raise_exception(env, "cannot create timer", strerror(errno), __LINE__);

    ResourceStopEvent<int> stop = [](int* fd_ptr, ErlNifEnv*, ErlNifEvent, [[maybe_unused]] int is_direct) {
        [[maybe_unused]] auto res = fd_ptr ? close(*fd_ptr) : 0;
        //fprintf(stderr, "==> Closing timer resource %d: %d%s\r\n",
        //    fd_ptr ? *fd_ptr : 0, res, is_direct ? " (direct)" : " (indirect)");
    };

    auto ptr = construct_resource_with_events<int>(resource_events<int>(ResourceDownEvent<int>(nullptr), stop), fd);

    return timeout == -1 ? make_tuple(env, am_ok, ptr) : set_timer(env, timeout, ptr);
}

static ERL_NIF_TERM set_timer_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 2);

    resource_ptr<int> fdptr;
    if (!nifpp::get(env, argv[0], fdptr)) [[unlikely]]
        return enif_make_badarg(env);

    int64_t timeout;
    if (!nifpp::get(env, argv[1], timeout) || timeout < 0) [[unlikely]]
        return enif_make_badarg(env);

    return set_timer(env, timeout, fdptr);
}

static ERL_NIF_TERM delete_timer_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    resource_ptr<int> ptr;
    if (!nifpp::get(env, argv[0], ptr))
        return enif_make_badarg(env);

    int r;

    if ((r = enif_select(env, (ErlNifEvent)((long)*ptr), ERL_NIF_SELECT_STOP, &ptr, nullptr, am_undefined)) < 0) {
        if (r & ERL_NIF_SELECT_INVALID_EVENT)
            return nifpp::make_tuple(env, am_error, "argument event is not a valid OS event object"_b);
        if (r & ERL_NIF_SELECT_FAILED)
            return nifpp::make_tuple(env, am_error, "system call failed to add the event object to the poll set"_b);
        return nifpp::make_tuple(env, am_error, "error in enif_select_read"_b);
    }

    /*
    if (r & ERL_NIF_SELECT_STOP_CALLED)
        return make_tuple(env, am_ok, "stop callback was called directly by enif_select"_b);
    if (r & ERL_NIF_SELECT_STOP_SCHEDULED)
        return make_tuple(env, am_ok, "stop callback was scheduled to run on some other thread or later by this thread"_b);
    if (r & ERL_NIF_SELECT_READ_CANCELLED)
        return make_tuple(env, am_ok, "read event was cancelled by ERL_NIF_SELECT_CANCEL or ERL_NIF_SELECT_STOP"_b);
    if (r & ERL_NIF_SELECT_WRITE_CANCELLED)
        return make_tuple(env, am_ok, "write event was cancelled by ERL_NIF_SELECT_CANCEL or ERL_NIF_SELECT_STOP"_b);
    */
    return am_ok;
}

static ERL_NIF_TERM read_timer_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    resource_ptr<int> ptr;
    if (!nifpp::get(env, argv[0], ptr))
        return enif_make_badarg(env);

    fprintf(stderr, "Using fd=%d\r\n", *ptr);

    uint64_t value;
    int r = 0;

    while ((r = read(*ptr, reinterpret_cast<char*>(&value), sizeof(value))) == EINTR)
        continue;

    return r == sizeof(value) ? make_tuple(env, am_ok, value) : make_tuple(env, am_error, strerror(errno));
}

static int load(ErlNifEnv* env, [[maybe_unused]] void** priv_data, [[maybe_unused]] ERL_NIF_TERM load_info) {
    nifpp::initialize_known_atoms(env);

    am_select = atom(env, "select");
    am_timer  = atom(env, "timer");

    register_resource<int>(env, "int");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"create_timer", 0, create_timer_nif, 0},
    {"create_timer", 1, create_timer_nif, 0},
    {"delete_timer", 1, delete_timer_nif, 0},
    {"set_timer",    2, set_timer_nif,    0},
    {"read_timer",   1, read_timer_nif,   0},
};

ERL_NIF_INIT(selecttest, nif_funcs, load, NULL, NULL, NULL)
