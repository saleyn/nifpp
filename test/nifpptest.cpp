#include <iostream>
#include <cstdint>
#include <cstring>
using std::cout;
using std::cerr;
using std::endl;

#include <functional>

using std::ref;

#define NIFPP_INTRUSIVE_UNIT_TEST
#include "enif.hpp"

using namespace nifpp;

// Test custom known atoms
NIFPP_ADD_KNOWN_ATOM(am_test_atom)
NIFPP_ADD_KNOWN_ATOM(am_custom_success)
NIFPP_ADD_KNOWN_ATOM(am_my_special_atom)

extern nifpp::resource_ptr<int> get_resource_int(int val);

// list test abstracted over container type and contained element type
template<typename T> // supported container type
nifpp::TERM list2_test(ErlNifEnv* env, ERL_NIF_TERM term)
{
    T container;
    get_throws(env, term, container);

    // append reversed copy of container to itself
    T dup = container;
    for(auto i=dup.rbegin(); i!=dup.rend(); i++)
    {
        //container.push_back(*i);
        container.insert(container.end(), *i);
    }

    return make(env, container);
}

// set test abstracted over container type and contained element type
template<typename T> // supported container type
nifpp::TERM set2_test(ErlNifEnv* env, ERL_NIF_TERM term)
{
    T container;
    get_throws(env, term, container);

    // append reversed copy of container to itself
    T dup = container;
    for(auto i=dup.begin(); i!=dup.end(); i++)
    {
        //container.push_back(*i);
        container.insert(container.end(), *i);
    }

    return make(env, container);
}

// swap keys and values.
template<typename TK, typename TV>
nifpp::TERM mapflip_test(ErlNifEnv* env, ERL_NIF_TERM term)
{
    std::map<TK,TV> inmap;
    std::map<TV,TK> outmap;
    get_throws(env, term, inmap);
    for(auto i=inmap.begin(); i!=inmap.end(); i++)
    {
        outmap[i->second] = i->first;
    }
    return make(env, outmap);
}

// Build a nested map
nifpp::TERM nestedmap_test(ErlNifEnv* env, ERL_NIF_TERM )
{
	std::map<nifpp::str_atom, int> int_map{ {"a", 1} };
    nifpp::TERM outmap = make(env, int_map);

    // Add another entry to map
    add_to_map(env, outmap, std::make_pair(str_atom("b"), "b"));

    // Add a nested map
    std::map<nifpp::str_atom, int> nested_map = {
        {"a1" , 1},
        {"b1" , 2}
    };
    add_to_map(env, outmap, std::make_pair(str_atom("c"), make(env, nested_map)));

    return outmap;
}

// swap keys and values (unordered_map version.  I would templatize the abstract map type if I knew how)
template<typename TK, typename TV>
nifpp::TERM umapflip_test(ErlNifEnv* env, ERL_NIF_TERM term)
{
    std::unordered_map<TK,TV> inmap;
    std::unordered_map<TV,TK> outmap;
    get_throws(env, term, inmap);
    for(auto i=inmap.begin(); i!=inmap.end(); i++)
    {
        outmap[i->second] = i->first;
    }
    return make(env, outmap);
}


class tracetype
{
public:
    static int ctor_cnt;
    static int dtor_cnt;
    static int mon_cnt;
    static int down_cnt;
    tracetype() : pid{} { ctor_cnt++; }
    tracetype(ErlNifEnv* env, ErlNifPid& pid) : pid(pid) {
        ctor_cnt++; mon_cnt++;
        enif_monitor_process(env, this, &pid, nullptr);
    }
    ~tracetype() { dtor_cnt++; }
    static void reset()
    {
        ctor_cnt = 0;
        dtor_cnt = 0;
        mon_cnt  = 0;
        down_cnt = 0;
    }

    void monitor_triggered(ErlNifPid* down_pid) { if (*down_pid == pid) down_cnt++; }

    int x;
    ErlNifPid pid;
};

int tracetype::ctor_cnt;
int tracetype::dtor_cnt;
int tracetype::mon_cnt;
int tracetype::down_cnt;

ERL_NIF_TERM nif_main(ErlNifEnv* env, nifpp::TERM term)
{

    str_atom cmd;
    nifpp::TERM cmddata;
    auto cmdtup=std::tie(cmd,cmddata);
    get_throws(env, term, cmdtup);
    #ifdef DEBUG_OUTPUT
    cout << "cmd = " << cmd << "\r\n";
    #endif
    if(cmd=="atom2")
    {
        str_atom in;
        str_atom out;
        get_throws(env, cmddata, in);
        out = in;
        out += in;
        return make(env,out);
    }
    else if(cmd=="binary2")
    {
        std::string in;
        std::string out;
        get_throws(env, cmddata, in);
        out = in;
        out += in;
        return make_binary(env,out);
    }
    else if(cmd=="binary3")
    {
        uint32_t n;
        get_throws(env, cmddata, n);
        return make(env, binary(n));
    }
    else if(cmd=="string2")
    {
        std::string in;
        std::string out;
        get_throws(env, cmddata, in);
        out = in;
        out += in;
        return make(env,out);
    }
    else if(cmd=="double2")
    {
        double in;
        double out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="int2")
    {
        int in;
        int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="uint2")
    {
        unsigned int in;
        unsigned int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="long2")
    {
        long in;
        long out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
    else if(cmd=="booln")
    {
        bool in;
        bool out;
        get_throws(env, cmddata, in);
        out = !in;
        return make(env,out);
    }
    else if(cmd=="pidcp")
    {
        ErlNifPid in;
        get_throws(env, cmddata, in);
        return make(env,in);
    }
    else if(cmd=="tuple2a")
    {
        // test complex tie() coding
        // double each member of {123, "abc", {xyz, 456}, 500.0}}
        int a;
        std::string b;
        str_atom c;
        int d;
        double e;

        auto tup = make_tuple(ref(a),ref(b),std::tie(c,d),ref(e));
        get_throws(env, cmddata, tup);

        a*=2;
        b = b + b;
        c = c + c;
        d*=2;
        e*=2;

        return make(env, tup);
    }
    else if(cmd=="tuple2b")
    {
        // test simpler tie() coding
        // double each member of {123, "abc", 500.0}
        int a;
        std::string b;
        double e;

        auto tup = tie(a,b,e);
        get_throws(env, cmddata, tup);

        a*=2;
        b = b + b;
        e*=2;

        return make(env, tup);
    }
    else if(cmd=="tuple2c")
    {
        // non-ref tuple test
        // double each member of {123, "abc", {xyz, 456}, 500.0}}

        std::tuple<int, std::string, std::tuple<str_atom, int>, double>  tup;
        get_throws(env, cmddata, tup);

        int &a = std::get<0>(tup);
        std::string &b = std::get<1>(tup);
        str_atom &c = std::get<0>(std::get<2>(tup));
        int &d =  std::get<1>(std::get<2>(tup));
        double &e = std::get<3>(tup);

        a*=2;
        b = b + b;
        c = c + c;
        d*=2;
        e*=2;

        return make(env, tup);
    }
    else if(cmd=="tuple2d")
    {
        // test wrong data passing in a duple
        // double each member of {"abc", 1, 500.0}
        int a;
        std::string b;
        double e;

        auto tup = tie(a,b,e);
        return make(env, get(env, cmddata, tup));
    }
//    else if(cmd=="tuple2d")
//    {
//        // test multi-param get/make
//        // double each member of {123, "abc", 500.0}
//        int a;
//        std::string b;
//        double e;

//        get_throws(env, cmddata, a, b, e);

//        a*=2;
//        b = b + b;
//        e*=2;

//        return make(env, a, b, e);
//    }
//    else if(cmd=="tuple2e")
//    {
//        // test multi-param get/make (nested)
//        // double each member of {123, "abc", {xyz, 456}, 500.0}}
//        int a;
//        std::string b;
//        str_atom c;
//        int d;
//        double e;

//        auto inner_tup = std::tie(c,d);
//        get_throws(env, cmddata, a, b, inner_tup, e);

//        a*=2;
//        b = b + b;
//        c.name = c.name + c.name;
//        d*=2;
//        e*=2;

//        return make(env, a, b, inner_tup, e);
//    }
    else if(cmd=="list2aa") { return list2_test<std::vector   <int> >(env, cmddata); }
    else if(cmd=="list2ab") { return list2_test<std::list     <int> >(env, cmddata); }
    else if(cmd=="list2ac") { return list2_test<std::deque    <int> >(env, cmddata); }
    else if(cmd=="list2ad") { return  set2_test<std::set      <int> >(env, cmddata); }
    else if(cmd=="list2ae") { return list2_test<std::multiset <int> >(env, cmddata); }
    else if(cmd=="list2af") { return  set2_test<std::unordered_set<int> >(env, cmddata); }

    else if(cmd=="list2ba") { return list2_test<std::vector   <nifpp::TERM> >(env, cmddata); }
    else if(cmd=="list2bb") { return list2_test<std::list     <nifpp::TERM> >(env, cmddata); }
    else if(cmd=="list2bc") { return list2_test<std::deque    <nifpp::TERM> >(env, cmddata); }
    else if(cmd=="list2bd") { return  set2_test<std::set      <nifpp::TERM> >(env, cmddata); }
    else if(cmd=="list2be") { return list2_test<std::multiset <nifpp::TERM> >(env, cmddata); }
    else if(cmd=="list2bf") { return  set2_test<std::unordered_set<nifpp::TERM> >(env, cmddata); }

    else if(cmd=="list2ca") { return list2_test<std::vector   <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cb") { return list2_test<std::list     <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cc") { return list2_test<std::deque    <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2cd") { return  set2_test<std::set      <std::tuple<int, std::string> > >(env, cmddata); }
    else if(cmd=="list2ce") { return list2_test<std::multiset <std::tuple<int, std::string> > >(env, cmddata); }
    //else if(cmd=="list2cf") { return  set2_test<std::unordered_set<std::tuple<int, std::string> > >(env, cmddata); }

    // increment 2nd element of std::array<int, 5>
    else if(cmd=="stdarray_inc2")
    {
        std::array<int, 5> array;
        get_throws(env, cmddata, array);
        array[2]++;
        return make(env, array);
    }

    // copy 3rd elemend to 2nd  of std::array<ERL_NIF_TERM, 5>
    else if(cmd=="stdarray_cp32")
    {
        std::array<ERL_NIF_TERM, 5> array;
        get_throws(env, cmddata, array);
        array[2]=array[3];
        return make(env, array);
    }

    else if(cmd=="mapflipaa") { return  mapflip_test<nifpp::TERM, nifpp::TERM>(env, cmddata); }
    else if(cmd=="mapflipab") { return umapflip_test<nifpp::TERM, nifpp::TERM>(env, cmddata); }

    else if(cmd=="mapflipba") { return  mapflip_test<nifpp::str_atom, int>(env, cmddata); }
    else if(cmd=="mapflipbb") { return umapflip_test<nifpp::str_atom, int>(env, cmddata); }

    else if(cmd=="nestedmap") { return nestedmap_test(env, cmddata); }

    // basic resource testing
    else if(cmd=="makeresint")
    {
        int num;
        get_throws(env, cmddata, num);

        auto ptr = construct_resource<int>(num);
        return make(env, ptr); // make resource term
    }
    else if(cmd=="incresint")
    {
        int *rawptr;
        get_throws(env, cmddata, rawptr);
        (*rawptr)++;
        return make(env, *rawptr); // return value of int
    }
    else if(cmd=="decresint")
    {
        resource_ptr<int> ptr;
        get_throws(env, cmddata, ptr);
        (*ptr)--;
        return make(env, *ptr); // return value of int
    }
    else if(cmd=="makeresint_ext")
    {
        int num;
        get_throws(env, cmddata, num);

        auto ptr = get_resource_int(num);
        return make(env, ptr); // make resource term
    }

    // verify resource destruction
    else if(cmd=="tracetype_reset")
    {
        tracetype::reset();
        return make(env,str_atom("ok"));
    }
    else if(cmd=="tracetype_create")
    {
        return make(env, construct_resource<tracetype>());
    }
    else if(cmd=="tracetype_mon_create")
    {
        ErlNifPid pid;
        get_throws(env, cmddata, pid);

        resource_events<tracetype> events(
            [](tracetype* obj, ErlNifEnv*, ErlNifPid* down_pid, ErlNifMonitor*) {
                obj->monitor_triggered(down_pid);
            });
        return make(env, construct_resource_with_events<tracetype>(events, env, pid));
    }
    else if(cmd=="tracetype_getcnts")
    {
        return make(env, std::make_tuple(tracetype::ctor_cnt,
            tracetype::dtor_cnt, tracetype::mon_cnt, tracetype::down_cnt));
    }

    // test binaries
    else if(cmd=="bin2")
    {
        ErlNifBinary ebin;
        get_throws(env, cmddata, ebin);
        binary newbin(ebin.size);
        std::memcpy(newbin.data,           ebin.data, ebin.size);
        if (!newbin.realloc(ebin.size*2))
            return make(env, str_atom("failed_to_realloc"));
        std::memcpy(newbin.data+ebin.size, ebin.data, ebin.size);

        // make sure these give compile errors:
        //binary bincopy = newbin; //error
        //binary bincopy2(newbin); //error
        //binary bincopy3(std::move(newbin)); //error

        return make(env, newbin);
    }

    // verify binary destruction
    else if(cmd=="binary_release_counter_reset")
    {
        binary::release_counter=0;
        return make(env,str_atom("ok"));
    }
    else if(cmd=="binary_release_counter_get")
    {
        return make(env,binary::release_counter);
    }
    else if(cmd=="bina")
    {
        binary a(10);
        binary b(20);
        binary c(20);
        binary d(20);
        make(env, b);
        return make(env,str_atom("ok"));
        // expect 3 release callss
    }
    else if(cmd=="badarg")
    {
        return badarg(env);
    }
    else if(cmd=="raise_exception")
    {
        if (cmddata == am_nil)
            return raise_exception(env, am_error, "exception");
        else
            return raise_exception(env, cmddata);
    }
    // Test new binary ok() and operator bool() methods
    else if(cmd=="binary_ok_test")
    {
        int size;
        get_throws(env, cmddata, size);

        binary bin(size);
        bool is_ok = bin.ok();
        bool bool_cast = static_cast<bool>(bin);

        return make(env, std::make_tuple(is_ok, bool_cast));
    }
    // Test binary allocation failure (simulate with huge size)
    else if(cmd=="binary_fail_test")
    {
        // Try to allocate a very large binary that should fail
        binary bin(SIZE_MAX - 1);
        bool is_ok = bin.ok();
        bool bool_cast = static_cast<bool>(bin);

        return make(env, std::make_tuple(is_ok, bool_cast));
    }
    // Test PID inequality operator
    else if(cmd=="pid_neq_test")
    {
        ErlNifPid pid1, pid2;
        auto pidtuple = std::tie(pid1, pid2);
        get_throws(env, cmddata, pidtuple);

        bool are_equal = (pid1 == pid2);
        bool are_not_equal = (pid1 != pid2);

        return make(env, std::make_tuple(are_equal, are_not_equal));
    }
    // Test Monitor inequality operator
    else if(cmd=="monitor_neq_test")
    {
        // Since we don't have get() support for ErlNifMonitor directly,
        // we'll create two monitors and test the inequality operator
        ErlNifMonitor mon1, mon2;

        // Initialize monitors (they start with some default state)
        memset(&mon1, 0, sizeof(mon1));
        memset(&mon2, 1, sizeof(mon2)); // Different pattern

        bool are_equal = (mon1 == mon2);
        bool are_not_equal = (mon1 != mon2);

        return make(env, std::make_tuple(are_equal, are_not_equal));
    }
    // Test self() function
    else if(cmd=="self_test")
    {
        ErlNifPid my_pid = self(env);
        return make(env, my_pid);
    }
    // Test msg_env class
    else if(cmd=="msg_env_test")
    {
        // Create result tuple components by copying from message environments
        TERM copied_term1, copied_term2, copied_term3;

        {
            // Scope 1: Test basic msg_env usage
            msg_env env1;
            TERM term1 = make(env1, 42);
            copied_term1 = TERM(enif_make_copy(env, term1));
            // env1 destructor will be called here, but copied_term1 is safe
        }

        {
            // Scope 2: Test with different msg_env
            msg_env env2;
            TERM term2 = make(env2, str_atom("test"));
            copied_term2 = TERM(enif_make_copy(env, term2));
            // env2 destructor will be called here, but copied_term2 is safe
        }

        {
            // Scope 3: Test move constructor
            msg_env env1_original;
            msg_env env3 = std::move(env1_original);  // env1_original.m_env is now nullptr
            TERM term3 = make(env3, "hello");
            copied_term3 = TERM(enif_make_copy(env, term3));
            // env3 destructor will be called here, but copied_term3 is safe
            // env1_original destructor is safe (m_env is nullptr)
        }

        // Return a tuple with the copied terms - all source environments are now freed
        return make(env, std::make_tuple(copied_term1, copied_term2, copied_term3));
    }
    // Test send() function
    else if(cmd=="send_test")
    {
        ErlNifPid target_pid;
        TERM message;
        auto sendtuple = std::tie(target_pid, message);
        get_throws(env, cmddata, sendtuple);

        msg_env msg_env_obj;
        TERM copied_msg = make(msg_env_obj, message);

        bool send_result = send(env, &target_pid, msg_env_obj, copied_msg);

        return make(env, send_result);
    }
    // Test resource type with std::function callbacks (new ResourceDownEvent type)
    else if(cmd=="resource_function_callback_test")
    {
        ErlNifPid pid;
        get_throws(env, cmddata, pid);

        // Create a resource with std::function-based callbacks
        resource_events<tracetype> events(
            [](tracetype* obj, ErlNifEnv*, ErlNifPid* down_pid, ErlNifMonitor*) {
                obj->monitor_triggered(down_pid);
            });

        auto res_ptr = construct_resource_with_events<tracetype>(events, env, pid);
        return make(env, res_ptr);
    }
    // Test new range-checking get() functions for int
    else if(cmd=="int_range_test")
    {
        int value;
        int min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        int result;
        bool success = get(env, make(env, value), result, min, max);

        uint32_t umin = uint32_t(min), umax = uint32_t(max);
        success &= get(env, make(env, value), result, umin, umax);

        return make(env, std::make_tuple(success, result));
    }
    // Test new range-checking get() functions for unsigned int
    else if(cmd=="uint_range_test")
    {
        unsigned int value;
        unsigned int min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        unsigned int result;
        bool success1 = get(env, make(env, value), result, min, max);
        bool success2 = get(env, make(env, value), result, max);
        return make_tuple(env, success1 && success2, result);
    }
    // Test new range-checking get() functions for long
    else if(cmd=="long_range_test")
    {
        long value;
        long min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        long result;
        bool success = get(env, make(env, value), result, min, max);
        return make(env, std::make_tuple(success, result));
    }
    // Test new range-checking get() functions for unsigned long
    else if(cmd=="ulong_range_test")
    {
        unsigned long value;
        unsigned long min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        unsigned long result;
        bool success = get(env, make(env, value), result, min, max);
        return make(env, std::make_tuple(success, result));
    }
    // Test new range-checking get() functions for ErlNifSInt64
    else if(cmd=="int64_range_test")
    {
        ErlNifSInt64 value;
        ErlNifSInt64 min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        ErlNifSInt64 result;
        bool success = get(env, make(env, value), result, min, max);
        return make(env, std::make_tuple(success, result));
    }
    // Test new range-checking get() functions for ErlNifUInt64
    else if(cmd=="uint64_range_test")
    {
        ErlNifUInt64 value;
        ErlNifUInt64 min, max;
        auto rangetuple = std::tie(value, min, max);
        get_throws(env, cmddata, rangetuple);

        ErlNifUInt64 result;
        bool success = get(env, make(env, value), result, min, max);
        return make(env, std::make_tuple(success, result));
    }
    // Test that demonstrates template consolidation would work
    else if(cmd=="consolidation_demo_test")
    {
        // This test shows that all integer types can work through same interface
        // Currently we have separate functions, but templates could unify this

        int test_cases = 0;
        int passed_cases = 0;

        // Test int
        int i_val = 42;
        int i_result;
        if (get(env, make(env, i_val), i_result) && i_result == i_val) passed_cases++;
        test_cases++;

        // Test unsigned int
        unsigned int ui_val = 42u;
        unsigned int ui_result;
        if (get(env, make(env, ui_val), ui_result) && ui_result == ui_val) passed_cases++;
        test_cases++;

        // Test long
        long l_val = 42L;
        long l_result;
        if (get(env, make(env, l_val), l_result) && l_result == l_val) passed_cases++;
        test_cases++;

        // Test range checking across types
        if (get(env, make(env, 50), i_result, 10, 100) && i_result == 50) passed_cases++;
        test_cases++;

        if (get(env, make(env, 50u), ui_result, 10u, 100u) && ui_result == 50u) passed_cases++;
        test_cases++;

        return make(env, std::make_tuple(test_cases, passed_cases));
    }
    // Test container consolidation functionality
    else if(cmd=="container_consolidation_test")
    {
        int test_cases = 0;
        int passed_cases = 0;

        // Test vector consolidation
        std::vector<int> vec_in = {1, 2, 3, 4, 5};
        TERM vec_term = make(env, vec_in);
        std::vector<int> vec_out;
        if (get(env, vec_term, vec_out) && vec_out == vec_in) passed_cases++;
        test_cases++;

        // Test list consolidation
        std::list<int> list_in = {10, 20, 30};
        TERM list_term = make(env, list_in);
        std::list<int> list_out;
        if (get(env, list_term, list_out) &&
            std::equal(list_in.begin(), list_in.end(), list_out.begin())) passed_cases++;
        test_cases++;

        // Test deque consolidation
        std::deque<int> deque_in = {100, 200, 300, 400};
        TERM deque_term = make(env, deque_in);
        std::deque<int> deque_out;
        if (get(env, deque_term, deque_out) &&
            std::equal(deque_in.begin(), deque_in.end(), deque_out.begin())) passed_cases++;
        test_cases++;

        // Test set consolidation
        std::set<int> set_in = {5, 1, 3, 2, 4};
        TERM set_term = make(env, set_in);
        std::set<int> set_out;
        if (get(env, set_term, set_out) && set_out == set_in) passed_cases++;
        test_cases++;

        // Test unordered_set consolidation (special case with forward iterators)
        std::unordered_set<int> uset_in = {10, 30, 20};
        TERM uset_term = make(env, uset_in);
        std::unordered_set<int> uset_out;
        if (get(env, uset_term, uset_out) && uset_out == uset_in) passed_cases++;
        test_cases++;

        return make(env, std::make_tuple(test_cases, passed_cases));
    }
    else if(cmd=="process_monitoring_test")
    {
        int test_cases = 0;
        int passed_cases = 0;

        // Test self() function - this one adds value by returning PID directly
        ErlNifPid current_pid = self(env);
        test_cases++;
        if (current_pid.pid != 0) passed_cases++;

        return make(env, std::make_tuple(test_cases, passed_cases));
    }
    else if(cmd=="type_checking_test")
    {
        int test_cases = 0;
        int passed_cases = 0;

        // Test is_atom
        TERM atom_term = make(env, str_atom("test_atom"));
        test_cases++;
        if (is_atom(env, atom_term)) passed_cases++;

        // Test is_binary
        std::string test_str = "hello";
        TERM binary_term = make(env, test_str);
        test_cases++;
        if (is_binary(env, binary_term)) passed_cases++;

        // Test is_list
        std::vector<int> test_list = {1, 2, 3};
        TERM list_term = make(env, test_list);
        test_cases++;
        if (is_list(env, list_term)) passed_cases++;

        // Test is_empty_list
        TERM empty_list = make(env, std::vector<int>());
        test_cases++;
        if (is_empty_list(env, empty_list)) passed_cases++;

        // Test is_tuple
        TERM tuple_term = make(env, std::make_tuple(1, 2, 3));
        test_cases++;
        if (is_tuple(env, tuple_term)) passed_cases++;

        // Test is_number (integer)
        TERM int_term = make(env, 42);
        test_cases++;
        if (is_number(env, int_term)) passed_cases++;

        // Test is_number (double)
        TERM double_term = make(env, 3.14);
        test_cases++;
        if (is_number(env, double_term)) passed_cases++;

        // Test is_pid (current process)
        ErlNifPid current_pid = self(env);
        TERM pid_term = make(env, current_pid);
        test_cases++;
        if (is_pid(env, pid_term)) passed_cases++;

        // Test is_identical
        TERM term1 = make(env, 100);
        test_cases++;
        if (is_identical(term1, term1)) passed_cases++; // Same object

        // Test negative cases
        test_cases++;
        if (!is_atom(env, binary_term)) passed_cases++; // binary is not atom

        test_cases++;
        if (!is_list(env, atom_term)) passed_cases++; // atom is not list

        return make(env, std::make_tuple(test_cases, passed_cases));
    }
    else if(cmd=="custom_atoms_test")
    {
        int test_cases = 0;
        int passed_cases = 0;

        // Test that custom atoms are properly initialized
        test_cases++;
        if (am_test_atom.initialized()) passed_cases++;

        test_cases++;
        if (am_custom_success.initialized()) passed_cases++;

        test_cases++;
        if (am_my_special_atom.initialized()) passed_cases++;

        // Test that we can create terms from them
        TERM test_term = make(env, am_test_atom);
        test_cases++;
        if (is_atom(env, test_term)) passed_cases++;

        // Test that we can compare them
        test_cases++;
        if (am_test_atom == am_test_atom) passed_cases++;

        test_cases++;
        if (!(am_test_atom == am_custom_success)) passed_cases++;

        // Test string representation
        std::string atom_str = am_test_atom.to_string(env);
        test_cases++;
        if (atom_str == "test_atom") passed_cases++;

        return make(env, std::make_tuple(test_cases, passed_cases));
    }



#if SIZEOF_LONG != 8
    else if(cmd=="longlongint2")
    {
        long long int in;
        long long int out;
        get_throws(env, cmddata, in);
        out = in*2;
        return make(env,out);
    }
#endif
    cout << "cmd no match\r\n";
    return enif_make_badarg(env);
}


static int load(ErlNifEnv* env, [[maybe_unused]] void** priv, [[maybe_unused]] ERL_NIF_TERM load_info)
{
    nifpp::initialize_known_atoms(env);
    register_resource<std::string>(env, "std::string");
    register_resource<int>(env, "int");
    register_resource<tracetype>(env, "tracetype");
    return 0;
}

static ERL_NIF_TERM invoke_nif(ErlNifEnv* env, [[maybe_unused]] int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        return nif_main(env, nifpp::TERM(argv[0]));
    }
    catch(std::invalid_argument const&)
    {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"invoke_nif", 1, invoke_nif, 0},
};

ERL_NIF_INIT(nifpptest, nif_funcs, load, NULL, NULL, NULL)
