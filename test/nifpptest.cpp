#include <iostream>
using std::cout;
using std::cerr;
using std::endl;

#include <functional>

using std::ref;

#define NIFPP_INTRUSIVE_UNIT_TEST
#include "nifpp.h"

extern nifpp::resource_ptr<int> get_resource_int(int val);

using namespace nifpp;

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
    cout << "cmd = " << cmd << "\r\n";
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
        binary newbin(ebin.size*2);
        std::memcpy(newbin.data,           ebin.data, ebin.size);
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
