type opt_flag_t = {
    cf: bool;
    sc: bool;
    fe: bool;
}

let default_opt_flags = {
    cf=true;
    sc=true;
    fe=true;
}

type size_flag_t = {
    stack: int;
    storage: int;
}

let default_size_flags = {
    stack=1024;
    storage=4096;
}
