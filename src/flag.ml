type opt_flag_t = {
    fe: bool;
}

let default_opt_flags = {
    fe=false;
}

type size_flag_t = {
    stack: int;
    storage: int;
}

let default_size_flags = {
    stack=1024;
    storage=4096;
}
