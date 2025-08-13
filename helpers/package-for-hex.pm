#! /usr/bin/env bash

show_usage() {
    cat <<EOF
`basename "$0"` -- build and upload documentation to hexdoc
Usage: `basename "$0"` [options]
Options:
    -h      Show this help
    -d      Print debug info (set the shell -xv options)
    -n      Dry-run do not send anything
    -m      Almost dry-run (moist) This will disable the
            version format check, but will not send anything
EOF
}

dry_run=false
moist_run=false
while getopts "hdnm" opt
do
    case "$opt" in
        h) show_usage; exit 0;;
        d) set -xv;;
        n) dry_run=true;;
        m) moist_run=true;;
    esac
done
shift $(($OPTIND - 1))

script_dir="$(dirname "$0")"
script_path="$(cd "$script_dir"; pwd)"
cd "$script_dir"
repo_top=$(git rev-parse --show-toplevel)
cd "$repo_top"
repo_path="$(pwd)"
repo_sha=$(git log -1 --format=%H)

d="$(mktemp -d "tmp-pkg-for-hex-XXXXXXX")"
cleanup () { /bin/rm -rf "$repo_path/$d"; }
trap 'xc=$?; cleanup; exit $xc' EXIT INT QUIT TERM

vsn="$(git describe --always --tags --match '[0-9]*.[0-9]*')"
if [ $dry_run = false -a $moist_run = false ]
then
    if ! (echo "$vsn" | egrep '^[0-9]+(\.[0-9]+)*$' >/dev/null)
    then
        echo "ERROR: bad version \"$vsn\", will only publish versions" >&2
        echo "that are dotted numbers only." >&2
        echo "Aborting." >&2
        cleanup
        exit 1
    fi
fi
echo "Version of gpb: $vsn"

set -e
cd "$d"

yes_no () {
    echo "Run?" "$@"
    valid_answer=false
    while [ $valid_answer = false ]
    do
        printf "Proceed? [Yn]"
        read x
        case "$x" in
            y|Y|yes|Yes|YES|"")
                valid_answer=true
                "$@"
                ;;
            n|N|no|No|NO)
                valid_answer=true
                echo "Not running that command"
                ;;
            *)
                :
                ;;
        esac
    done
}

(

    git clone -q "$repo_path"
    cd "$(basename "$repo_path")"
    unset ERL_LIBS
    git checkout -q "$repo_sha"
    make -j all
    echo "$vsn" > gpb.vsn
    cat > .publish-tags <<EOF
[{description,
  "A compiler for Google protocol buffer definitions files for Erlang."},
 {files, ["src", "include", "descr_src", "priv", "bin", "build",
          "gpb.vsn",
          "COPYING.LIB", "README.*", "rebar.config.script",
          "ChangeLog", "Makefile"]},
 {build_tools, [rebar3, make]},
 {licenses, ["LGPL-2.1-or-later"]},
 {links, [{"GitHub", "https://github.com/tomas-abrahamsson/gpb"}]},
 {exclude_files, ["include/gpb_version.hrl",
                  "descr_src/gpb_descriptor.erl",
                  "descr_src/gpb_descriptor.hrl"]}
].
EOF
    # Substitute the values above in the gpb.app.src,
    # retaining file file head.
    erl +B -noinput -boot no_dot_erlang -eval '
       AppSrc = "src/gpb.app.src",
       {ok,B} = file:read_file(AppSrc),
       {ok,[{application,gpb,Infos}]} = file:consult(AppSrc),
       {ok,[New]} = file:consult(".publish-tags"),
       Infos2 = lists:foldl(
                  fun({K,_}=Item, Acc) -> lists:keystore(K,1,Acc,Item) end,
                  Infos, New),
       [FHead | _] = re:split(B, <<"{application,.*gpb">>),
       B2 = [FHead, io_lib:format("~p.~n", [{application,gpb,Infos2}])],
       case file:write_file(AppSrc, B2) of
           ok -> halt(0);
           Error -> io:format(standard_error, "Error writing ~p:~n  ~p~n",
                              [AppSrc, Error]),
                    halt(1)
       end.
'
    if [ $? != 0 ]
    then
        exit 1
    fi
    cat src/gpb.app.src
    export DIAGNOSTIC=1
    yes_no rebar3 hex publish
)
