# -*- mode: sh -*-
# ~/.bash_profile

# Some environment variables used by plenty of programs
export NAME=$LOGNAME
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TIME_STYLE="long-iso"

prepend_path () {
    if [ -d "$1" ] ; then
	export PATH="${1}:${PATH}"
    fi
}

# Set environment variables to include macports' stuffs
export MP_PREFIX=${HOME}/store/mp

if [ -d $MP_PREFIX ] ; then
    prepend_path $MP_PREFIX/sbin
    prepend_path $MP_PREFIX/bin
    prepend_path $MP_PREFIX/libexec/gnubin
    export MANPATH=$MP_PREFIX/share/man
    export INFOPATH=$MP_PREFIX/share/info
fi

prepend_path $HOME/.local/n/bin
prepend_path $HOME/.local/yarn/bin
prepend_path $HOME/.local/go/bin
prepend_path $HOME/.local/bin

# Use clang in PATH as the default C/C++ compiler
export CC="$(which clang)"
export CXX="$(which clang++)"

if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
