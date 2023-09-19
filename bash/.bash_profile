# -*- mode: sh -*-
# ~/.bash_profile

function prepend_path () {
    if [ -d "$1" ] ; then
	export PATH="$1:$PATH"
    fi
}

# Set environment variables to include macports' stuffs
if [ -d /opt/local ] ; then
    prepend_path /opt/local/bin
    prepend_path /opt/local/libexec/gnubin
    export MANPATH=/opt/local/share/man
    export INFOPATH=/opt/local/share/info
fi

prepend_path $HOME/.local/n/bin
prepend_path $HOME/.local/yarn/bin
prepend_path $HOME/.local/go/bin
prepend_path $HOME/.local/bin

# Some environment variables used by plenty of programs
export NAME=$LOGNAME
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TIME_STYLE="long-iso"

# Use clang in PATH as the default C/C++ compiler
export CC="$(which clang)"
export CXX="$(which clang++)"

if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
