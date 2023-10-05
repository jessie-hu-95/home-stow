# -*- mode: sh -*-
# ~/.bash_profile

# Some environment variables used by plenty of programs
export NAME=$LOGNAME
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export TIME_STYLE="long-iso"

prepend() {
    # Usage: prepend PATH /usr/local/bin
    [ -d "$2" ] && eval $1=\"$2':'\$$1\" && export $1
}

# PATHs for macports
export MP_PREFIX=$HOME/store/mp
prepend PATH     $MP_PREFIX/sbin
prepend PATH     $MP_PREFIX/bin
prepend PATH     $MP_PREFIX/libexec/gnubin
prepend MANPATH  $MP_PREFIX/share/man
prepend INFOPATH $MP_PREFIX/share/info

# Bash-completion provided by macports
if [ -f $MP_PREFIX/etc/profile.d/bash_completion.sh ]; then
    . $MP_PREFIX/etc/profile.d/bash_completion.sh
fi

# PATHs for texlive 2023
export TL_PREFIX=$HOME/store/tl/2023
prepend PATH     $TL_PREFIX/bin/universal-darwin
prepend MANPATH  $TL_PREFIX/texmf-dist/doc/man
prepend INFOPATH $TL_PREFIX/texmf-dist/doc/info

# Common binary PATHs
prepend PATH $HOME/.local/n/bin
prepend PATH $HOME/.local/yarn/bin
prepend PATH $HOME/.local/go/bin
prepend PATH $HOME/.local/bin

# Use clang in PATH as the default C/C++ compiler
export CC="$(which clang)"
export CXX="$(which clang++)"

if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
