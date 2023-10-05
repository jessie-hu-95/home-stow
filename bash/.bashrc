# -*- mode: sh -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case "$-" in
    *i*) ;;
    *) return ;;
esac

# Enable XON/XOFF flow control (that is, ‘Ctrl-S’/‘Ctrl-Q’).  May be
# negated.
stty -ixon

# Don't put duplicate lines or lines starting with space in the
# history.  See bash(1) for more options
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=2000
HISTFILESIZE=5000

# Check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# Do not overwrite files when redirecting output by default.
set -o noclobber

cmdp () {
    command -v "${@}"
}

# https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
. ~/.git-prompt.sh

_git_branch () {
    local branch="$(__git_ps1)"
    local ret=$?
    # Remove leading space and parentheses
    printf "${branch:2:-1}"
    return $ret
}

_ansi_esc_seq () {
    local str=''

    for var in "$@"; do
        str="${str};${var}"
    done

    if [ -z "$str" ]; then
        echo "No argument provided" >&2
        return 1
    fi

    # Remove the first semicolon
    printf "\[\e[${str:1}m\]"
}

# Set the basic prompt for bash
_prompt_command () {
    local code=$?

    # ANSI escape codes
    local reset=0
    local bold=1
    local faint=2
    local italic=3
    local underline=4
    local red=31
    local green=32
    local yellow=33
    local blue=34
    local magenta=35

    alias aes='_ansi_esc_seq'

    local branch="$(_git_branch)"
    local cgit
    if [ -n "$branch" ]; then
        cgit=" $(aes $faint)on$(aes $reset) $(aes $underline $magenta)${branch}$(aes $reset)"
    else
        cgit=''
    fi

    local ccode
    if [ $code -eq 0 ]; then
        ccode=" $(aes $bold $green)${code}$(aes $reset)"
    else
        ccode=" $(aes $bold $red)${code}$(aes $reset)"
    fi

    local symbol="$(aes $bold $blue)\$$(aes $reset) "

    local path="$(aes $italic)\w$(aes $reset)"

    PS1="\n${path}${cgit}${ccode}\n${symbol}"

    unalias aes
}

PROMPT_COMMAND=_prompt_command

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Emacs eat integration
if [ -n "$EAT_SHELL_INTEGRATION_DIR" ]; then
    source "$EAT_SHELL_INTEGRATION_DIR/bash"
fi

# Enter directory and list contents
cd () {
    if [ -n "$1" ]
    then
        builtin cd "$@" && la
    else
        builtin cd ~ && la
    fi
}
