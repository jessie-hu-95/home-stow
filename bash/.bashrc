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

_prepare_git_prompt_sh () {
    local dload_file=$1
    local url='https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh'
    local dload_cmd="curl -o ${dload_file} ${url}"

    if ! [ -e "$dload_file" ]; then
	if ! [ -x "$(cmdp curl)" ]; then
	    echo "Command curl is required to download scripts" >&2
	    return 1
	fi
	# Download the git-prompt.sh script
	echo $dload_cmd
	eval $dload_cmd
    fi

    if ! [ -x "$dload_file" ]; then
	chmod +x "$dload_file"
    fi
    return
}

# Download git-prompt and source it
_bin_dir=$HOME/.local/bin
mkdir -p $_bin_dir
_git_prompt_sh=${_bin_dir}/git-prompt
chmod +x $_git_prompt_sh
_prepare_git_prompt_sh $_git_prompt_sh
. $_git_prompt_sh

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
        cgit=" on $(aes $underline $magenta)${branch}$(aes $reset)"
    else
        cgit=''
    fi

    local ccode
    if [ $code -eq 0 ]; then
        ccode=" $(aes $bold $green)${code}$(aes $reset)"
    else
        ccode=" $(aes $bold $red)${code}$(aes $reset)"
    fi

    local symbol="$(aes $bold $italic $blue)λ$(aes $reset) "

    PS1="\n\w${cgit}${ccode}\n${symbol}"

    unalias aes
}

PROMPT_COMMAND=_prompt_command

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

# The COLORTERM is documented in (info "(emacs) General Variables").
# The reference to `dumb-emacs-ansi' is in (info "(emacs) Connection
# Variables").
if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ]
then
    export PAGER=cat
    alias less=cat
    export TERM="dumb-emacs-ansi"
    export COLORTERM=1
else
    export PAGER=less
fi

# Emacs eat integration
if [ -n "$EAT_SHELL_INTEGRATION_DIR" ]; then
    source "$EAT_SHELL_INTEGRATION_DIR/bash"
fi

# Colourise man pages
man () {
    env \
        LESS_TERMCAP_mb=$(tput bold; tput setaf 6) \
        LESS_TERMCAP_md=$(tput bold; tput setaf 6) \
        LESS_TERMCAP_me=$(tput sgr0) \
        LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
        LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
        LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 4) \
        LESS_TERMCAP_mr=$(tput rev) \
        LESS_TERMCAP_mh=$(tput dim) \
        LESS_TERMCAP_ZN=$(tput ssubm) \
        LESS_TERMCAP_ZV=$(tput rsubm) \
        LESS_TERMCAP_ZO=$(tput ssupm) \
        LESS_TERMCAP_ZW=$(tput rsupm) \
        man "$@"
}

# Enter directory and list contents
cd () {
    if [ -n "$1" ]
    then
        builtin cd "$@" && la
    else
        builtin cd ~ && la
    fi
}
