# -*- mode: sh -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case "$-" in
    *i*) ;;
    *) return ;;
esac

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

prepare_git_prompt_sh () {
    local dload_file=$1
    local url='https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh'
    local dload_cmd="curl -o ${dload_file} ${url}"

    if ! [ -e "$dload_file" ]; then
	if ! [ -x "$(command -v curl)" ]; then
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

git_prompt_sh="${HOME}/.git-prompt.sh"
prepare_git_prompt_sh "$git_prompt_sh"
. "$git_prompt_sh"

git_branch () {
    local branch="$(__git_ps1)"
    local ret=$?
    # Remove leading space and parentheses
    printf "${branch:2:-1}"
    return $ret
}

ansi_esc_seq () {
    local str=''

    for var in "$@"; do
        str="${str};${var}"
    done

    if [ -z "$str" ]; then
        echo "No argument provided" >&2
        return 1
    fi

    # Remove the first semicolon
    printf "\e[${str:1}m"
}

# Set the basic prompt for bash
prompt_command () {
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

    alias aes='ansi_esc_seq'

    local branch="$(git_branch)"
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

PROMPT_COMMAND=prompt_command

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

# Emacs eat integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"
