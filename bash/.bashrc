# -*- mode: sh -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case "$-" in
    *i*) ;;
    *) return ;;
esac

# Don't put duplicate lines or lines starting with space in the
# history.  See bash(1) for more options
export HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=2000
export HISTFILESIZE=5000

# Check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# Enable color support of ls and also add handy aliases
if [ -x "$(command -v dircolors)" ]; then
    if [ -r ~/.dircolors ]; then
	eval "$(dircolors --bourne-shell ~/.dircolors)"
    else
	eval "$(dircolors --bourne-shell)"
    fi

    alias ls="ls --color=auto"
    alias dir="dir --color=auto"
    alias vdir="vdir --color=auto"
    alias grep="grep --color=auto"
    alias fgrep="fgrep --color=auto"
    alias egrep="egrep --color=auto"
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

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

git_prompt () {
    local branch=$(git_branch)
    local ret=$?
    if [ $branch ]; then
	echo " on ${branch}"
    fi
    return $ret
}

# Set the basic prompt for bash
export PS1="\n\w\$(git_prompt) \nλ "

# Starship prompt
if [ -x "$(command -v starship)" ] && [ "dumb" != $TERM ]; then
    eval "$(starship init bash)"
fi
