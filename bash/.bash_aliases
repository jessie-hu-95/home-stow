# -*- mode: sh -*-
# ~/.bash_aliases

# Wrap the following commands for interactive use to avoid accidental
# file overwrites.
rm() { command rm -iv "${@}"; }
cp() { command cp -iv "${@}"; }
mv() { command mv -iv "${@}"; }

# Enable color support of ls and also add handy aliases
if [ -x "$(cmdp dircolors)" ]; then
    if [ -r ~/.dircolors ]; then
	eval "$(dircolors --bourne-shell ~/.dircolors)"
    else
	eval "$(dircolors --bourne-shell)"
    fi
    coloropt="--color=auto"
else
    coloropt=""
fi

alias ll="ls ${coloropt} --group-directories-first --format=verbose --human-readable"
alias la="ls ${coloropt} --group-directories-first --format=verbose --human-readable --almost-all"
alias ls="ls ${coloropt} --group-directories-first --format=single-column"
alias diff="diff ${coloropt}"
alias dir="dir ${coloropt}"
alias vdir="vdir ${coloropt}"
alias grep="grep ${coloropt}"
alias fgrep="fgrep ${coloropt}"
alias egrep="egrep ${coloropt}"

# My convenient aliases
alias jq="jq -S --indent 4"
