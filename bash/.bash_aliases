# -*- mode: sh -*-
# ~/.bash_aliases

# Wrap the following commands for interactive use to avoid accidental
# file overwrites.
rm() { command rm -i "${@}"; }
cp() { command cp -i "${@}"; }
mv() { command mv -i "${@}"; }

# Enable color support of ls and also add handy aliases
if [ -x "$(command -v dircolors)" ]; then
    if [ -r ~/.dircolors ]; then
	eval "$(dircolors --bourne-shell ~/.dircolors)"
    else
	eval "$(dircolors --bourne-shell)"
    fi
    coloropt="--color=auto"
else
    coloropt=""
fi

alias ll="ls ${coloropt} --format=verbose --human-readable"
alias la="ls ${coloropt} --format=verbose --human-readable --almost-all"
alias ls="ls ${coloropt} --format=single-column"
alias dir="dir ${coloropt}"
alias vdir="vdir ${coloropt}"
alias grep="grep ${coloropt}"
alias fgrep="fgrep ${coloropt}"
alias egrep="egrep ${coloropt}"

# My convenient aliases
alias jq="jq -S --indent 4"
