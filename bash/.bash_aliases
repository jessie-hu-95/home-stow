# -*- mode: sh -*-
# ~/.bash_aliases

alias ll="${BASH_ALIASES[ls]} --format=verbose --human-readable"
alias la="${BASH_ALIASES[ll]} --almost-all"
alias ls="${BASH_ALIASES[ls]} --format=single-column"

alias jq="jq -S --indent 4"
