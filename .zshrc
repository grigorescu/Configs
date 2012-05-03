## History - Save a lot of history
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# If we have a ZDOTDIR set, we keep it. Otherwise, set it to the user's home directory.
ZDOTDIR=${ZDOTDIR-/home/$USER}

EDITOR="emacs -nw -l $ZDOTDIR/.emacs"

# Compinit - completion
autoload -U compinit; compinit -d $HOME/.zcompdump

# Select word style - use bash style word delimiters (whitespace, forward slashes, etc.)
autoload -U select-word-style
select-word-style bash

# zmv is a module that allow people to do massive renames
autoload -U zmv

# Auto-escape special chars in URLs:
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
#zstyle -e ':url-quote-magic:*' url-seps 'reply=(";&<>${histchars[1]}")'
zstyle -e ':url-quote-magic:*' url-seps 'reply=("&<${histchars[1]}")'

# We like kewl prompts
autoload -U promptinit
promptinit

# Options set here: 
#   - Do not enter command lines into the history list if they are duplicates of the previous event.
#   - When searching for history entries in the line editor, do not display duplicates of a line previously found
#   - If this is set, zsh sessions will append their history list to the history file, rather than overwrite it. Thus, multiple parallel zsh sessions will all have their history lists added to the history file, in the order they are killed.
#   - Share the history file among all sessions
#   - Don't store commands prefixed with a space in the history
#   - Don't show the types of each file
#   - Don't use menu completion
#   - Don't run background jobs at a lower priority
#   - Don't print an error if a pattern for filename generation has no matches.
#   - Don't use flow control
#
setopt hist_ignore_all_dups hist_find_no_dups append_history sharehistory hist_ignore_space no_list_types no_auto_menu no_bg_nice no_nomatch no_flow_control

# The zsh/complist module offers three extensions to completion
# listings: the ability to highlight matches in such a list, the ability
# to scroll through long lists and a different style of menu completion.

zmodload -i zsh/complist

# Misc completion things

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:processes' command 'ps -u$USER -o pid=,command='
zstyle ':completion:*:processes-names' command 'ps -u$USER -o comm='
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle :compinstall filename '/home/vlad/.zshrc'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
setopt completealiases

# Emacs keybindings
bindkey -e

# You can set the autocd option if you want to avoid tedious typing of cd command while changing current directory (for example /etc instead of cd /etc).
# More powerful globbing (e.g. ^ does negation)
setopt autocd extendedglob

## Aliases
alias mv='nocorrect mv'       # no spelling correction
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias grep='grep --color'
alias emacs=$EDITOR

## Exports
export LESS="-i -F -M -# 5"

## Special su stuff for SMG
if [[ -a /etc/psg.conf ]]; then
    if grep 'release 6' /etc/redhat-release &> /dev/null; then
	su() { service=$1; [[ "$1" == "-" ]] && service=$2; /bin/su - $service --session-command "ZDOTDIR=$ZDOTDIR /bin/zsh" }
    elif grep 'release 4' /etc/redhat-release &> /dev/null; then
	su() { service=$1; [[ "$1" == "-" ]] && service=$2; /bin/su - $service -s /bin/sh -c "ZDOTDIR=$ZDOTDIR /bin/zsh" }
    fi
zstyle ':completion:*' users ls /services | egrep -v 'lost\+found|scratch|www|nagios|ftp|smg-p'
fi
## Set the TERM name to the hostname
preexec () {
    if [[ "$TERM" == "screen" ]]; then
        local SHORTHOST=`echo $HOST | cut -f1 -d"."`
        echo -ne "\ek$SHORTHOST\e\\"
    fi
}

update_configs() {
    wget -O configs.tgz https://github.com/grigorescu/Configs/tarball/master &>/dev/null
    tar xvzf configs.tgz --strip-components=1 &>/dev/null
    source .zshrc
    rm configs.zip
}

extract() {
    for i; do
        case "$i" in
            *.tar.bz2)  tar -xjvf "$i"   ;;
            *.tbz2)     tar -xjvf "$i"   ;;
            *.tar.gz)   tar -xzvf "$i"   ;;
            *.tgz)      tar -xzvf "$i"   ;;
            *.rar)      unrar x -kb "$i" ;;
            *.bz2)      bzip2 -d "$i"    ;;
            *.gz)       gunzip -d "$i"   ;;
            *.tar)      tar -xvf "$i"    ;;
            *.zip)      unzip "$i"       ;;
            *.7z)       7z e "$i"        ;;
            *)          echo "unknown archive format: '$i'" ;;
        esac
    done
}


# hgrep - highlight grep
#
# This works very similarly to grep except that it always prints out
# all the content, not just matching lines.
#
# Matching text will be highlighted.
#
function hgrep () {
    if (( ! $# )); then
      echo "Usage: $0:t [-e pattern...] [file...]" >&2
      return 1
    fi

    local -a regex
    local htext=`echotc so` ntext=`echotc se`
    while [[ "$1" = -e ]]; do
        regex=( $regex "$2" )
        shift 2
    done

    if (( ! $#regex )); then
                regex=( "$1" )
                shift
    fi

    regex=( "-e
    s/${^regex[@]}/$htext&$ntext/g" )
    sed ${(Ff)regex[@]} "$@"
}

# Colors!
autoload -U colors && colors

# Set our prompt to something cool-looking.
export PS1="%{$fg[green]%}%n@%m%{$reset_color%} %{$fg[blue]%}%1~ %{$reset_color%}%% "