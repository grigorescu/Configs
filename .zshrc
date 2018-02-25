[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

## History - Save a lot of history
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

## Exports

# If we have a ZDOTDIR set, we keep it. Otherwise, set it to the user's home directory.
export ZDOTDIR=${ZDOTDIR-$HOME}

if [[ -d $ZDOTDIR/bin ]]; then
    [[ ":$PATH": != *":$ZDOTDIR/bin:"* ]] && export PATH="$ZDOTDIR/bin:$PATH"
fi
if [[ -d $ZDOTDIR/.local/bin ]]; then
    [[ ":$PATH": != *":$ZDOTDIR/.local/bin:"* ]] && export PATH="$ZDOTDIR/.local/bin:$PATH"
fi

export LESS="--ignore-case --no-init --quit-if-one-screen --LONG-PROMPT --shift 5 --RAW-CONTROL-CHARS"
export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"

if [[ -d $ZDOTDIR/.local/lib64/python2.7/site-packages ]]; then
    export PYTHONPATH="$ZDOTDIR/.local/lib64/python2.7/site-packages"
fi

if [[ -f $ZDOTDIR/.emacs ]]; then
    export EDITOR="emacs -nw -l $ZDOTDIR/.emacs"
else
    export EDITOR="emacs -nw"
fi

if which lesspipe.sh &> /dev/null;
then
    export LESSOPEN="| lesspipe.sh %s"
fi

export EVENT_NOKQUEUE=1

# Compinit - completion
autoload -U compinit; compinit -d $ZDOTDIR/.zcompdump

# Select word style - use bash style word delimiters (whitespace, forward slashes, etc.)
autoload -U select-word-style
select-word-style bash

# zmv is a module that allow people to do massive renames
autoload -U zmv

# Auto-escape special chars in URLs:
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
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
zstyle :compinstall filename "$ZDOTDIR/.zshrc"
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

# Source our keychain file
if [[ -a ~/.keychain/`hostname`-sh ]]; then
    source ~/.keychain/`hostname`-sh
fi

## Set the TERM name to the hostname
preexec () {
    if [[ "$TERM" == "screen" ]]; then
        local SHORTHOST=`echo $HOST | cut -f1 -d"."`
        echo -ne "\ek$SHORTHOST\e\\"
    fi
}

_install_dep() {
    COMMAND=$1
    typeset -A pre_install_cmd
    pre_install_cmd=(ubuntu "sudo -v" redhat "sudo -v")
    typeset -A install_cmd
    install_cmd=(ubuntu "sudo apt-get install" redhat "sudo yum install" apple "brew install")
    typeset -A package_name
    package_name=(git git tmux tmux emacs-nox emacs-nox)
    if [[ "$VENDOR" == "redhat" ]]; then
        package_name[pip]=python2-pip
    elif [[ "$VENDOR" == "ubuntu" ]]; then
        package_name[pip]=python-pip
    elif [[ "$VENDOR" == "apple" ]]; then
        package_name[pip]=pip
    fi

    if ! which $COMMAND &> /dev/null;
    then
        echo "$COMMAND not found"
        if [[ -z "${install_cmd[$VENDOR]}" ]]; then
            echo "Don't know how to install $COMMAND for $VENDOR"
            return 1
        fi

        if [[ -n "${pre_install_cmd[$VENDOR]}" ]]; then
            eval "${pre_install_cmd[$VENDOR]}" || { echo "pre-install command failed"; exit 1 }
        fi

        eval "${install_cmd[$VENDOR]} ${package_name[$COMMAND]}" || echo "install command failed"
    fi

}

check_config_deps() {
    _install_dep git
    _install_dep pip
    pip install --user powerline-status
    _install_dep tmux
    if [[ $VENDOR != "apple" ]]; then
        _install_dep emacs-nox
    fi
}

update_configs() {
    wget -O configs.tgz https://github.com/grigorescu/Configs/tarball/master &>/dev/null
    tar xvzf configs.tgz --strip-components=1 &>/dev/null
    rm configs.tgz
    GROUP=$(groups $(whoami) | cut -d' ' -f1)
    sed -i.bak -e "s/ users / $GROUP /" .git_cache_meta && rm .git_cache_meta.bak
    USER=$(whoami)
    sed -i.bak -e "s/ vladg / $USER /" .git_cache_meta && rm .git_cache_meta.bak
    ./git-cache-meta.sh --apply
    rm git-cache-meta.sh .git_cache_meta
}

install_configs() {
    update_configs
    check_config_deps
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

POWERLINE_PATH="python2.7/site-packages/powerline/bindings/zsh/powerline.zsh"

if [[ -f "$ZDOTDIR/.lib/$POWERLINE_PATH" ]]; then
    source "$ZDOTDIR/.lib/$POWERLINE_PATH"
elif [[ -f "$ZDOTDIR/.local/lib/$POWERLINE_PATH" ]]; then
    source "$ZDOTDIR/.local/lib/$POWERLINE_PATH"
elif [[ -f "/usr/lib64/$POWERLINE_PATH" ]]; then
    source "/usr/lib64/$POWERLINE_PATH"
elif [[ -f "/usr/local/lib/$POWERLINE_PATH" ]]; then
    source "/usr/local/lib/$POWERLINE_PATH"
else
    echo "Could not find powerline"
    install_configs
fi

if [[ -n "$LC_TMUX" ]]; then
    echo "Already within tmux, so not attaching again."
fi

if [[ -z "$ITERM_PROFILE" && -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$LC_TMUX" ]]; then
    if [[ -z "$POWERLINE_COMMAND" ]]; then
        powerline-daemon -q
    fi
    # Create a session if no session has been defined in tmux.conf.
    if ! tmux has-session 2> /dev/null; then
        tmux_session='main'
        tmux \
            new-session -d -s "$tmux_session" \; \
            set-option -t "$tmux_session" destroy-unattached off &> /dev/null
    fi
    # Attach to the 'prezto' session or to the last session used.
    echo "Connecting to tmux..."
    exec tmux attach-session
elif [[ -n "$TMUX" ]]; then
    powerline-config tmux setup
fi

# I want to avoid tmux within tmux, so I abuse LC_TMUX which is usually allowed to be passed through ssh
# I also special case running with iterm, which uses tmux internally but supports nesting.
if [[ -z "$ITERM_PROFILE" ]]; then
    export LC_TMUX="tmux"
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_TMUX=0
