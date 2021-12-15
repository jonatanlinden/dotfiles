# zmodload zsh/zprof

# Load all stock functions (from $fpath files) called below.
# autoload -U compaudit
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.history"
HISTSIZE=50000
SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history data
setopt hist_reduce_blanks     # Pretty    Obvious.  Right?




function env_default() {
    env | grep -q "^$1=" && return 0
    export "$1=$2"       && return 3
}

NVM_PROMPT_PREFIX="(⬡ "
NVM_PROMPT_SUFFIX=")"
# get the node.js version
function nvm_prompt_info() {
  [[ -f "$NVM_DIR/nvm.sh" ]] || return
  local nvm_prompt
  nvm_prompt=$(node -v 2>/dev/null)
  [[ "${nvm_prompt}x" == "x" ]] && return
  nvm_prompt=${nvm_prompt:1}
  echo "${NVM_PROMPT_PREFIX}${nvm_prompt}${NVM_PROMPT_SUFFIX}"
}

export NVM_NO_USE=true

# must occur before zsh-syntax-highlighting due to bug. (loaded in .zsh_plugins.sh)
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /:@+|"
zstyle ':zle:*' word-style unspecified

source ~/.zsh_plugins.sh

RPROMPT='$(nvm_prompt_info)'

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

bindkey -e

# load modules
autoload -U zmv

# fix issue in iterm?
export COLUMNS

alias svn=/usr/bin/svn

alias emacs="emacsclient -nc"

setopt no_beep

# Changing/making/removing directory
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus


setopt multios
setopt prompt_subst

# Make zsh know about hosts already accessed by SSH
#zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# emacs read only
er() {
  emacs "$1" --eval '(setq buffer-read-only t)'
}

function ev; {
    emacsclient --nw --eval '(view-file "$1")'
}

function E; {
    emacsclient -t -a "" "/sudo::$@"
}

alias E3="SUDO_EDITOR=\"emacsclient -c -a emacs\" sudoedit"

insert_sudo () {
    zle beginning-of-line; zle -U "sudo "
}
zle -N insert-sudo insert_sudo
bindkey "s" insert-sudo
# Meta-u to chdir to the parent directory
bindkey -s 'u' '^Ucd ..; ls^M'


# Remove default keyboard shortcuts C-s and C-q
stty stop undef
stty start undef

# URL encode something and print it.
function url-encode; {
        setopt extendedglob
        echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

# Search google for the given keywords.
function google; {
        $VIEW "http://www.google.com/search?q=`url-encode "${(j: :)@}"`"
}

function cropopen; {
    pdfcrop --resolution 72 "$1" && open "${1:t:r}-crop.pdf"
}


alias cat=ccat

# Enable ls colors
export LSCOLORS="Gxfxcxdxbxegedabagacad"

alias ls='ls -G'
alias ll='ls -lh'
alias la='ls -lAh'
alias zmv='noglob zmv -W'

alias brewall="brew list | while read cask; do echo -n $cask; brew deps $cask | awk '{printf(\" %s \", ${0})}'; echo ''; done"

#source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# lazily run
rbenv() {
  eval "$(command rbenv init - --no-rehash)"
  rbenv "$@"
}

# zprof

# export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
