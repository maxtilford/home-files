#!/bin/zsh
source /etc/profile
setopt promptsubst

autoload -U promptinit
promptinit
prompt wunjo

autoload -U compinit
compinit

#setopt correctall

export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt autocd

export BROWSER=chromium
export EDITOR=ed
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/local/bin:$HOME/.node/bin:$HOME/work/android-sdk-linux/tools:$HOME/work/android-sdk-linux/platform-tools
export AUTOFEATURE=true
unset RUBYOPT; env-update &>/dev/null

#alias heroku="nocorrect bundle exec heroku"
alias grepl="rlwrap git repl"
alias cd="nocorrect cd"
alias mkdir="nocorrect mkdir"
alias mv="nocorrect mv"

#add autocompletion for custom git commands
zstyle ':completion:*:git:*' user-commands ${${(k)commands[(I)git-*]}#git-}

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

rmv()
{
    while [ ! -z "$1" ]; do
        mkdir "$HOME/tmp/trash"
        mv "$1" "$HOME/tmp/trash/"
        shift
    done
}
