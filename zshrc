# Path to your oh-my-zsh installation.
export ZSH=~/dotfiles/oh-my-zsh

export TERM=xterm-256color
# Making emacs start server on emacsclient call
export ALTERNATE_EDITOR=""

ZSH_THEME="philips"
COMPLETION_WAITING_DOTS="true"

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# plugins
plugins=(git npm node meteor history-substring-search)

source $ZSH/oh-my-zsh.sh
# path
export PATH="/opt/local/bin:/opt/local/sbin:/Library/Frameworks/Python.framework/Versions/3.5/bin:/Library/Frameworks/Python.framework/Versions/3.4/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/git/bin:/Library/TeX/texbin:/usr/local/go/bin"
export PATH="/Library/PostgreSQL/9.5/bin:$PATH"
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/Cellar/openssl/1.0.2d_1/lib

# golang
export GOROOT=/usr/local/go
export GOPATH=~/Documents/Code/goworkspace

# aliases
## brew install gls
alias ls="/usr/local/bin/gls --color -hX --group-directories-first"
alias la="/usr/local/bin/gls --color -o -AhX --group-directories-first"
## brew install trash
alias rm="trash"
##
alias clr="clear"

alias -g Gi='| grep -i --color=always'

# emacs
alias e="emacsclient -t"
alias kill-emacs="emacsclient -e \"(kill-emacs)\""

# configs
alias zshrc="e ~/.zshrc"
alias emacsconfig="e ~/.emacs"
alias tmuxconf="e ~/.tmux.conf"

# npm
alias ni="npm install"
alias nis="npm i -S "
alias nid="npm i -D "
alias nig="npm i -g "
alias nrs="npm run start"
alias nrb="npm run build"
alias nrt="npm run test"
alias nrd="npm run dev"

# rest
alias bower="noglob bower"
## nig tldr
alias h="tldr"

# ssh
alias fourier="ssh vkolmako@fourier.cs.iit.edu"

# zaw
source ~/dotfiles/zaw/zaw.zsh

bindkey '^R' "zaw-history"
zstyle ':filter-select' max-lines 10
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' extended-search yes
zstyle ':filter-select' hist-find-no-dups yes
zstyle ':filter-select' rotate-list yes
