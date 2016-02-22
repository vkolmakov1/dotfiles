# Path to your oh-my-zsh installation.
export ZSH=/Users/vkolmakov/.oh-my-zsh

export TERM=xterm-256color
# Making emacs start server on emacsclient call
export ALTERNATE_EDITOR=""

ZSH_THEME="minimal"

COMPLETION_WAITING_DOTS="true"

# plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

# path
export PATH="/opt/local/bin:/opt/local/sbin:/Library/Frameworks/Python.framework/Versions/3.5/bin:/Library/Frameworks/Python.framework/Versions/3.4/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/git/bin:/usr/texbin:/usr/local/go/bin"
export PATH="/Library/PostgreSQL/9.5/bin:$PATH"
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/Cellar/openssl/1.0.2d_1/lib

# golang
export GOROOT=/usr/local/go
export GOPATH=~/Documents/Code/goworkspace

# aliases
alias e="emacsclient -t"
alias kill-emacs="emacsclient -e \"(kill-emacs)\""

alias zshcfg="e ~/.zshrc"
alias emacscfg="e ~/.emacs"
alias tmuxcfg="e ~/.tmux.conf"

alias bower="noglob bower"
alias fourier="ssh vkolmako@fourier.cs.iit.edu"
