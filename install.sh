#!/bin/bash
dir=~/dotfiles
bckdir=~/dotfiles.bck
files="zshrc emacs tmux.conf"

echo -n "Creating $bckdir to backup of any existing dotfiles in ~"
mkdir -p $bckdir
echo "Done"

echo -n "Changing to the $dir"
cd $dir
echo "Done"

for file in $files; do
    echo "Moving any existing dotfiles from ~ to $bckdir"
    mv ~/.$file $bckdir
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done
