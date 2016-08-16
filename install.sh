#!/bin/bash
create_dir() {
    backup_dir=$1
    mkdir -p "$backup_dir"
}

backup_file() {
    # move src file to the dst folder
    target=$1
    backup_dir=$2

    printf "\tBacking up $target to $backup_dir\n"
    mv "$src" "$dst"
}

create_symlink() {
    # create symlink from src file to dst file
    src=$1
    dst=$2

    printf "\tCreating symlink from $src to $dst\n"
    ln -s "$src" "$dst"
}

setup_homedir() {
    dotfiles_dir=$1
    backup_dir=$2

    home_dir_dotfiles=".zshrc .emacs .tmux.conf"

    for file in $home_dir_dotfiles; do
        backup_file "~/$file" "$backup_dir"
        create_symlink "$dotfiles_dir/$file" "~/$file"
    done
}

setup_karabiner() {
    dotfiles_dir=$1
    backup_dir=$2

    karabiner_dir=~/Library/Application\ Support/Karabiner
    karabiner_config_file=private.xml

    backup_file "$karabiner_dir/$karabiner_config_file" "$backup_dir"
    create_symlink "$dotfiles_dir/karabiner/$karabiner_config_file" "$karabiner_dir/$karabiner_config_file"
}

main() {
    dotfiles_dir=~/dotfiles
    backup_dir=~/dotfiles.bck

    printf "* Creating $backup_dir to backup of any existing dotfiles\n"
    create_dir "$backup_dir"

    printf "* Setting up dotfiles in ~/\n"
    setup_homedir "$dotfiles_dir" "$backup_dir"

    printf "* Setting up karabiner\n"
    setup_karabiner "$dotfiles_dir" "$backup_dir"

    printf "Done!\n"
}

main
