const fs = require("fs");
const path = require("path");

const HOME_DIR = require("os").homedir();

function doesFileOrSymlinkAlreadyExistSync(file) {
  let result = false;
  // test for a regular file

  try {
    fs.accessSync(file, fs.constants.F_OK);
    result = true;
  } catch (err) {
    // test for a symlink
    try {
      fs.readlinkSync(file);
      result = true;
    } catch (err) {
      result = false;
    }
  }

  return result;
}

function createSymlinkSync(from, to) {
  if (!fs.existsSync(from)) {
    throw new Error(`File ${from} does not exist`);
  }

  if (doesFileOrSymlinkAlreadyExistSync(to)) {
    const stats = fs.lstatSync(to);

    if (stats.isFile()) {
      // file exists and not a symlink, make a copy.
      console.log(`File ${to} already exists, making a copy`);
      fs.copyFileSync(to, `${to}.backup`);
    }

    if (stats.isFile() || stats.isSymbolicLink()) {
      console.log(`Unlinking existing ${to}`);
      fs.unlinkSync(to);
    }
  }

  // ensure target directory exists
  const targetDirectoryName = path.dirname(to);
  if (!fs.existsSync(targetDirectoryName)) {
    console.log(`Creating ${targetDirectoryName}`);
    fs.mkdirSync(targetDirectoryName, { recursive: true });
  }

  fs.symlinkSync(from, to);
}

createSymlinkSync(path.resolve("zshrc"), path.join(HOME_DIR, ".zshrc"));
createSymlinkSync(path.resolve("emacs"), path.join(HOME_DIR, ".emacs"));
createSymlinkSync(path.resolve("vimrc"), path.join(HOME_DIR, ".vimrc"));
createSymlinkSync(path.resolve("tmux.conf"), path.join(HOME_DIR, ".tmux.conf"));

createSymlinkSync(
  path.resolve("kitty.conf"),
  path.join(HOME_DIR, ".config", "kitty", "kitty.conf")
);

createSymlinkSync(
  path.resolve("alacritty.yml"),
  path.join(HOME_DIR, ".config", "alacritty", "alacritty.yml")
);

