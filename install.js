const fs = require("fs");
const path = require("path");
const https = require("https");

const HOME_DIR = require("os").homedir();
const COLOR = {
  CYAN: "\x1b[36m",
  YELLOW: "\x1b[33m",
  GREEN: "\x1b[32m",
  BOLD: "\x1b[1m",
  RESET: "\x1b[0m"
};

function cyan(s) {
  return COLOR.CYAN + s + COLOR.RESET;
}

function yellow(s) {
  return COLOR.YELLOW + s + COLOR.RESET;
}

function green(s) {
  return COLOR.GREEN + s + COLOR.RESET;
}

function bold(s) {
  return COLOR.BOLD + s + COLOR.RESET;
}

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

function ensureDirectorySync(directoryName) {
  if (!fs.existsSync(directoryName)) {
    console.log(`Creating ${cyan(directoryName)}`);
    fs.mkdirSync(directoryName, { recursive: true });
  }
}

function createSymlinkSync(from, to) {
  if (!fs.existsSync(from)) {
    throw new Error(`File ${from} does not exist`);
  }

  if (doesFileOrSymlinkAlreadyExistSync(to)) {
    const stats = fs.lstatSync(to);

    if (stats.isFile()) {
      // file exists and not a symlink, make a copy.
      console.log(`File ${yellow(to)} already exists, making a copy`);
      fs.copyFileSync(to, `${to}.backup`);
    }

    if (stats.isFile() || stats.isSymbolicLink()) {
      console.log(`Unlinking existing ${yellow(to)}`);
      fs.unlinkSync(to);
    }
  }

  // ensure target directory exists
  const targetDirectoryName = path.dirname(to);
  ensureDirectorySync(targetDirectoryName);

  console.log(`Creating a symlink ${cyan(to)} -> ${cyan(from)}`);
  fs.symlinkSync(from, to);
}

function httpGetToFile(url, destinationFilePath) {
  return new Promise((resolve, reject) => {
    if (fs.existsSync(destinationFilePath)) {
      console.log(
        `File ${yellow(
          destinationFilePath
        )} already exists, skipping the download from ${cyan(url)}`
      );
      resolve();
    } else {
      const targetDirectoryName = path.dirname(destinationFilePath);
      ensureDirectorySync(targetDirectoryName);
      const fileInputStream = fs.createWriteStream(destinationFilePath);

      console.log(
        `Downloading ${cyan(url)} into ${yellow(destinationFilePath)}...`
      );
      https
        .get(url, response => {
          response.pipe(fileInputStream);
          fileInputStream.on("finish", () => {
            fileInputStream.close();
            resolve();
          });
        })
        .on("error", error => {
          // clean up
          fs.unlink(destinationFilePath);
          reject(error);
        });
    }
  });
}

function createSymlinksForDotfiles() {
  createSymlinkSync(path.resolve("zshrc"), path.join(HOME_DIR, ".zshrc"));
  createSymlinkSync(path.resolve("emacs"), path.join(HOME_DIR, ".emacs"));
  createSymlinkSync(path.resolve("vimrc"), path.join(HOME_DIR, ".vimrc"));
  createSymlinkSync(
    path.resolve("tmux.conf"),
    path.join(HOME_DIR, ".tmux.conf")
  );

  // TODO: fix an issue with .config not existing
  createSymlinkSync(
    path.resolve("kitty.conf"),
    path.join(HOME_DIR, ".config", "kitty", "kitty.conf")
  );

  createSymlinkSync(
    path.resolve("alacritty.yml"),
    path.join(HOME_DIR, ".config", "alacritty", "alacritty.yml")
  );
}

function section(title) {
  return bold(`\n/* ${title} */\n`);
}

async function main() {
  console.log(section("Creating symlinks for the dotfiles"));
  createSymlinksForDotfiles();

  // TODO: clone submodules if not present

  console.log(section("Installing plug.vim"));
  await httpGetToFile(
    "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim",
    path.join(HOME_DIR, ".vim", "autoload", "plug.vim")
  );
}

main()
  .then(() => console.log(bold(green("\nDone\n"))))
  .catch(err => {
    console.error(err);
    process.exit(1);
  });
