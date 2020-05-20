//@ts-check

const fs = require("fs");
const path = require("path");
const https = require("https");
const { exec } = require("child_process");
const sudo = require("sudo-prompt");

const OS = {
  OSX: "OSX",
  LINUX: "LINUX",
};

const PACKAGE_MANAGER = {
  APT: (packageName) => ({
    install() {
      return runCommand(`apt-get install -y ${packageName}`, { shouldLog: false, sudo: true });
    },
    shouldInstall() {
      return runCommand(`apt list ${packageName}`, { shouldLog: false, sudo: false })
        .then(output => !output.toLowerCase().includes("[installed]"));
    },
  }),
  HOMEBREW: (packageName) => ({
    install() {
      // TODO
    },
    shouldInstall() {
      // TODO
    }
  }),
  NPM: (packageName) => ({
    install() {
      return runCommand(`npm install -g ${packageName}`, { shouldLog: false, sudo: true });
    },
    shouldInstall() {
      return runCommand("npm list -g --depth=0", { shouldLog: false, sudo: false })
        .then((output) => !output.includes(packageName));
    }
  }),
  SKIP: (_packageName) => ({
    install() {
      return Promise.reject("This should never be called");
    },
    shouldInstall() {
      return Promise.resolve(false);
    }
  })
}

const REQUIRED_PACKAGES = [
  {
    name: "zsh",
    url: "https://www.zsh.org",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("zsh"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "tmux",
    url: "https://github.com/tmux/tmux",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("tmux"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "vim",
    url: "https://github.com/vim/vim",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("vim"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "tldr",
    url: "https://github.com/tldr-pages/tldr",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.NPM("tldr"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "bat",
    url: "https://github.com/sharkdp/bat",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("bat"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "ripgrep",
    url: "https://github.com/BurntSushi/ripgrep",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("ripgrep"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "osx coreutils",
    url: "https://www.gnu.org/software/coreutils",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.SKIP(),
      [OS.OSX]: PACKAGE_MANAGER.HOMEBREW("coreutils"),
    }
  },
  {
    name: "kitty terminal",
    url: "https://github.com/kovidgoyal/kitty",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("kitty"),
      [OS.OSX]: {} // TODO
    }
  },
  {
    name: "Fira Code font",
    url: "https://github.com/tonsky/FiraCode",
    install: {
      [OS.LINUX]: PACKAGE_MANAGER.APT("fonts-firacode"),
      [OS.OSX]: {} // TODO
    }
  },
];

const HOME_DIR = require("os").homedir();
const COLOR = {
  CYAN: "\x1b[36m",
  YELLOW: "\x1b[33m",
  GREEN: "\x1b[32m",
  BOLD: "\x1b[1m",
  RED: "",
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

function red(s) {
  return COLOR.RED + s + COLOR.RESET;
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

function runCommand(command, options = { shouldLog: true, sudo: false }) {
  return new Promise((resolve, reject) => {
    if (options.shouldLog) {
      console.log(`Running ${cyan(command)}`);
    }
    if (options.sudo) {
      sudo.exec(command, { name: "install script" }, (err, stdout, stderr) => {
        if (err || stderr) {
          return reject(err || stderr);
        }
        resolve(stdout);
      });
    } else {
      exec(command, (err, stdout, stderr) => {
        if (err) {
          return reject(stderr);
        }
        resolve(stdout);
      });
    }
  });
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
          fs.unlink(destinationFilePath, (unlinkError) => {
            if (unlinkError) {
              console.error(red(`Failed to unlink file ${destinationFilePath} after downloading it.`));
            }
            reject(error);
          });
        });
    }
  });
}

function copyFile(src, dest) {
  const targetDirectoryName = path.dirname(dest);
  ensureDirectorySync(targetDirectoryName);

  return new Promise((resolve, reject) => {
    console.log(`Copying ${cyan(src)} to ${cyan(dest)}`);
    fs.copyFile(src, dest, error => {
      if (error) {
        reject(error);
      }

      resolve();
    });
  });
}

function createSymlinksForDotfiles() {
  createSymlinkSync(path.resolve(".zshrc"), path.join(HOME_DIR, ".zshrc"));
  createSymlinkSync(path.resolve(".emacs"), path.join(HOME_DIR, ".emacs"));
  createSymlinkSync(path.resolve(".vimrc"), path.join(HOME_DIR, ".vimrc"));
  createSymlinkSync(
    path.resolve(".tmux.conf"),
    path.join(HOME_DIR, ".tmux.conf")
  );

  ensureDirectorySync(path.join(HOME_DIR, ".config", "kitty"))
  createSymlinkSync(
    path.resolve("kitty.conf"),
    path.join(HOME_DIR, ".config", "kitty", "kitty.conf")
  );
}

function section(title) {
  console.log(bold(`\n/* ${title} */\n`));
}

async function main() {
  const os = OS.LINUX;

  if (process.platform === "darwin") {
    // TODO: ensure homebrew is installed
  }

  section("Installing required packages");
  for (let package of REQUIRED_PACKAGES) {
    if (await package.install[os].shouldInstall()) {
      console.log(`Installing package ${cyan(package.name)} (${bold(package.url)})...`);
      await package.install[os].install();
    } else {
      console.log(`Package ${cyan(package.name)} (${bold(package.url)}) is already installed`);
    }
  }

  section("Cloning submodules");
  const submodulesCommandOutput = await runCommand("git submodule update --init --recursive");
  if (submodulesCommandOutput) {
    console.log(submodulesCommandOutput);
  }

  section("Creating symlinks for the dotfiles");
  createSymlinksForDotfiles();

  section("Installing plug.vim");
  await httpGetToFile(
    "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim",
    path.join(HOME_DIR, ".vim", "autoload", "plug.vim")
  );

  section("Installing vim color theme");
  await copyFile(
    path.resolve(".", "Apprentice", "colors", "apprentice.vim"),
    path.join(HOME_DIR, ".vim", "colors", "apprentice.vim")
  );

  section("Installing vim plugins");
  await runCommand("vim +PlugInstall +qall > /dev/null");

  section("Changing default shell to zsh");
  const zshPath = await runCommand("which zsh", { sudo: false, shouldLog: false });
  // TODO: this requires password, ask for it
  await runCommand(`chsh -s ${zshPath.trim()} ${process.env["USER"]}`)
}

main()
  .then(() => console.log(bold(green("\nDone\n"))))
  .catch(err => {
    console.error(err);
    process.exit(1);
  });
