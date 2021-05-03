//@ts-check

const fs = require("fs");
const path = require("path");
const https = require("https");
const { exec } = require("child_process");
const sudo = require("sudo-prompt");
const { EOL } = require("os");
const readline = require("readline");

const OS = {
	OSX: "OSX",
	LINUX: "LINUX",
	WINDOWS: "WINDOWS",
};

const PACKAGE_MANAGER = {
	APT: (packageName) => ({
		install() {
			return runCommand(`apt-get install -y ${packageName}`, {
				shouldLog: false,
				sudo: true,
			});
		},
		shouldInstall() {
			return runCommand(`apt list ${packageName}`, {
				shouldLog: false,
				sudo: false,
			}).then((output) => !output.toLowerCase().includes("[installed]"));
		},
	}),
	BREW: (packageName) => ({
		install() {
			return runCommand(`brew install ${packageName}`, {
				shouldLog: false,
				sudo: false,
			});
		},
		shouldInstall() {
			return runCommand(`brew ls --versions ${packageName}`, {
				shouldLog: false,
				sudo: false,
			})
				.then((output) => !output) // if command succeeds and something is returned, it's already installed
				.catch((_err) => {
					// if this command returns with an error, package was not found
					return true;
				});
		},
	}),
	BREW_CASK: (packageName) => ({
		install() {
			return runCommand(`brew install --cask ${packageName}`, {
				shouldLog: false,
				sudo: false,
			});
		},
		shouldInstall() {
			return runCommand(`brew ls --cask --versions ${packageName}`, {
				shouldLog: false,
				sudo: false,
			})
				.then((output) => !output) // if command succeeds and something is returned, it's already installed
				.catch((_err) => {
					// if this command returns with an error, package was not found
					return true;
				});
		},
	}),
	NPM: (packageName) => ({
		install() {
			return runCommand(`npm install -g ${packageName}`, {
				shouldLog: false,
				sudo: true,
			}).catch((err) => {
				if (
					typeof err === "string" /* err is stderr output */ &&
					!err.includes("ERR")
				) {
					// not a real error, probably failed with some warnings but still installed
					return Promise.resolve();
				}
				// could be a real error if it includes ERR substring
				return Promise.reject(err);
			});
		},
		shouldInstall() {
			return runCommand("npm list -g --depth=0", {
				shouldLog: false,
				sudo: false,
			}).then((output) => !output.includes(packageName));
		},
	}),
	SCOOP: (packageName) => ({
		install() {
			return runCommand(`powershell -command "scoop install ${packageName}"`, {
				shouldLog: false,
				sudo: false,
			});
		},
		shouldInstall() {
			// no harm in rerunning the install command here
			return Promise.resolve(true);
		},
	}),
	SKIP: {
		install() {
			return Promise.reject("This should never be called");
		},
		shouldInstall() {
			return Promise.resolve(false);
		},
	},
};

const REQUIRED_PACKAGES = [
	{
		name: "zsh",
		url: "https://zsh.org",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("zsh"),
			[OS.OSX]: PACKAGE_MANAGER.BREW("zsh"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "tmux",
		url: "https://github.com/tmux/tmux",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("tmux"),
			[OS.OSX]: PACKAGE_MANAGER.BREW("tmux"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "vim",
		url: "https://github.com/vim/vim",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("vim"),
			[OS.OSX]: PACKAGE_MANAGER.BREW("vim"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("vim"),
		},
	},
	{
		name: "tldr",
		url: "https://github.com/tldr-pages/tldr",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.NPM("tldr"),
			[OS.OSX]: PACKAGE_MANAGER.NPM("tldr"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "bat",
		url: "https://github.com/sharkdp/bat",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("bat"),
			[OS.OSX]: PACKAGE_MANAGER.BREW("bat"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "ripgrep",
		url: "https://github.com/BurntSushi/ripgrep",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("ripgrep"),
			[OS.OSX]: PACKAGE_MANAGER.BREW("ripgrep"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "osx coreutils",
		url: "https://gnu.org/software/coreutils",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("coreutils"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "Fira Code font",
		url: "https://github.com/tonsky/FiraCode",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.APT("fonts-firacode"),
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("font-fira-code"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("firacode"),
		},
	},
	{
		name: "trash",
		url: "https://hasseg.org/trash",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("trash"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "spectacle",
		url: "https://spectacleapp.com",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("spectacle"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "VS Code",
		url: "https://code.visualstudio.com",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("visual-studio-code"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("vscode"),
		},
	},
	{
		name: "licecap",
		url: "https://cockos.com/licecap",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("licecap"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "Karabiner Elements",
		url: "https://pqrs.org/osx/karabiner",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("karabiner-elements"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "exa",
		url: "https://github.com/ogham/exa",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("exa"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "delta",
		url: "https://github.com/dandavison/delta",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("git-delta"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("delta"),
		},
	},
	{
		name: "fd",
		url: "https://github.com/sharkdp/fd",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("fd"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("fd"),
		},
	},
	{
		name: "gitui",
		url: "https://github.com/extrawurst/gitui",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW("gitui"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("gitui"),
		},
	},
	{
		name: "alacritty",
		url: "https://github.com/alacritty/alacritty",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.BREW_CASK("alacritty"),
			[OS.WINDOWS]: PACKAGE_MANAGER.SKIP,
		},
	},
	{
		name: "insomnia",
		url: "https://github.com/Kong/insomnia",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.SKIP,
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("insomnia"),
		},
	},
	{
		name: "nuget",
		url: "https://www.nuget.org/",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.SKIP,
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("nuget"),
		},
	},
	{
		name: "nvm",
		url: "https://github.com/nvm-sh/nvm",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.SKIP,
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("nvm"),
		},
	},
	{
		name: "gsudo",
		url: "https://github.com/gerardog/gsudo",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.SKIP,
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("gsudo"),
		},
	},
	{
		name: "sharex",
		url: "https://getsharex.com/",
		install: {
			[OS.LINUX]: PACKAGE_MANAGER.SKIP,
			[OS.OSX]: PACKAGE_MANAGER.SKIP,
			[OS.WINDOWS]: PACKAGE_MANAGER.SCOOP("sharex"),
		},
	},
];

const HOME_DIR = require("os").homedir();
const COLOR = {
	CYAN: "\x1b[36m",
	YELLOW: "\x1b[33m",
	GREEN: "\x1b[32m",
	BOLD: "\x1b[1m",
	RED: "",
	RESET: "\x1b[0m",
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
					return reject(stderr || err);
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
	return fs.symlinkSync(from, to);
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
				.get(url, (response) => {
					response.pipe(fileInputStream);
					fileInputStream.on("finish", () => {
						fileInputStream.close();
						resolve();
					});
				})
				.on("error", (error) => {
					// clean up
					fs.unlink(destinationFilePath, (unlinkError) => {
						if (unlinkError) {
							console.error(
								red(
									`Failed to unlink file ${destinationFilePath} after downloading it.`
								)
							);
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
		fs.copyFile(src, dest, (error) => {
			if (error) {
				reject(error);
			}

			resolve();
		});
	});
}

async function createSymlinksForDotfiles(os) {
	createSymlinkSync(path.resolve(".zshrc"), path.join(HOME_DIR, ".zshrc"));
	createSymlinkSync(path.resolve(".emacs"), path.join(HOME_DIR, ".emacs"));
	createSymlinkSync(path.resolve(".vimrc"), path.join(HOME_DIR, ".vimrc"));
	createSymlinkSync(path.resolve(".vimrc"), path.join(HOME_DIR, ".gvimrc"));
	createSymlinkSync(
		path.resolve(".ideavimrc"),
		path.join(HOME_DIR, ".ideavimrc")
	);

	// Copying gitconfig instead of symlinking it to make sure that we can set
	// a different email address on different machines
	copyFile(
		path.resolve(".gitconfig"),
		path.join(HOME_DIR, ".gitconfig")
	);

	createSymlinkSync(
		path.resolve(".tmux.conf"),
		path.join(HOME_DIR, ".tmux.conf")
	);

	ensureDirectorySync(path.join(HOME_DIR, ".config", "kitty"));
	createSymlinkSync(
		path.resolve("kitty.conf"),
		path.join(HOME_DIR, ".config", "kitty", "kitty.conf")
	);
	createSymlinkSync(
		path.resolve(".alacritty.yml"),
		path.join(HOME_DIR, ".alacritty.yml")
	);
	if (os === OS.WINDOWS) {
		const profileLocationStdout = await runCommand(
			`powershell -command "echo $Profile"`,
			{
				sudo: false,
				shouldLog: false,
			}
		);

		createSymlinkSync(
			path.resolve("Microsoft.Powershell_profile.ps1"),
			path.resolve(profileLocationStdout.trim())
		);
	}
}

function getInput(question) {
	const interface = readline.createInterface({
		input: process.stdin,
		output: process.stdout,
	});

	return new Promise((resolve) => {
		interface.question(`${bold(question)} > `, (response) => {
			interface.close();
			resolve(response);
		});
	});
}

function section(title) {
	console.log(bold(`\n/* ${title} */\n`));
}

async function longRunningOperation(message, operation) {
	process.stdout.write(`${message}...`);
	try {
		await operation();
		process.stdout.write(green("OK\n"));
	} catch (err) {
		process.stdout.write(red("ERROR\n"));
		return Promise.reject(err);
	}

	return Promise.resolve();
}

const preSetup = {
	[OS.OSX]: async () => {
		await longRunningOperation(
			`Checking if ${cyan("homebrew")} is installed (${bold(
				"https://brew.sh"
			)})`,
			async () => {
				const brewExecutable = await runCommand("which brew", {
					sudo: false,
					shouldLog: false,
				});
				if (!brewExecutable) {
					console.log("Cannot find homebrew executable. Installing homebrew");
					await runCommand(
						`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"`
					);
				}
			}
		);
		// Set up cask-fonts tap - required for fira-code font to be installed
		const BREW_FONTS_TAP_NAME = "homebrew/cask-fonts";
		const availableBrewTaps = await runCommand("brew tap", {
			shouldLog: false,
			sudo: false,
		});
		if (!availableBrewTaps.includes(BREW_FONTS_TAP_NAME)) {
			await runCommand(`brew tap ${BREW_FONTS_TAP_NAME}`);
		}
	},
	[OS.LINUX]: () => Promise.resolve(),
	[OS.WINDOWS]: async () => {
		// Make sure that admin powershell is running
		console.log("Making sure that the script is running with an admin user (elevated permissions are required to create symlinks on Windows)");
		const adminRoleCheckStdout = await runCommand(
			`powershell -command "(New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)"`,
			{ sudo: false, shouldLog: false },
		);
		const hasAdminPermissions = adminRoleCheckStdout.trim().toLowerCase().includes('true')
		if (!hasAdminPermissions) {
			console.error("This script requires admin permissions on Windows. Start an admin powershell session and try again.");
			process.exit(1);
		}
		
		// install scoop
		await longRunningOperation(
			`Checking if ${cyan("scoop")} is installed (${bold(
				"https://scoop.sh/"
			)})`,
			async () => {
				try {
					await runCommand(`powershell -command "Get-Command scoop"`, {
						sudo: false,
						shouldLog: false,
					});
				} catch (err) {
					console.log("Cannot find scoop executable. Installing scoop");
					await runCommand(
						`powershell -command "Set-ExecutionPolicy RemoteSigned -scope CurrentUser"`
					);
					await runCommand(
						`powershell -command "iwr -useb get.scoop.sh | iex"`
					);
				}
			}
		);
		console.log("Checking if all of the required scoop buckets are added");
		/** @type {string} */
		const bucketsListStdout = await runCommand(
			`powershell -command "scoop bucket list"`,
			{ sudo: false, shouldLog: false }
		);
		const requiredBuckets = ["extras", "nerd-fonts", "versions"];
		const existingBuckets = bucketsListStdout.split(EOL);
		for (const bucket of requiredBuckets) {
			if (!existingBuckets.includes(bucket)) {
				console.log(`Adding ${bucket} scoop bucket`);
				await runCommand(`powershell -command "scoop bucket add ${bucket}"`);
			}
		}
	},
};

async function main() {
	let os;
	if (process.platform === "darwin") {
		os = OS.OSX;
	} else if (process.platform === "linux") {
		os = OS.LINUX;
	} else if (process.platform === "win32") {
		os = OS.WINDOWS;
	} else {
		return Promise.reject(
			`Error: platform ${cyan(process.platform)} is not supported`
		);
	}

	const emailAddress = await getInput(
		"Enter your email address (will be used to configure git)"
	);

	section("Pre-setup");
	await preSetup[os]();

	section("Installing required packages");
	for (let package of REQUIRED_PACKAGES) {
		if (await package.install[os].shouldInstall()) {
			await longRunningOperation(
				`Installing package ${cyan(package.name)} (${bold(package.url)})`,
				() => {
					return package.install[os].install();
				}
			);
		} else if (package.install[os] === PACKAGE_MANAGER.SKIP) {
			console.log(
				`Skipping package ${cyan(package.name)} (${bold(
					package.url
				)}) - not required on this platform`
			);
		} else {
			console.log(
				`Package ${cyan(package.name)} (${bold(
					package.url
				)}) is already installed`
			);
		}
	}

	section("Cloning submodules");
	const submodulesCommandOutput = await runCommand(
		"git submodule update --init --recursive"
	);
	if (submodulesCommandOutput) {
		console.log(submodulesCommandOutput);
	}

	section("Creating symlinks for the dotfiles");
	await createSymlinksForDotfiles(os);

	section("Setting up git email address");
	runCommand(`git config --global user.email "${emailAddress}"`);

	if (os === OS.WINDOWS) {
		section("Installing plug.vim");
		await httpGetToFile(
			"https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim",
			path.join(HOME_DIR, "vimfiles", "autoload", "plug.vim")
		);
		section("Installing vim color theme");
		await copyFile(
			path.resolve(".", "Apprentice", "colors", "apprentice.vim"),
			path.join(HOME_DIR, "vimfiles", "colors", "apprentice.vim")
		);

		section("Installing vim plugins");
		await runCommand(`powershell -command "vim +PlugInstall +qall"`);
	} else {
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
	}

	// TODO: fixme
	// section("Changing default shell to zsh");
	// const zshPath = await runCommand("which zsh", { sudo: false, shouldLog: false });
	// await runCommand(`chsh -s ${zshPath.trim()} ${process.env["USER"]}`)
}

main()
	.then(() => console.log(bold(green("\nDone\n"))))
	.catch((err) => {
		console.error(err);
		process.exit(1);
	});
