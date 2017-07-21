# Essentials
Set-PSReadlineOption -EditMode Emacs


# Aliases

## General
Set-Alias open Explorer.exe
Set-Alias which Get-Command

## git
function git-status { git status -sb }
Set-Alias gsb git-status

function git-hist {git log --oneline --graph --decorate --all}
Set-Alias gh git-hist

## npm
function npm-run-start {npm run start}
Set-Alias nrs npm-run-start

function npm-run-build {npm run build}
Set-Alias nrb npm-run-build

function npm-run-test {npm run test}
Set-Alias nrt npm-run-test

function npm-run-dev {npm run dev}
Set-Alias nrd npm-run-dev


# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
