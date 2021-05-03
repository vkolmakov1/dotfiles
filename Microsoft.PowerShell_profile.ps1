# Import-Module posh-git

# Essentials
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineOption -MaximumHistoryCount 1000000
Set-PSReadlineOption -HistoryNoDuplicates # no dups in search history
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Aliases

## General
Set-Alias open Explorer.exe
Set-Alias which Get-Command
Set-Alias l Get-ChildItemColor -Option AllScope
Set-Alias ls Get-ChildItemColorFormatWide -Option AllScope

function JumpTo-Mark {
  param(
    [string]
    $Mark
  )
  # TODO: read from file
  $Marks = @{}

  if ($Marks.ContainsKey($Mark)) {
    Push-Location $Marks[$Mark]
  } else {
    Write-Host "Mark '$Mark' not found"
  }
}
Set-Alias j JumpTo-Mark # my script



## git
function git-status { git status -sb }
Set-Alias gsb git-status

function git-hist {git log --oneline --graph --decorate --all}
Set-Alias gh git-hist

function git-diff { git diff }
Set-Alias gd git-diff

## npm
function npm-run-start {npm run start}
Set-Alias nrs npm-run-start

function npm-run-build {npm run build}
Set-Alias nrb npm-run-build

function npm-run-test {npm run test}
Set-Alias nrt npm-run-test

function npm-run-dev {npm run dev}
Set-Alias nrd npm-run-dev

## vim
# Because command line vim doesn't seem to work too well on Windows
Set-Alias vim gvim

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
