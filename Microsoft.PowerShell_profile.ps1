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
if (Get-Command Get-ChildItemColor -ErrorAction SilentlyContinue) {
  Set-Alias l Get-ChildItemColor -Option AllScope
}
if (Get-Command Get-ChildItemColorFormatWide -ErrorAction SilentlyContinue) {
  Set-Alias ls Get-ChildItemColorFormatWide -Option AllScope
}

function Create-Mark {
  param(
    [Parameter(Mandatory = $True)]
    [string]$Mark
  )
  $MarksFilePath = Join-Path $HOME ".vkolmakov-dotfiles-marks"
  $CurrentLocation = (Get-Location).Path.Trim()
  $UpdatedMarksFile = New-TemporaryFile

  if (-not (Test-Path $MarksFilePath)) {
    $null = New-Item $MarksFilePath
  }

  if ($Mark -match "\s+") {
    Write-Error "Mark name must not contain any spaces."
    return
  }

  # In case the mark already exists, remove it and swap it with the updated value
  $null = Get-Content $MarksFilePath | Where-Object {$_ -notmatch "^$([regex]::escape($Mark)) "} | Set-Content $UpdatedMarksFile
  $null = Move-Item $UpdatedMarksFile $MarksFilePath -Force

  $null = Add-Content $MarksFilePath "$Mark $CurrentLocation"

  Write-Host "Created mark $Mark that points to $CurrentLocation"
}
Set-Alias cm Create-Mark

function JumpTo-Mark {
  param(
    [string]
    $Mark
  )
  $MarksFilePath = Join-Path $HOME '.vkolmakov-dotfiles-marks'
  if (-not (Test-Path $MarksFilePath)) {
    $null = New-Item $MarksFilePath
  }

  if (-not $Mark) {
    Write-Host 'Available marks:'
    Write-Host ''
    return Get-Content $MarksFilePath
  }

  foreach ($Line in Get-Content $MarksFilePath) {
    if ($Line.StartsWith("$Mark ")) {
      $null = Push-Location ($Line.TrimStart("$Mark "))
      return
    }
  }

  Write-Host "Mark '$Mark' not found"
}
Set-Alias j JumpTo-Mark

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
