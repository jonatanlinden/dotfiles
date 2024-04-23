# PowerShellGet\Install-Module posh-git -Scope CurrentUser -AllowPrerelease -Force
# Install-Module posh-sshell -Scope CurrentUser

Import-Module PSReadLine
Import-Module posh-git
Import-Module posh-sshell
start-SshAgent

Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineKeyHandler -Key Tab -Function Complete

$Env:EDITOR="emacsclient"

function which { param($bin) Get-Command $bin }

function Invoke-Starship-PreCommand {
    $Host.UI.RawUI.WindowTitle = $((''+$PWD).replace($HOME, '~'))

    # get location from session (so that the pane can be restored)
    $loc = $executionContext.SessionState.Path.CurrentLocation;
    $prompt = "$([char]27)]9;12$([char]7)"
    if ($loc.Provider.Name -eq "FileSystem")
    {
        $prompt += "$([char]27)]9;9;`"$($loc.ProviderPath)`"$([char]27)\"
    }
    $host.ui.Write($prompt)
}

Invoke-Expression (&starship init powershell)
