# PowerShellGet\Install-Module posh-git -Scope CurrentUser -AllowPrerelease -Force
# Install-Module posh-sshell -Scope CurrentUser

Import-Module PSReadLine
Import-Module posh-git
Import-Module posh-sshell
start-SshAgent

Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineKeyHandler -Key Tab -Function Complete

$Env:EDITOR="emacsclient"

function reload_alias { & $PROFILE.CurrentUserAllHosts }
Set-Alias -Name reload -Value reload_alias

Invoke-Expression (&starship init powershell)

# capture starship prompt
$PromptScript = (Get-Item function:Prompt).ScriptBlock

# update window title and reuse starship prompt
function Prompt {
    $PreservedExitStatus = $? # Before doing anything else, capture current $?

    $CurrentWorkingDirectory = Split-Path -Path ((Get-Location).Path.Replace($Env:USERNAME, "~")) -Leaf
    $CurrentWorkingProcess = (Get-Process -Id $PID).ProcessName
    $Host.UI.RawUI.WindowTitle = "$CurrentWorkingDirectory - $CurrentWorkingProcess" # Set Title

    if ($? -ne $PreservedExitStatus) {
        # Reset $? to False
        Write-Error "" -ErrorAction Ignore # Powershell 7+
        # Source: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#section-1
    }
    Invoke-Command $PromptScript
}
