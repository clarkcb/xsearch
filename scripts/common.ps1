################################################################################
#
# common.ps1
#
# The common functionality for PowerShell scripts
#
################################################################################

########################################
# Utility Functions
########################################

# Generate a log string
function GetLogString
{
	param([string]$msg)
	$ds = '{0:yyyy-MM-dd hh:mm:ss tt}' -f (Get-Date)
	'[{0}] {1}' -f $ds, $msg
}

# Write a log string to the console
function Log
{
	param([string]$msg)
	$s = (GetLogString $msg)
	Write-Host $s
}

# Write an error to the console (colorized)
function PrintError
{
	param([string]$msg)
	$s = (GetLogString ('ERROR: ' + $msg))
	Write-Host $s -BackgroundColor Black -ForegroundColor Red
}

# Write an error to the console and exit
function ExitWithError
{
	param([string]$errmsg)
	PrintError "$errmsg`n"
	exit
}
