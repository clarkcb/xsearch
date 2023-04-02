################################################################################
#
# common.ps1
#
# The common functionality for PowerShell scripts
#
################################################################################

########################################
# Configuration
########################################

# Unset this variable to terminate color usage
$useColor=$TRUE

$lineSep="--------------------------------------------------------------------------------"


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
	# $s = (GetLogString $msg)
	$ds = '{0:yyyy-MM-dd hh:mm:ss tt}' -f (Get-Date)
	if ($useColor) {
		Write-Host "[$ds]" -ForegroundColor Green -NoNewline
		Write-Host " $msg"
	} else {
		Write-Host "[$ds] $msg"
	}
}

# Write an error to the console (colorized)
function PrintError
{
	param([string]$msg)
	$s = (GetLogString ('ERROR: ' + $msg))
	if ($useColor) {
		Write-Host $s -BackgroundColor Black -ForegroundColor Red
	} else {
		Write-Host $s
	}
}

# Write an error to the console and exit
function ExitWithError
{
	param([string]$errmsg)
	PrintError "$errmsg`n"
	exit
}

function Hdr {
	param ([string]$title)
	if ($useColor) {
		Write-Host $lineSep -ForegroundColor Cyan
		Write-Host $title -ForegroundColor Cyan
		Write-Host $lineSep -ForegroundColor Cyan
	} else {
		Write-Host $lineSep
		Write-Host $title
		Write-Host $lineSep
	}
}
