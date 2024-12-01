################################################################################
#
# common.sh
#
# The common functionality for bash scripts
#
################################################################################

########################################
# Configuration
########################################

# Unset this variable to terminate color usage
USECOLOR=Yes

# set colors
if [ -n "$USECOLOR" ]
then
    DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
    source "$DIR/color.sh"
fi

LINESEP="--------------------------------------------------------------------------------"


########################################
# Utility Functions
########################################

# log(msg)
log () {
    local msg="$1" dt=$(date +"%Y-%m-%d %H:%M:%S")
    local color="$GREEN"
    echo -e "${color}[$dt]${COLOR_RESET} $msg"
}

# log_error(msg)
log_error () {
    local msg="$1" dt=$(date +"%Y-%m-%d %H:%M:%S")
    local color="$RED"
    echo -e "${color}[$dt] $msg${COLOR_RESET}"
}

hdr () {
    local text="$1"
    local color="$BICYAN"
    echo -e "${color}${LINESEP}${COLOR_RESET}"
    echo -e "${color}${text}${COLOR_RESET}"
    echo -e "${color}${LINESEP}${COLOR_RESET}"
}
