# Style the command line prompt
ENDCOLOR="\e[0m"
NAVY="\e[44m"
BLACKBG="\e[40m"
WHITE="\e[97m"
CYAN="\e[36m"
GREEN="\e[32m"
PURPLE="\e[35m"

PS1="$PURPLE\[\$(date +%H:%M:%S)\] $GREEN\u $CYAN\w $ENDCOLOR\n$ \[\e]2;\j - \W\a\]"
