source "$(blesh-share)"/ble.sh --attach=none

function cdproj() {
	cd $(codeProject.py) "$@"
}

function pushdproj() {
	pushd $(codeProject.py) "$@"
}

. ~/.nix-profile/share/git/contrib/completion/git-prompt.sh
. ~/.nix-profile/share/git/contrib/completion/git-completion.bash

export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWDIRTYSTATE=1
# export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCONFLICTSTATE="yes"

whoami_=$(whoami)
hostname_=$(hostname)

function prompt_right() {
	res=""
	if [ $1 -gt 0 ]; then
		res+="\e[0;1;31m$1 "
	else
		res+="\e[0;1;32m"
	fi
	res+="⦗"
	if [[ -n $IN_NIX_SHELL ]]; then
		res+="\e[0;1;34mnix"
		res+="\e[0;1;97m\$"
	fi
	res+="\e[0;1;36m$whoami_"
	res+="\e[0;1;97m@"
	res+="\e[0;1;35m$hostname_"
	if [ $1 -gt 0 ]; then
		res+="\e[0;1;31m"
	else
		res+="\e[0;1;32m"
	fi
	res+="⦘\e[0m"
	echo -e $res
}

function prompt_left() {
	res=""
	if [ $1 -gt 0 ]; then
		res+='\[\e[31;1m\]'
	else
		res+='\[\e[32;1m\]'
	fi
	res+='⦗'
	res+='\[\e[34;1m\]\w\[\e[0m\]${PS1_CMD1}'
	if [ $1 -gt 0 ]; then
		res+='\[\e[31;1m\]'
	else
		res+='\[\e[32;1m\]'
	fi
	res+='⦘'
	res+='\[\e[33m\]› \[\e[0m\]'
	echo -e $res
}

function prompt() {
	local EXIT="$?"
	PS1_CMD1=$(__git_ps1 " (%s)")
	psl=$(prompt_left $EXIT)
	psr=$(prompt_right $EXIT)
	psr_stripped=$(sed "s,\x1B\[[0-9;]*[a-zA-Z],,g" <<<"$psr")
	local Save='\e[s'
	local Rest='\e[u'
	PS1="\[${Save}\e[${COLUMNS:-$(tput cols)}C\e[${#psr_stripped}D${psr}${Rest}\]${psl}"
}
PROMPT_COMMAND=prompt


alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

fastfetch \
	--separator-output-color black \
	--logo-width 37 \
	--logo-height 17 \
	--logo-padding-left 1 \
	--logo-padding-top 3 \
	--logo-padding-right 3 \
	--logo-type kitty-direct \
	--logo ~/.config/fastfetch/logo.nix.2.png

[[ ! ${BLE_VERSION-} ]] || ble-attach
