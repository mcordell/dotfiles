#! /usr/bin/env zsh

tested=$(find ./zsh/boot_strap_functions -type f | grep -v helpers | fzf)
source zsh/boot_strap_functions/helpers
source $tested

if [[ -z $1 ]]; then
	$(echo $tested | sed -e "s{./zsh/boot_strap_functions/{{")
else
	$1
fi
