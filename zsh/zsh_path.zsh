#! /usr/bin/env zsh
# Paths

path=(
    /usr/local/opt/gnu-sed/libexec/gnubin
	/usr/local/opt/python/libexec/bin
	/usr/local/opt/go/libexec/bin
	/Users/michael/go/bin
	/usr/local/opt/grep/libexec/gnubin
	/Users/michael/Code/shell/temp/git-fuzzy/bin
    $path
)

[ -f "$HOME/.fzf.zsh" ] && source "$HOME/.fzf.zsh"

if [ -f "/opt/homebrew/bin/brew" ]; then
    path=(
        /opt/homebrew/bin
        /opt/homebrew/sbin
        $path
	)
fi

if [ -f "$HOME/.cargo/env" ]; then source "$HOME/.cargo/env"; fi

if [ -d "$HOME/.emacs.d/bin" ]; then
	PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -d "/usr/local/opt/postgresql@12/bin" ]; then
    path+=('/usr/local/opt/postgresql@12/bin' $path)
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/michael/Downloads/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/michael/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/michael/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/michael/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

if [ -f '/System/Library/Frameworks/JavaScriptCore.framework/Versions/Current/Helpers/jsc' ]; then
	PATH="/System/Library/Frameworks/JavaScriptCore.framework/Versions/Current/Helpers:$PATH"
fi


if which "brew" &> /dev/null; then
	if [ -d "$(brew --prefix)/opt/findutils/libexec/gnubin" ]; then
		PATH="$(brew --prefix)/opt/findutils/libexec/gnubin:$PATH"
	fi
	if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
		PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
	fi
fi

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path PATH
