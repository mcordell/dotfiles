update_brewfile:
    rm Brewfile
    brew bundle dump
    git add -p Brewfile
