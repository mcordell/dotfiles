update_brewfile:
    rm Brewfile
    brew bundle dump
    git add -p Brewfile
    git commit -m "brew: update brewfile"
