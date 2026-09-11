export GPG_TTY=$TTY

# Point ssh at this machine's gpg-agent — but never at the cost of a forwarded
# one. Over `ssh -A`, sshd hands us a socket under /tmp/ssh-*/agent.*, and
# overwriting it with the local gpg-agent silently breaks every onward hop:
# on a remote box that agent holds nothing ("The agent has no identities"), so
# the failure surfaces one machine later than its cause. Measured on bigbox
# 2026-09-11 — a forwarded agent survived a login shell and was discarded by
# the interactive one, which is this file.
#
# The guard tests SSH_CONNECTION *and* a live socket rather than "is it already
# set", because on macOS launchd presets SSH_AUTH_SOCK to Apple's agent and
# that one we do want to replace. This keeps ssh's and overrides Apple's.
if [[ -n "$SSH_CONNECTION" && -S "$SSH_AUTH_SOCK" ]]; then
  : # forwarded agent — leave it alone
else
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  gpgconf --launch gpg-agent
fi
