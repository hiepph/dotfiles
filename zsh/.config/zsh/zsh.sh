export LC_ALL="en_US.UTF-8"

# custom binary dirs
export PATH="/opt/local/bin:$HOME/go/bin:$HOME/.local/bin:$HOME/bin/:$HOME/.emacs.d/bin:$HOME/.cargo/bin:$HOME/Projects/h-lab/helper/bin:$HOME/dotfiles/scripts/bin:$PATH"

# direnv
eval "$(direnv hook zsh)"

# atuin
eval "$(atuin init zsh)"

# Nvm
source /opt/local/share/nvm/init-nvm.sh

# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PATH:$PYENV_ROOT/shims:$PYENV_ROOT/bin"

# Kubernetes
export KUBECONFIG=$HOME/.kube/kubeconfig
alias k="kubectl"

# THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
