# Load antigen and zsh plugins.
source ~/.include/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme robbyrussell

antigen apply

# Export xdg directories.
test -f ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs && \
source ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs

# Load aliases.
source ~/.aliases

# Set LD_LIBRARY_PATH
if [ -z $GUIX_ENVIRONMENT ]; then
else
    export LD_LIBRARY_PATH=$LIBRARY_PATH
fi

# Add cargo binaries to environment and set CC for cargo
source $HOME/.cargo/env
export PATH=/bin:$HOME/repos/freedom-e-sdk/toolchain/bin:$PATH
