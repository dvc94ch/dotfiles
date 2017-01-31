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
