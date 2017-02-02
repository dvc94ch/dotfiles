# editor
export EDITOR='emacsclient -c --alternate-editor nano'

# browser
export BROWSER='chromium-browser'

# guix packages
# export GUIX_PACKAGE_PATH='$HOME/repos/guix-nonfree'

# certs
export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt

# gstreamer
export GST_PLUGIN_PATH=$HOME/.guix-profile/lib/gstreamer-1.0:/run/current-system/profile/lib/gstreamer-1.0
