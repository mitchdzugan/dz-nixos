# XONSH WEBCONFIG START
# $XONSH_COLOR_STYLE = 'paraiso-dark'
# XONSH WEBCONFIG END
xontrib load coconut
fastfetch \
    --shell-format $(xonsh -V) \
		--separator-output-color black \
		--logo-type file-raw \
		--logo ~/.config/fastfetch/logo.nix.ascii
