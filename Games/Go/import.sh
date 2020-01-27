
export PATH="~/.config/go/bin":"$PATH"
mkdir -p ~/.config/go/tmp/$$
cd ~/.config/go/tmp/$$
trap "rm -r ~/.config/go/tmp/$$" EXIT
