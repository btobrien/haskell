
set -e
export PATH="~/.config/noise/bin":"$PATH"
mkdir ~/.config/noise/tmp/$$
cd ~/.config/noise/tmp/$$
trap "rm -r ~/.config/noise/tmp/$$" EXIT
