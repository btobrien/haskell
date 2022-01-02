
export PATH="~/.config/chomp/bin":"$PATH"
mkdir -p ~/.config/chomp/tmp/$$
cd ~/.config/chomp/tmp/$$
trap "rm -r ~/.config/chomp/tmp/$$" EXIT
