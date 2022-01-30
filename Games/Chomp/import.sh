#id=$$
id='test'
export PATH="~/.config/chomp/bin":"$PATH"
mkdir -p ~/.config/chomp/tmp/$id
cd ~/.config/chomp/tmp/$id
trap "rm -r ~/.config/chomp/tmp/$id" EXIT
