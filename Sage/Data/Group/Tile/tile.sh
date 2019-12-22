
#!/bin/bash

set -o pipefail
echo
movetile $1 | coat showtile
