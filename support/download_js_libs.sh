#/bin/bash
cd $(dirname $0)
SUPPORT_DIR="$PWD"
cd "$SUPPORT_DIR/../priv/www/lib/"
git clone https://github.com/jquery/jquery-ui.git
git clone https://github.com/localhost/jquery-fieldselection.git
git clone https://github.com/carhartl/jquery-cookie.git
git clone https://github.com/jasonkarns/css-reset.git
