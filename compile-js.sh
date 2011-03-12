#!/bin/bash

java -jar /home/noah/bin/compiler.jar \
--js ./priv/www/js/jquery-1.4.2.min.js \
--js ./priv/www/js/jquery-ui-1.8.4.custom.min.js \
--js ./priv/www/js/json2.js \
--js ./priv/www/js/underscore-min.js \
--js ./priv/www/js/underscore.strings-min.js \
--js ./priv/www/js/jquery.hotkeys.js \
--js ./priv/www/js/base64.js \
--js ./priv/www/js/application.js > ./priv/www/js/app.js
