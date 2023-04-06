#!/bin/bash
# @Author: Juan Manuel Soltero Sánchez
# @Date:   2023-04-06 16:19:14
# @Last Modified by:   Juan Manuel Soltero Sánchez
# @Last Modified time: 2023-04-06 16:24:06
###############################################################################
ACTUALDIR=$(pwd)

DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CURDIR=$DIR
cd "$DIR/"
###############################################################################


/bin/convert Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.256.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.128.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.096.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.072.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.064.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.048.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.032.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.024.png \
Oxygen-Icons.org-Oxygen-Apps-preferences-web-browser-cache.016.png \
icono_app.ico







