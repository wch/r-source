#!/bin/bash

cp /etc/R/Renviron /tmp/
Sysfile=/etc/papersize
Rfile=/tmp/Renviron

syspaper=`cat $Sysfile`
echo "Papersize in /etc is $syspaper"

rpaper=`grep ^R_PAPERSIZE $Rfile | cut -d"'" -f2`
echo "Papersize in Renviron is $rpaper"

if [ "$syspaper" != "$rpaper" ]; then
    echo "Need to replace $rpaper with $syspaper"

    perl -p -i -e "s|\'$rpaper\'|\'$syspaper\'|"    "$Rfile"
fi

grep PAPERSIZE $Rfile
