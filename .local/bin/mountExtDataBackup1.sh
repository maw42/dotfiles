#!/usr/bin/env sh

VERACRYPT_DEV_PATH="/dev/` ls -la /dev/disk/by-id/ | grep usb-WD_Elements_10B8_575844314131343538333933-0:0-part1 | awk '{print $11}' | sed s/[\.\/]//g `"

printf "mount extDataBackup1 to veracrypt11 \n"
printf "path is: $VERACRYPT_DEV_PATH\n"
veracrypt -t -k "" --protect-hidden=no "$VERACRYPT_DEV_PATH" "/mnt/veracrypt11"
