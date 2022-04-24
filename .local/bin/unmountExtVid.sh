#!/usr/bin/env sh

VERACRYPT_DEV_PATH="/dev/` ls -la /dev/disk/by-id/ | grep usb-WD_Elements_25A2_57583831413438434E303944-0:0-part1 | awk '{print $11}' | sed s/[\.\/]//g `"

printf "unmount extVid (veracrypt12) \n"
printf "path is: $VERACRYPT_DEV_PATH\n"
veracrypt -d "$VERACRYPT_DEV_PATH"
