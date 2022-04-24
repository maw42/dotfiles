#!/usr/bin/env sh

VERACRYPT_DEV_PATH="/dev/` ls -la /dev/disk/by-id/ | grep usb-Intenso_External_USB_3.0_20170504106AF-0:0 | awk '{print $11}' | sed s/[\.\/]//g `"

printf "unmount extMixedBackup1 (veracrypt13) \n"
printf "path is: $VERACRYPT_DEV_PATH\n"
veracrypt -d "$VERACRYPT_DEV_PATH"
