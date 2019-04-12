#!/bin/sh

exec nc -lkp 80 -e 'cat /etc/hostname'
