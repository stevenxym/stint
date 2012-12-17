#!/bin/sh

#
# copy necessary files for senario test cases to ../compiler/
#

if [ "$1" == "-c" ] ; then
	rm -f ../compiler/sn*
else
	cp sn_clean.sh ../compiler/
	cp sn1* ../compiler/
fi
