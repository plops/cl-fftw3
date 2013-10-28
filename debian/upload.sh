#!/bin/bash -e

dup cl-fftw3 -Ufiles.b9.com -D/home/ftp/cl-fftw3 -C"(umask 022; /home/kevin/bin/remove-old-versions cl-fftw3 latest)" -su $*
