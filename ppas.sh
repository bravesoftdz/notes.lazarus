#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/biohazard/Projects/lazarus1/project1
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf32-i386 -m elf_i386  --build-id --dynamic-linker=/lib/ld-linux.so.2    -L. -o /home/biohazard/Projects/lazarus1/project1 /home/biohazard/Projects/lazarus1/link.res
if [ $? != 0 ]; then DoExitLink /home/biohazard/Projects/lazarus1/project1; fi
IFS=$OFS
