#!/bin/zsh

source showdemo-utils.sh; clear
$runcmd --nlgstyle=obfuscated | perl -000 -ple 's/\$8(.*)(the difference)/\$8 of$1$2/gs'
$runcmd --nlgstyle=algebravars; echo ""
$runcmd --nlgstyle=algebra;     echo ""
$runcmd --nlgstyle=algebranums; echo ""
$runcmd ;                       echo ""
$runcmd --nlgstyle=yaml
