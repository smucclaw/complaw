#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.HaXml curl
#! /bin/bash

set -x

XSD_DIR=xsd
LOG_DIR=log
HS_DIR=HS

mkdir -p $XSD_DIR $LOG_DIR $HS_DIR

xsd_files=(DI DC DMNDI13 DMN13)

# Download schemas:
curl https://www.omg.org/spec/DMN/20191111/DMN13.xsd -o $XSD_DIR/DMN13.xsd
curl https://www.omg.org/spec/DMN/20191111/DMNDI13.xsd -o $XSD_DIR/DMNDI13.xsd
curl https://www.omg.org/spec/DMN/20180521/DI.xsd -o $XSD_DIR/DI.xsd 
curl https://www.omg.org/spec/DMN/20180521/DC.xsd -o $XSD_DIR/DC.xsd 


for file in ${xsd_files[@]}; do
  XsdToHaskell $XSD_DIR/$file.xsd $HS_DIR/$file\'xsd.hs > $LOG_DIR/$file.log
done

# XsdToHaskell DMNDI13.xsd DMNDI13.hs > DMNDI13.log
# XsdToHaskell DMN13.xsd DMN13.hs > DMN13.log
