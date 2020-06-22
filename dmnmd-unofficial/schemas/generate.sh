#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p haskellPackages.HaXml curl
#! /bin/bash

set -x

# Download schemas:

curl https://www.omg.org/spec/DMN/20191111/DMN13.xsd -o DMN13.xsd
curl https://www.omg.org/spec/DMN/20191111/DMNDI13.xsd -o DMNDI13.xsd
curl https://www.omg.org/spec/DMN/20180521/DI.xsd -o DI.xsd 
curl https://www.omg.org/spec/DMN/20180521/DC.xsd -o DC.xsd 

xsd_files=(DI DC DMNDI13 DMN13)

for file in ${xsd_files[@]}; do
  XsdToHaskell $file.xsd $file\'xsd.hs > $file.log
done

# XsdToHaskell DMNDI13.xsd DMNDI13.hs > DMNDI13.log
# XsdToHaskell DMN13.xsd DMN13.hs > DMN13.log
