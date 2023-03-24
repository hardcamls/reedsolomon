#!/bin/bash 

echo "############################################################"
echo "Encoding file"
echo "############################################################"

dune exec ./bin/rs.exe -- files encode README.md test/README.md.coded.iter
dune exec ./bin/rs.exe -- files encode README.md test/README.md.coded.sim -simulate 

echo "############################################################"
echo "Comparing coded files" 
echo "############################################################"

patdiff test/README.md.coded.iter test/README.md.coded.sim 

echo "############################################################"
echo "Decoding file" 
echo "############################################################"

dune exec ./bin/rs.exe -- files decode test/README.md.coded.iter test/README.md.decoded.iter
dune exec ./bin/rs.exe -- files decode test/README.md.coded.iter test/README.md.decoded.sim -simulate 

echo "############################################################"
echo "Comparing decoded file (iter)" 
echo "############################################################"

patdiff README.md test/README.md.decoded.iter

echo "############################################################"
echo "Comparing decoded file (sim)" 
echo "############################################################"

patdiff README.md test/README.md.decoded.sim
