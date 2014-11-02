#!/bin/bash

if [ ! -f ./parser ];
then
  ghc -o parser --make Main.hs
  if [ ! -f ./parser ];
  then
    echo "Error occured, check output above"
    exit 0
  else
    continue
  fi
else
  continue
fi

./parser 1
./parser 25
./parser "12.4312341234"
./parser "12.43L1"
./parser "#d1234"
./parser "#xFA12"
./parser "#b1101"
./parser "#o176"
./parser "3/12"
./parser "4+18i"
./parser "%atom"
./parser "#t"
./parser "\"string\""
./parser "#\\s"
./parser "(1 2 3)"
./parser "(1 2 . 4)"
./parser "'12"
./parser "\`12"
./parser ",12"
./parser "#(1 (1 2 . 3) \"string\" #\\s)"

rm ./parser
