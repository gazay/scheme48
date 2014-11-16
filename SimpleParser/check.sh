#!/bin/bash

if [ ! -f ./parser ];
then
  ghc -o parser --make Main.hs
  if [ ! -f ./parser ];
  then
    echo "Error occured, check output above"
    exit 0
  fi
fi

if [ ! $# -eq 0 ];
then
  echo "Evaluating types:"
  echo
  echo "Numbers"
fi
./parser "1"
./parser "25"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Floats"
fi
./parser "12.4312341234"
./parser "12.43L1"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Formatted numbers"
fi
./parser "#d1234"
./parser "#xFA12"
./parser "#b1101"
./parser "#o176"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Rational"
fi
./parser "3/12"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Complex"
fi
./parser "4+18i"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Atom"
fi
./parser "%atom"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Boolean"
fi
./parser "#t"
./parser "#f"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- String"
fi
./parser "\"string\""
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Character"
fi
./parser "#\\s"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Simple list"
fi
./parser "(1 2 3)"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Dotted list"
fi
./parser "(1 2 . 4)"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Quoted value"
fi
./parser "(1 '(1 2))"
./parser "'2"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Backquoted value"
fi
./parser "(0 \`(1 2))"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Unquoted value"
fi
./parser "(1 ,(1 2))"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Simple implementation of vector"
fi
./parser "#(1 (1 2 . 3) \"string\" #\\s)"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Primitive functions"
fi
./parser "(+ 3 2)"
./parser "(+ 3 2 1)"
./parser "(* 3 2)"
./parser "(* 3 2 2)"
./parser "(- 3 2)"
./parser "(- 3 2 8)"
./parser "(/ 3 2)"
./parser "(mod 3 2)"
./parser "(quotinent 3 2)"
./parser "(remainder 3 2)"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Checking types"
fi
./parser "(symbol? %)"
./parser "(symbol? 1)"
./parser "(number? 1)"
./parser "(number? %)"
./parser "(string? \"string\")"
./parser "(string? %)"
./parser "(string->symbol \"string\")"
./parser "(symbol->string %symbol)"
./parser "(float? 1.1)"
./parser "(float? %)"
./parser "(list? (1 2 3))"
./parser "(list? (1 2 . 3))"
if [ ! $# -eq 0 ];
then
  echo
  echo "-- Primitives"
fi
./parser "(> 2 3)"
./parser "(< 2 3)"
./parser "(<= 2 3)"
./parser "(string=? \"test\" \"test\")"
./parser "(string=? \"test2\" \"test\")"
./parser "(string<? \"abc\" \"bba\")"
./parser "(car (1 2 3))"
./parser "(car (\"a\" \"b\" \"c\"))"
./parser "(cdr (1 2 3))"
./parser "(cdr (\"a\" \"b\" \"c\"))"
./parser "(eqv? 2 3)"
./parser "(eqv? 2 2)"
./parser "(eqv? 2 \"2\")"
./parser "(equal? 2 \"2\")"


rm ./parser
