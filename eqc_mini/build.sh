#!/bin/bash
BUILD_DIR="./build"
BINSON_C_LIGHT="../../binson-c-light"
EQC_INCLUDE="/usr/include/"

if [ ! -d "$BUILD_DIR" ]; then
    mkdir $BUILD_DIR
fi

gcc -Os -std=c99 -fPIC -shared -o $BUILD_DIR/binson_nif.so binson_nif.c $BINSON_C_LIGHT/src/binson_parser.c $BINSON_C_LIGHT/src/binson_writer.c -I$EQC_INCLUDE -I$BINSON_C_LIGHT/include
erlc -o $BUILD_DIR binson_nif.erl ../binson.erl binson_eqc.erl
