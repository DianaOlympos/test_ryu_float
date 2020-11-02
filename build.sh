#!/bin/env bash
export KERL_CONFIGURE_OPTIONS="--disable-hipe --without-javac --without-ssl --without-odbc"
kerl_deactivate
kerl build git https://github.com/DianaOlympos/otp master ryu_float
kerl install ryu_float ~/kerl/ryu_float
. /home/DianaO/kerl/ryu_float/activate
kerl status