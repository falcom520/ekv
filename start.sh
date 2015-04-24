#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname ekv \
    -s ekv \
    -setcookie 387 \
    -detached
