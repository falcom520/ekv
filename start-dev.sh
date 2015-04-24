#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname ekv_dev \
    -s ekv \
    -setcookie 387 \
    -s reloader
