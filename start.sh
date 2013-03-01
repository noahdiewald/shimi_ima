#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -config shimi_ima -boot start_sasl -s reloader -s shimi_ima
