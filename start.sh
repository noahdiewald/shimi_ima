#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -config dm -boot start_sasl -s reloader -s dictionary_maker
