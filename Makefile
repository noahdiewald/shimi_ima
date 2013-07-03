ERL ?= erl
REBAR ?= ./rebar
CURL ?= /usr/bin/curl
CUC ?= $(HOME)/.gem/ruby/2.0.0/bin/cucumber
REDIS ?= /usr/bin/redis-server
RCLIENT ?= /usr/bin/redis-cli
GRUNT ?= /usr/bin/grunt
URL ?= http://tester:tester@127.0.0.1:5984
APP_URL ?= $(URL)/shimi_ima
ALL_URL ?= $(APP_URL)/_all_docs?include_docs=true
APP := dictionary_maker

all: build

clean:
	$(REBAR) clean
	rm -rf priv/log/*
	rm -Rf .eunit

distclean: clean
	rm -Rf deps/

depends:
	@if test ! -d ./deps; then \
		$(REBAR) get-deps; \
	else \
		$(REBAR) update-deps; \
	fi

build: depends
	$(REBAR) compile

eunit:
	$(REBAR) eunit skip_deps=true

mocha:
	$(GRUNT) test

mochacov:
	$(GRUNT) coverage

runcuc:
	$(REDIS) > /dev/null &
	/bin/bash -c 'pushd ./Cukes;$(CUC);popd'
	echo "shutdown" | $(RCLIENT)

cleancuc:
	@for i in `$(CURL) -s $(ALL_URL)|awk -F\" '/__test/ {print $$4}'`; \
	do \
		revision=`$(CURL) -s $(APP_URL)/$$i|awk -F\" '{print $$8}'`; \
		$(CURL) -X DELETE $(APP_URL)/$$i?rev=$$revision; \
		$(CURL) -X DELETE $(URL)/project-$$i; \
	done

cucumber: runcuc cleancuc

fasttest: eunit mocha

slowtest: fasttest cucumber
