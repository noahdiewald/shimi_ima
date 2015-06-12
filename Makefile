ERL ?= erl
REBAR ?= /usr/bin/rebar
CURL ?= /usr/bin/curl
CUCARG ?= ''
CUC ?= /usr/bin/bundle exec cucumber $(CUCARG)
GRUNT ?= /usr/bin/grunt
NPM ?= /usr/bin/npm
URL ?= http://tester:tester@127.0.0.1:5984
APP_URL ?= $(URL)/shimi_ima
ALL_URL ?= $(APP_URL)/_all_docs?include_docs=true
APP := dictionary_maker

all: build jbuild

cleantemplates:
	rm -rf ebin/*_dtl.beam

templates: cleantemplates build

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

jdepends:
	$(NPM) install

build: depends
	$(REBAR) compile

jbuild: jdepends
	$(GRUNT)

eunit:
	$(REBAR) eunit skip_deps=true

mocha:
	$(GRUNT) test

mochacov:
	$(GRUNT) coverage

runcuc:
	/bin/bash -c 'pushd ./Cukes;$(CUC);popd'

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
