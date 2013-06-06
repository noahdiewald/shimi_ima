ERL ?= erl
REBAR ?= ./rebar
CUC ?= $(HOME)/.gem/ruby/2.0.0/bin/cucumber
REDIS ?= /usr/bin/redis-server
RCLIENT ?= /usr/bin/redis-cli
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

cucumber:
	$(REDIS) > /dev/null &
	/bin/bash -c 'pushd ./Cukes;$(CUC);popd'
	echo "shutdown" | $(RCLIENT)
