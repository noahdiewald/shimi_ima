ERL ?= erl
REBAR ?= ./rebar
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
