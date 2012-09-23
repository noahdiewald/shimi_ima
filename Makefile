ERL ?= erl
REBAR ?= ./rebar
APP := dictionary_maker

all: build

clean:
	$(REBAR) clean
	rm -rf priv/log/*
	rm -Rf .eunit

depends:
	@if test ! -d ./deps; then \
		$(REBAR) get-deps; \
	else \
		$(REBAR) update-deps; \
	fi

build: depends
	$(REBAR) compile
