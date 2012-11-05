#REBAR= which rebar || ./rebar

REBAR=./rebar

all: app
app: get-deps
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
ria: ria-build ria-source
ria-build:
	cd ./priv/static/ria/ && python generate.py build || exit 1;
ria-source:
	cd ./priv/static/ria/ && python generate.py source || exit 1;
get-deps:
	@$(REBAR) get-deps
clean:
	@$(REBAR) clean
	rm -f erl_crash.dump
dist-clean: clean

