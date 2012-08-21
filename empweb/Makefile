# Feel free to use, reuse and abuse the code in this file.

all: app ria

app: get-deps
	@./rebar compile

ria: ria-build ria-source

ria-build:
	cd ./priv/static/ria/ && python generate.py build || exit 1;

ria-source:
	cd ./priv/static/ria/ && python generate.py source || exit 1;

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

dist-clean: clean

