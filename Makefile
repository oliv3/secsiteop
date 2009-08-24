all: erlang

erlang: loadavg.beam poller.beam gui.beam cube.beam cmap.beam u.beam \
	load.beam

%.beam: %.erl ss.hrl
	@erlc $<

COOKIE = 'CopSeesIt'
NODE ?= node1

client: all
	erl -setcookie $(COOKIE) -sname $(NODE) -s loadavg -s init stop

poller: all
	erl -setcookie $(COOKIE) -sname poller -s poller -s init stop

run: all
	erl -setcookie $(COOKIE) -sname gui -s gui -s cube -s load

clean:
	@rm -f *.beam erl_crash.dump *~
