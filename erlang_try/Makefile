NAME = WorldOfErlcraft

REBAR = ./rebar
DEPSLVCMD  = $(REBAR) get-deps
COMPILECMD = $(REBAR) compile
CLEANCMD   = $(REBAR) clean
DOCCMD     = $(REBAR) doc
ESCRPTCMD  = $(REBAR) escriptize
TESTCMD    = $(REBAR) eunit
REPLCMD    = $(REBAR) shell

DEPSDIR = deps
DEPS    = $(wildcard $(DEPSDIR)/*)
DEPSBIN = $(DEPS:%=%/ebin)

ERLANG  = erl
DIALYZER = dialyzer
BEAMDIR = ebin

NODENAME = crafter

SYSTHREADS = 10

SRCDIR      = src
ERLFILES    = $(wildcard $(SRCDIR)/*.erl)
BEAMFILES   = $(ERLFILES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
APPSRCFILES = $(wildcard $(SRCDIR)/*.app.src)
APPFILES    = $(APPSRCFILES:$(SRCDIR)/%.src=$(BEAMDIR)/%)

OBJFILES    = $(APPFILES) $(BEAMFILES)
BIN         = $(NODENAME)

CLASSPATHS  = $(BEAMDIR) $(DEPSBIN)

APPSTART    = application:ensure_all_started(crafter).

ERLRUNOPTS  = -config $(NODENAME) +A $(SYSTHREADS) $(CLASSPATHS:%=-pa %) -noshell

OTPVERSION  = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTPPLTFILE  = .global_plt.$(OTPVERSION)
PLTFILE     = $(NODENAME).plt

all: $(REBAR)
	@$(DEPSLVCMD) # > /dev/null 2> /dev/null
	@$(COMPILECMD) # > /dev/null 2> /dev/null
.PHONY: all

run:
	$(ERLANG) $(ERLRUNOPTS) -eval '$(APPSTART)' # 2> /dev/null

$(BIN): $(OBJFILES)
	$(ESCRPTCMD)

doc:
	@$(DOCCMD)
.PHONY: doc

typecheck: $(PLTFILE) $(OTPPLTFILE)
	$(DIALYZER) --plts $(OTPPLTFILE) $(PLTFILE) -Wrace_conditions --src $(SRCDIR)

$(PLTFILE): all
	$(DIALYZER) --output_plt $@ --build_plt -r ebin $(DEPSBIN:%=-r %)

$(OTPPLTFILE):
	$(DIALYZER) --output_plt $@ --build_plt --apps edoc erts eunit kernel mnesia stdlib tools webtool xmerl
