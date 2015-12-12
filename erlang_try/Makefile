NAME = WorldOfErlcraft

DEPSDIR = deps
DEPS    = $(wildcard $(DEPSDIR)/*)
DEPSBIN = $(DEPS:%=%/ebin)

ERLANG   = erl
ERLC     = erlc
DIALYZER = dialyzer
BEAMDIR  = ebin

NODENAME = crafter

SYSTHREADS = 10

SRCDIR      = src
ERLFILES    = $(wildcard $(SRCDIR)/*.erl)
TESTSRC     = $(wildcard test/*_tests.erl)
PROPSRC     = $(wildcard properties/*_props.erl)
BEAMFILES   = $(ERLFILES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
TESTBEAMS   = $(TESTSRC:test/%.erl=$(BEAMDIR)/%.beam)
PROPBEAMS   = $(PROPSRC:properties/%.erl=$(BEAMDIR)/%.beam)
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

ERLWARN  = unused_vars export_all shadow_vars unused_import unused_function \
	bif_clash unused_record deprecated_function obselete_guard export_vars \
	exported_vars
ERLCOPTS = $(ERLWARN:%=+warn_%) +'{hipe, [o3]}' +compressed +verbose \
	-Iinclude

APPSPEC     = ebin/crafter.app
SOURCEFILES = $(wildcard src/*.erl)
MODULES     = $(SOURCEFILES:src/%.erl=%)
BEAMFILES   = $(MODULES:%=ebin/%.beam)

all: $(BEAMFILES) $(APPSPEC)

clean: $(PLTFILE:%=clean_plt_%) $(TESTBEAMS:ebin/%.beam=clean_mod_%) $(MODULES:%=clean_mod_%) \
	clean_spec clean_dir_ebin clean_dir_doc
.PHONY: clean

clean_plt_%:
	rm -f $*

clean_mod_%:
	rm -f $(*:%=ebin/%.beam)

clean_spec:
	rm -f $(APPSPEC)

clean_dir_%:
	rm -rfv $*

rebuild: clean all
.PHONY: rebuild

run:
	@$(ERLANG) $(ERLRUNOPTS) -eval '$(APPSTART)' # 2> /dev/null

$(BIN): $(OBJFILES)
	$(ESCRPTCMD)

doc:
	$(ERLANG) -noshell -run edoc_run application "'$(NODENAME)'" \
		'"."' '[{def,{vsn,"0.0.1"}}]'
.PHONY: doc

typecheck: $(PLTFILE) $(OTPPLTFILE)
	$(DIALYZER) --plts $(OTPPLTFILE) $(PLTFILE) -Wrace_conditions --src $(SRCDIR)

$(PLTFILE): all
	$(DIALYZER) --output_plt $@ --build_plt -r ebin $(DEPSBIN:%=-r %)

$(OTPPLTFILE):
	$(DIALYZER) --output_plt $@ --build_plt --apps edoc erts eunit kernel mnesia stdlib tools webtool xmerl

test: eunit props

eunit: $(TESTBEAMS) $(BEAMFILES) $(MODULES:%=run_test_%)

props: $(PROPBEAMS:ebin/%.beam=run_prop_%)

run_prop_%_props: ebin/%_props.beam ebin/%.beam
	@$(ERLANG) -noshell -pa ebin -eval '$*_props:runner().' -s init stop

run_test_%: ebin/%.beam ebin/%_tests.beam
	@$(ERLANG) -noshell -pa ebin -eval 'eunit:test($*, [verbose]).' -s init stop

$(BEAMFILES): ebin/%.beam: src/%.erl | $(BEAMDIR)
	erlc $(ERLCOPTS) -o $(BEAMDIR) $<

$(TESTBEAMS): ebin/%.beam: test/%.erl | $(BEAMDIR)
	erlc $(ERLCOPTS) -o $(BEAMDIR) $<

$(PROPBEAMS): ebin/%.beam: properties/%.erl
	erlc $(ERLCOPTS) -o $(BEAMDIR) $<

$(APPSPEC): src/crafter.app | ebin
	cp $< $@

$(BEAMDIR):
	mkdir $@
