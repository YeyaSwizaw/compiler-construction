SRCDIR = src

OCAML = ocamlbuild -r -I $(SRCDIR) -use-menhir -use-ocamlfind -pkg core -tag thread -menhir "menhir -v"

TARGET = sfl
TESTTARGET = test

all: $(TARGET) $(TESTTARGET)

$(TARGET): clean_target $(TARGET).native

$(TESTTARGET): clean_test $(TESTTARGET).native

%.native: 
	$(OCAML) $@

clean: clean_target clean_test
	rm -rf _build

clean_target:
	rm -rf $(TARGET).native

clean_test:
	rm -rf $(TESTTARGET).native
