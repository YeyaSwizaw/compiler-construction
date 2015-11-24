DIRS = \
	src \
	test

LIBS = \
	core \
	ANSITerminal \
	llvm

MENHIR = menhir --unused-token ERROR
OCAML = ocamlbuild -r $(addprefix -I , $(DIRS)) -X $(SFLLIB) -use-menhir -use-ocamlfind $(addprefix -pkg , $(LIBS)) -tag thread -menhir "$(MENHIR)"

TARGET = sfl
TESTTARGET = test

all: $(TARGET) $(TESTTARGET)

$(TARGET): clean_target $(TARGET).native

$(TESTTARGET): clean_test $(TESTTARGET).native
	@echo Running tests:
	@./$(TESTTARGET).native
	@echo

%.native: 
	@echo Building $@:
	@$(OCAML) $@
	@echo

clean: clean_target clean_test
	@echo Removing build directory
	@rm -rf _build

clean_target:
	@echo Removing target executable
	@rm -rf $(TARGET).native

clean_test:
	@echo Removing test executable
	@rm -rf $(TESTTARGET).native
