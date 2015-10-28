DIRS = \
	src \
	test

SFLLIB = libsfl

LIBS = \
	core \
	llvm \
	ANSITerminal

MENHIR = menhir --unused-token ERROR
OCAML = ocamlbuild -r $(addprefix -I , $(DIRS)) -X $(SFLLIB) -use-menhir -use-ocamlfind $(addprefix -pkg , $(LIBS)) -tag thread -menhir "$(MENHIR)"

TARGET = sfl
TESTTARGET = test

.PHONY: $(SFLLIB)

all: $(TARGET) $(TESTTARGET) $(SFLLIB)

$(TARGET): clean_target $(TARGET).native

$(TESTTARGET): clean_test $(TESTTARGET).native
	@echo Running tests:
	@./$(TESTTARGET).native
	@echo

$(SFLLIB):
	@echo Building runtime library
	@make -C $(SFLLIB)

%.native: 
	@echo Building $@:
	@$(OCAML) $@
	@echo

clean: clean_target clean_test clean_libsfl
	@echo Removing build directory
	@rm -rf _build

clean_target:
	@echo Removing target executable
	@rm -rf $(TARGET).native

clean_test:
	@echo Removing test executable
	@rm -rf $(TESTTARGET).native

clean_libsfl:
	@echo Cleaning runtime library
	@make -C libsfl clean
