SRCDIR = src

OCAML = ocamlbuild -r -I $(SRCDIR) -use-menhir -use-ocamlfind -pkg core -tag thread -menhir "menhir -v"

TARGET = sfl.native

all: target

target: $(TARGET)

$(TARGET):	
	$(OCAML) $@

clean:
	rm -rf _build
	rm -rf $(TARGET)
