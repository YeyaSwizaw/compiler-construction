OUTPUT = libsfl.so

CC = clang
CFLAGS = -fPIC
OBJFLAGS = -c
LIBFLAGS = -shared

OBJ = \
	output.o \
	stack.o \
	ops.o \
	apply.o \
	types.o \
	main.o

HEADERS = \
	types.h \
	stack.h \
	ops.h \
	output.h

all: $(OBJ)
	$(CC) $(CFLAGS) $(LIBFLAGS) -o $(OUTPUT) $(OBJ)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $(OBJFLAGS) -o $@ $<

clean:
	@rm -rf $(OBJ) $(OUTPUT)
