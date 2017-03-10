CC				= gcc-4.7
CC_FLAGS	= -O0 -ansi -Wall

.PHONY: clean

debug: CC_FLAGS += -g -DDEBUG
debug: all

all: om lc

om: online_matching.o
	$(CC) $(CC_FLAGS) $< -o $@

lc: link_cut.o
	$(CC) $(CC_FLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CC_FLAGS) $< -o $@

clean:
	$(RM) lc om *.o *.i *.s a.out
