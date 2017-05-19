CC				= gcc
CC_FLAGS	= -O3 -ansi -Wall -Wfatal-errors

.PHONY: clean

# debug: CC_FLAGS += -g -DDEBUG
# debug: all

all: lc

lc: link_cut.o
	$(CC) $(CC_FLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CC_FLAGS) $< -o $@

clean:
	$(RM) lc om *.o *.i *.s a.out
