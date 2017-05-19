CC				= g++
CC_FLAGS	= -O3

.PHONY: clean

all: om_gen lc_gen

%.o: %.c
	$(CC) -c $(CC_FLAGS) $< -o $@

om_gen: random_strings.o
	$(CC) $(CC_FLAGS) $< -o $@

lc_gen: random_link_cut.o
	$(CC) $(CC_FLAGS) $< -o $@

clean:
	$(RM) lc-gen om_gen *.o *.i *.s a.out
