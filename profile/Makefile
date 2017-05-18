CC				= g++
CC_FLAGS	= -O3

.PHONY: clean

all: gen

%.o: %.c
	$(CC) -c $(CC_FLAGS) $< -o $@

gen: random_strings.o
	$(CC) $(CC_FLAGS) $< -o $@

clean:
	$(RM) gen *.o *.i *.s a.out
