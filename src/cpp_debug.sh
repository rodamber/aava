gcc-4.7 -DDEBUG -Wall -g -O0 -ansi online_matching.c -save-temps
grep ^[^\#].*$ online_matching.i > online_matchingi.c
gcc-4.7 -DDEBUG -Wall -g -O0 -ansi online_matchingi.c


