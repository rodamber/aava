gcc-4.7 -Wall -g -ansi online_matching.c -save-temps
grep ^[^\#].*$ online_matching.i > online_matchingi.c
gcc-4.7 -Wall -g -ansi online_matchingi.c


