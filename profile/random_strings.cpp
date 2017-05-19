#include <cmath>
#include <cstdlib>
#include <ctime>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

string filename(int i, int j) {
  stringstream ss;
  ss << "data/txt-" << (i < 10 ? "0" : "") << i
     <<     "-pat-" << (j < 10 ? "0" : "") << j << ".in";
  return ss.str();
}

char rand_char() {
  static char chars[] = {'A','T','C','G'};
  return chars[random() % 4];
}

void write_rand_string(ostream &out, int size) {
  for (int k = 0; k < size; k++) {
    out << rand_char();
  }
  out << "\n";
}

string naive_bad_filename(int size) {
  stringstream ss;
  ss << "data/naive-bad-txt-" << (size < 10 ? "0" : "") << size << ".in";
  return ss.str();
}

void write_bad_naive_string(ostream &out, int size) {
  for (int k = 1; k < size; k++) {
    out << "A";
  }
  out << "T\n";
}

void naive_bad_perf() {
  for (int i = 2; i <= 27; i++) {
    int txt_size = pow(2,i);
    int pat_size = pow(2,i-2); // The pattern length is 1/4 of the text size

    ofstream out(naive_bad_filename(i), ofstream::out);

    out << "T "; write_bad_naive_string(out, txt_size);
    out << "N "; write_bad_naive_string(out, pat_size);
    out << "X\n";

    out.close();
  }
}

void write_bm_sublinear(ostream &out, int n, int m) {
  ostringstream pat;

  out << "T ";
  for (int k = 1; k <= n - m; k++) {
    if (k % m == 0) {
      out << 'T';
    } else {
      out << 'G';
    }
  }

  for (int k = 1; k <= m; k++) {
      pat << 'C';
  }
  pat << 'A';

  out << pat.str();
  out << "\nB " << pat.str() << "\nX";
}

int main() {
  int _1_KiB = 1024;
  int _10_KiB = 10 * _1_KiB;
  int _1_MiB = 1048576;

  for (int n = _10_KiB; n <= _1_MiB; n += _10_KiB) {
    for (int m = 16; m <= 256; m *= 4) {

      cout << "> n = " << n << ", m = " << m << endl;

      ofstream out(filename(n,m), ofstream::out);

      // out << "T "; write_bad_naive_string(out, n); //write_rand_string(out, n);
      // out << "N "; write_bad_naive_string(out, m); //write_rand_string(out, m);
      // out << "X\n";

      write_bm_sublinear(out, n, m);

      out.close();
    }
  }

  return 0;
}

