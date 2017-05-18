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
  ss << "data/txt-" << i << "-pat-" << j << ".in";
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

void general_tests() {
  for (int i = 1; i <= 27; i++) {
    for (int j = 1; j <= min(i, 10); j++) {
      int txt_size = pow(2,i);
      int pat_size = pow(2,j);

      cout << "> i = " << i << ", j = " << j << endl;

      ofstream out(filename(i,j), ofstream::out);

      out << "T "; write_rand_string(out, txt_size);
      out << "N "; write_rand_string(out, pat_size);
      out << "X\n";

      out.close();
    }
  }
}

string naive_bad_filename(int size) {
  stringstream ss;
  ss << "data/naive-bad-txt-" << size << ".in";
  return ss.str();
}

void write_bad_naive_string(ostream &out, int size) {
  for (int k = 1; k < size; k++) {
    out << "A";
  }
  out << "T\n";
}

void naive_bad_perf() {
  for (int i = 8; i <= 27; i++) {
    int txt_size = pow(2,i);
    int pat_size = pow(2,7);

    ofstream out(naive_bad_filename(i), ofstream::out);

    out << "T "; write_bad_naive_string(out, txt_size);
    out << "N "; write_bad_naive_string(out, pat_size);
    out << "X\n";

    out.close();
  }
}

int main() {
  srand(time(NULL));

  naive_bad_perf();

  return 0;
}
