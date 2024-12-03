#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cassert>
#include <regex>

using namespace std;

// P1 = 188741603
// P2 = 67269798
int main() {
  ifstream fin("input.txt");
  stringstream buffer;
  buffer << fin.rdbuf();
  string line = buffer.str();;

  regex r(R"((mul\((\d+),(\d+)\))|(do\(\))|(don't\(\)))");

  vector<smatch> matches{
    sregex_iterator{line.begin(), line.end(), r},
    sregex_iterator{}
    };

  int p1 = 0;
  int p2 = 0;

  bool doOk = true;
  for (smatch& m: matches) {
    // cout << m[0] << " " << m[2] << " " << m[3] << "\n";
    if (m[0] == "do()") {
      doOk = true;
    } else if (m[0] == "don't()") {
      doOk = false;
    } else {
      int mul = stoi(m[2]) * stoi(m[3]);
      p1 += mul;
      if (doOk) {
        p2 += mul;
      }
    }
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}