#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cassert>

using namespace std;

const long MAXVAL = std::numeric_limits<long>::max(); // 9223372036854775807
const int MAXLEN = 18;

struct Test {
  long result;
  vector<long> operands;
};

Test readTest(ifstream &ifs) {
  string line;
  std::getline(ifs, line);
  stringstream ss(line);

  Test ret{-1};
  long val;
  if (ss >> val) {
    ret.result = val;
    char c;
    ss >> c;
    assert(c == ':');
    while (ss >> val) {
      ret.operands.push_back(val);
    }
  }
  return ret;
}

// long is not enough concatenate add up two long numbers which are
// roughly 50 bits. Return MAXVAL then.
long concat(long a, long b) {
  stringstream ss;
  ss << a << b;
  int sizeLog10 = ss.tellp();
  if (MAXLEN <= sizeLog10) {
    return MAXVAL;
  }
  long ret;
  ss >> ret;
  return ret;
}

bool hasSolutionRec(Test& test, int k, long v, bool isP2) {
    if (k == test.operands.size()) {
      return v == test.result;
    } else {
      if (test.result < v) {
        return false;
      }
      return hasSolutionRec(test, k+1, v + test.operands[k], isP2)
        || hasSolutionRec(test, k+1, v * test.operands[k], isP2)
        || (isP2 && hasSolutionRec(test, k+1, concat(v, test.operands[k]), isP2));
    }
}

// P1 = 12839601725877
// P2 = 149956401519484
int main() {
  ifstream fin("input.txt");

  long p1 = 0;
  long p2 = 0;

  Test test = readTest(fin);
  while (0 < test.result) {
    if (hasSolutionRec(test, 1, test.operands[0], false)) {
      p1 += test.result;
    }
    if (hasSolutionRec(test, 1, test.operands[0], true)) {
      p2 += test.result;
    }
    test = readTest(fin);
  }


  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";

  return 0;
}
