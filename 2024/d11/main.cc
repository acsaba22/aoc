#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cassert>
#include <map>
#include <set>

using namespace std;

map<pair<int, long>, long> cache;

long simulateOne(int steps, long val);

long simulateOneImpl(int steps, long val) {
  if (steps == 0) {
    return 1;
  }
  if (val == 0) {
    return simulateOne(steps-1, 1);
  }
  string s = to_string(val);
  if (s.size() % 2 == 0) {
    int k = s.size() / 2;
    return simulateOne(steps-1, stol(s.substr(0, k))) + simulateOne(steps-1, stol(s.substr(k))) ;
  }
  long newVal = val * 2024;
  assert(newVal / 2024 == val); // is long long enough?
  return simulateOne(steps-1, newVal);
}

long simulateOne(int steps, long val) {
  auto it = cache.find({steps, val});
  if (it != cache.end()) {
    return it->second;
  }
  long ret = simulateOneImpl(steps, val);
  cache[{steps, val}] = ret;
  return ret;
}

// P1 = 199753
// P2 = 239413123020116
int main() {
  ifstream fin("input.txt");


  long val;
  long p1 = 0;
  long p2 = 0;
  while (fin >> val) {
    p1 += simulateOne(25, val);
    p2 += simulateOne(75, val);
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";

  return 0;
}
