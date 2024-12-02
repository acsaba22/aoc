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

bool readvec(ifstream &ifs, vector<int> &vec) {
  string line;
  std::getline(ifs, line);
  stringstream ss(line);

  bool ret = false;
  int val;
  while (ss >> val) {
    ret = true;
    vec.push_back(val);
  }
  return ret;
}

// return 0 or 1 for easy summing
int safe(vector<int> &vec) {
    if (!(2 <= vec.size() && vec[0] != vec[1])) {
      assert(2 <= vec.size());
    }
    int direction = (vec[0] < vec[1] ?1 : -1);

    int ret = 1;
    for (int i = 1; i < vec.size(); i++) {
      int diff =  (vec[i] - vec[i-1]) * direction;
      if (diff < 1 || 3 < diff) {
        ret = 0;
      }
    }
  return ret;
}

// P1 = 321
// P2 = 328
int main() {
  ifstream fin("input.txt");

  int p1 = 0;
  int p2 = 0;

  vector<int> vec;
  while (readvec(fin, vec)) {
    int ok = safe(vec);
    p1 += ok;

    if (!ok) {
      for (int i = 0; !ok && i != vec.size(); i++) {
        vector<int> vec2(vec);
        vec2.erase(vec2.begin()+i);
        ok = safe(vec2);
      }
    }
    p2 += ok;

    vec.resize(0);
  }
  cout << p1 << "\n";
  cout << p2 << "\n";
  return 0;
}
