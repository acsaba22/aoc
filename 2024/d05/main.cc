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
#include <set>
#include <map>
#include <ranges>

#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>

using namespace std;

vector<int> parseLine(const string& line, string delims) {
  vector<string> strs;
  boost::split(strs, line, boost::is_any_of(delims));
  vector<int> ret(strs.size());
  transform(strs.begin(), strs.end(), ret.begin(), [](const string& s) {
    return stoi(s);
  });
  return ret;
}

template<typename T1, typename T2>
T2 getWithDefault(map<T1, T2>& m, const T1& k) {
  auto it = m.find(k);
  if (it != m.end()) {
    return it->second;
  }
  return T2();
}

bool hasIntersection(const set<int>& a, const set<int>& b) {
  set<int> intersection;
  boost::range::set_intersection(
    a, b, inserter(intersection, intersection.begin()));

  return intersection.size() != 0;
}

// P1 = 5129
// P2 = 4077
int main() {
  ifstream fin("input.txt");
  string line;
  getline(fin, line);

  map<int, set<int>> g;
  map<int, set<int>> gInverse;
  while (line != "") {
    vector<int> orderRule = parseLine(line, "|");
    assert(orderRule.size() == 2);
    g[orderRule[0]].insert(orderRule[1]);
    gInverse[orderRule[1]].insert(orderRule[0]);
    getline(fin, line);
  }

  int p1 = 0;
  int p2 = 0;

  getline(fin, line);
  while (line != "") {
    vector<int> updates = parseLine(line, ",");

    bool ok = true;
    set<int> seen;

    // P1
    for (int page: updates) {
      set<int> shouldBeAfter = getWithDefault(g, page);
      if (hasIntersection(seen, shouldBeAfter)) {
        ok = false;
      }
      seen.insert(page);
    }

    if (ok) {
      p1 += updates[updates.size()/2];
    } else {
      // P2
      set<int> postfix = seen;
      vector<int> corrected, heldBack;
      int readIdx = 0;

      while (corrected.size() != updates.size()) {
        bool written = false;
        for (int i = 0; !written && i < heldBack.size(); i++) {
          int page = heldBack[i];
          if (!hasIntersection(getWithDefault(gInverse, page), postfix)) {
            corrected.push_back(page);
            heldBack.erase(heldBack.begin()+i);
            postfix.erase(page);
            written = true;
          }
        }
        if (!written) {
          assert(readIdx < updates.size());
          int page = updates[readIdx];
          readIdx++;
          heldBack.push_back(page);
        }
      }
      p2 += corrected[corrected.size()/2];
    }

    getline(fin, line);
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";

  return 0;
}