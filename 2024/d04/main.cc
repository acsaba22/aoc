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

struct Matrix {
  int nx = 0;
  vector<string> m;

  void readFile(const string& fname) {
    ifstream fin(fname);
    string line;
    getline(fin, line);
    while (line!="") {
      if (nx == 0) {
        nx = line.size();
      }
      assert(nx == line.size());
      m.push_back(line);

      getline(fin, line);
    }
  }

  char v(int y, int x) {
    if (0 <= y && y < m.size() && 0 <= x && x <= nx) {
      return m[y][x];
    }
    return '.';
  }
};

// P1 = 2504
// p2 546 too low
int main() {
  Matrix m;
  m.readFile("input.txt");
  string searchStr = "XMAS";

  vector<pair<int, int>> dirs = {
    {0, 1},
    {1, 1},
    {1, 0},
    {1, -1},
    {0, -1},
    {-1, -1},
    {-1, 0},
    {-1, 1},
  };

  int p1 = 0;
  int p2 = 0;
  for (int y = 0; y < m.m.size(); y++) {
    for (int x = 0; x < m.nx; x++) {
      // p1
      for (auto d: dirs) {
        bool ok = true;
        int yy = y;
        int xx = x;
        for (char ch : searchStr) {
          if (m.v(yy, xx) != ch) {
            ok = false;
            break;
          }
          yy += d.first;
          xx += d.second;
        }
        if (ok) {
          p1++;
        }
      }

      //p2
      char middle = m.v(y, x);
      if (middle == 'A') {
        auto check = [&y,&x,&m](int dy, int dx) {
          char c0 = m.v(y-dy, x-dx);
          char c1 = m.v(y+dy, x+dx);
          string s{c0, c1};
          return s== "MS" || s == "SM";
        };
        if (check(1, 1) && check(1, -1)) {
          p2++;
        }
      }
    }
  }
  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}