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

struct Point {
  long x, y;

  Point operator+(const Point& p2) const {
      return Point{x + p2.x, y + p2.y};
  }

  Point operator-(const Point& p2) const {
      return Point{x - p2.x, y - p2.y};
  }

  Point operator*(long n) const {
      return Point{x * n, y * n};
  }

  bool operator<(const Point& p2) const {
      return x < p2.x && y < p2.y;
  }

  bool operator==(const Point& p2) const {
      return x == p2.x && y == p2.y;
  }
};

std::ostream& operator<<(std::ostream& outs, const Point& p) {
    return outs << "(" << p.x << ", " << p.y << ")";
}


// P1 = 31897
// P2 = 87596249540359
int main() {
  ifstream fin("input.txt");
  string line;
  getline(fin, line);

  regex reA(R"(Button A: X\+(\d+), Y\+(\d+))");
  regex reB(R"(Button B: X\+(\d+), Y\+(\d+))");
  regex rePrize(R"(Prize: X=(\d+), Y=(\d+))");

  auto parse = [](const string& s, const regex & re) {
    smatch m;
    assert(regex_match(s, m, re));
    Point ret{stoi(m[1]), stoi(m[2])};
    return ret;
  };

  long p1 = 0;
  long p2 = 0;

  while (line != "") {
    Point a = parse(line, reA);
    getline(fin, line);
    Point b = parse(line, reB);
    getline(fin, line);
    Point prize = parse(line, rePrize);

    { // p1
      long bestCost = 0;
      for (long n = 0; n <= 100 && a*n < prize; n++) {
        Point diff = prize - (a*n);
        long m = diff.x / b.x;
        if (b*m == diff) {
          long cost = n*3 + m;
          if (bestCost <= 0 || cost < bestCost) {
            bestCost = cost;
          }
        }
      }
      p1 += bestCost;
    }

    Point prize2 = prize + Point{10000000000000, 10000000000000};

    { // p2
      assert(a.x * b.y != b.x * a.y);
      long minN = 0;
      long maxN = prize2.x / a.x;

      struct Res {
        long m;
        int direction; // -1, 0 or 1
      };
      auto calcM = [&a, &b, &prize2](long n) {
        Point diff = prize2 - a*n;
        long m = diff.x / b.x;
        if (b*m == diff) {
          return Res{m, 0};
        }
        double md = double(diff.x) / b.x;
        double dy = diff.y - md*b.y;
        return Res{m, dy<0? -1: 1};
      };

      while (1 < maxN - minN) {
        long mid = (minN + maxN) / 2;
        Res resMin = calcM(minN);
        Res resMid = calcM(mid);
        Res resMax = calcM(maxN);
        if (!(resMin.direction != resMax.direction)) {
          break;
        }

        if (resMid.direction == 0) {
          minN = mid;
          maxN = mid;
          break;
        } else if (resMin.direction == resMid.direction) {
          minN = mid;
        } else {
          maxN = mid;
        }
      }
      Res sol = calcM(minN);
      if (sol.direction == 0) {
        p2 += minN*3 + sol.m;
      }
    }

    getline(fin, line);
    getline(fin, line);
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";

  return 0;
}
