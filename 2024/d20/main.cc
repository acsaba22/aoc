#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>
#include <cassert>
#include <regex>
#include <set>
#include <map>
#include <ranges>
#include <numeric>


#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

using namespace std;


typedef int Idx;
Idx NY = 0, NX = 0, NXY = 0;

struct Point {
  int y = -1;
  int x = -1;

  // auto operator<=>(const Point&) const = default;


  Point operator+(const Point& p2) const {
      return Point{y + p2.y, x + p2.x};
  }

  Point operator-(const Point& p2) const {
      return Point{y - p2.y, x - p2.x};
  }

  bool operator!=(const Point& p2) const {
      return x != p2.x || y != p2.y;
  }

  Point& operator+=(const Point& p2) {
    y += p2.y;
    x += p2.x;;
    return *this;
  }

  Point& operator-=(const Point& p2) {
    y -= p2.y;
    x -= p2.x;;
    return *this;
  }

  Idx toIdx() {
    return y*NX + x;
  }

  bool isInside() {
    return 0 <= this->y && this->y < NY && 0 <= this->x && this->x < NX;
  }

  vector<Point> allNeighbours4();

  vector<Point> neighbours4() {
    vector<Point> ret;
    for (Point b: allNeighbours4()) {
      if (b.isInside()) {
        ret.push_back(b);
      }
    }
    return ret;
  }
};
Point ToPoint(int idx) {
  return Point{idx / NX, idx % NX};
}
vector<Idx> Neighbours4(Idx idx) {
  vector<Idx> ret;
  for (Point p: ToPoint(idx).neighbours4()) {
    ret.push_back(p.toIdx());
  }
  return ret;
}

std::ostream& operator<<(std::ostream& outs, const Point& p) {
    return outs << "(" << p.y << ", " << p.x << ")";
}

Point Directions4[]{{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
vector<Point> Point::allNeighbours4() {
  vector<Point> ret;
  for (Point d: Directions4) {
    Point b = *this;
    b += d;
    ret.push_back(b);
  }
  return ret;
}


struct Matrix {
  int nx = 0, ny = 0;
  vector<string> m;

  void init(int ny, int nx, char c) {
    this->ny = ny;
    this->nx = nx;
    string s(nx, c);
    for (int i = 0; i < ny; i++) {
      m.push_back(s);
    }
  }
  void readFile(const string& fname) {
    ifstream fin(fname);
    readFile(fin);
  }

  void readFile(ifstream& fin) {
    string line;
    getline(fin, line);
    while (line!="") {
      if (nx == 0) {
        nx = line.size();
      }
      assert(nx == line.size());
      m.push_back(line);
      ny++;

      getline(fin, line);
    }
    NY = this->ny;
    NX = this->nx;
    NXY = NY * NX;
  }

  char val(Point coor) {
    if (coor.isInside()) {
      return m[coor.y][coor.x];
    }
    return '?';
  }

  char val(Idx idx) {
    assert(0 <= idx && idx < NXY);
    return m[idx / NX][idx % NX];
  }

  void set(Point coor, char c) {
    assert(coor.isInside());
    m[coor.y][coor.x] = c;
  }

  Point doFind(char c) {
    for (int y: boost::irange(0, ny)) {
      for (int x: boost::irange(0, nx)) {
        if (m[y][x] == c) {
          return Point{y, x};
        }
      }
    }
    assert(false); // doFind didn't Find
    // return Point{-1, -1};
  }
};

std::ostream& operator<<(std::ostream& outs, const Matrix& m) {
  for (auto line: m.m) {
    outs << line << "\n";
  }
  return outs;
}


///////// STARTS HERE

typedef unordered_map<Idx, int> distanceMap;
int D(distanceMap& distances, Idx i) {
  auto dit = distances.find(i);
  if (dit != distances.end()) {
    return dit->second;
  }
  return INT_MAX;
};


// P1: 1417
// P2: 1014683
int main() {
  Matrix m;

  // m.readFile("example.txt");
  // int betteringLimit = 10;

  m.readFile("input.txt");
  int betteringLimit = 100;

  Idx startIdx = -1, endIdx = -1;
  for (Idx i = 0; i < NXY; i++) {
    if (m.val(i) == 'S') {
      startIdx = i;
    }
    if (m.val(i) == 'E') {
      endIdx = i;
    }
  }

  distanceMap distFromStart;
  distanceMap distToEnd;

  typedef pair<int, Idx> distIdx ;
  auto doBfs = [&m](distanceMap& distances, Idx start) {
    set<distIdx> bfs;
    distances[start] = 0;
    bfs.insert({0, start});
    while (!bfs.empty()) {
      auto it = bfs.begin();
      int dist = it->first;
      int dist1 = dist+1;
      Idx idx = it->second;
      // cout << ToPoint(idx) << " " << dist1 << endl;
      bfs.erase(it);
      for (Idx neigh: Neighbours4(idx)) {
        if (m.val(neigh) != '#') {
          int neighD = D(distances, neigh);
          if (dist1 < neighD) {
            distances[neigh] = dist1;
            bfs.erase({neighD, neigh});
            bfs.insert({dist1, neigh});
          }
        }
      }
    }
  };

  doBfs(distFromStart, startIdx);
  doBfs(distToEnd, endIdx);
  int bestPathDist = D(distFromStart, endIdx);

  // cout << ToPoint(endIdx) << ": " << bestPathDist << endl;
  // cout << ToPoint(startIdx) << ": " << D(distToEnd, startIdx) << endl;

  int p1 = 0;

  auto searchCheats = [&m, &distFromStart, &distToEnd, &bestPathDist](
      int maxCheatDist, int betteringLimit) {
    int ret = 0;
    for (Idx cheatStart = 0; cheatStart < NXY; cheatStart++) {
      if (m.val(cheatStart) == '#') {
        continue;
      }
      for (Idx cheatEnd = max(0, cheatStart - maxCheatDist*NX-1);
          cheatEnd < min(NXY, cheatStart + maxCheatDist*NX+1);
          cheatEnd++) {
        if (m.val(cheatEnd) == '#') {
          continue;
        }
        Point diff = ToPoint(cheatEnd) - ToPoint(cheatStart);
        int len = abs(diff.x) + abs(diff.y);
        if (len <= maxCheatDist) {

          int newBest = D(distFromStart, cheatStart) + len + D(distToEnd, cheatEnd);
          int bettering = bestPathDist - newBest;

          if (betteringLimit <= bettering) {
            // cout << "sol: " << bettering << " | " << ToPoint(cheatStart) << endl;
            ret++;
          }
        }
      }
    }
    return ret;
  };

  cout << "P1: " << searchCheats(2, betteringLimit) << endl;
  cout << "P2: " << searchCheats(20, betteringLimit) << endl;

  return 0;
}
