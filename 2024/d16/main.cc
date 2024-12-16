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

  auto operator<=>(const Point&) const = default;


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


// P1 = 85420
// 500 too high
int main() {
  Matrix m;
  m.readFile("input.txt");

  long p1 = 0;
  long p2 = 0;

  Point start, finish;
  for (Idx i = 0; i < NXY; i++) {
    if (m.val(i) == 'S') {
      start = ToPoint(i);
    }
    if (m.val(i) == 'E') {
      finish = ToPoint(i);
    }
  }
  assert(start.isInside());
  assert(finish.isInside());

  struct State {
    Point p;
    int dir; // NESW

    auto operator<=>(const State&) const = default;
  };

  map<State, long> scores;
  set<State> visited;
  set<State> toVisit;
  map<State, vector<State>> backlinks;
  scores[State{start, 1}] = 0;
  toVisit.insert(State{start, 1});

  auto add = [&scores, &toVisit, &backlinks](State state, long score, State backlink) {
      auto it = scores.find(state);
      if (it == scores.end() || score <= it->second) {
        if (score < it->second) {
          backlinks[state].clear();
        }
        scores[state] = score;
        toVisit.insert(state);
        backlinks[state].push_back(backlink);
      }

  };

  bool found = false;
  long lastScore = -1;
  while (!toVisit.empty() && (!found || lastScore <= p1)) {
    State minState;
    long minScore = LONG_MAX;
    for (auto state: toVisit) {
      long score = scores[state];
      if (score < minScore) {
        minScore = score;
        minState = state;
      }
    }
    toVisit.erase(minState);
    visited.insert(minState);

    if (minState.p == finish) {
      p1 = minScore;
      found = true;
    }

    Point neigh = minState.p;
    neigh += Directions4[minState.dir];
    if (neigh.isInside() && m.val(neigh) != '#') {
      add(State{neigh, minState.dir}, minScore + 1, minState);
    }

    add(State{minState.p, (minState.dir + 1)%4}, minScore + 1000, minState);
    add(State{minState.p, (minState.dir + 3)%4}, minScore + 1000, minState);

    lastScore = minScore;
  }

  set<Point> onPath;
  set<State> backToVisit;
  set<State> backVisited;

  backToVisit.insert(State{finish, 0});
  backToVisit.insert(State{finish, 1});
  backToVisit.insert(State{finish, 2});
  backToVisit.insert(State{finish, 3});

  while (!backToVisit.empty()) {
    auto it = backToVisit.begin();
    State state = *it;
    backToVisit.erase(it);
    backVisited.insert(state);
    onPath.insert(state.p);

    for (State backlink: backlinks[state]) {
      if (backVisited.find(backlink) == backVisited.end()) {
        backToVisit.insert(backlink);
      }
    }
  }

  onPath.insert(start);
  p2 = onPath.size();

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}
