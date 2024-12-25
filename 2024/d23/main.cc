#include <fstream>
#include <iostream>
#include <cassert>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

using namespace std;

void reportProgress(const string& msg, int total, int iteration, int freq = 1000) {
  if ((iteration+1) % freq == 0) {
    cout << msg << " " << iteration << " / " << total << " | "
      << int(double(iteration)/total*1000) << " %o" << endl;
  }
}

// P1: 1358
// P2: cl,ei,fd,hc,ib,kq,kv,ky,rv,vf,wk,yx,zf
int main() {
  ifstream fin("input.txt");

  string line;
  map<string, set<string>> graph;

  auto isConnected = [&graph](const string& c1, const string& c2) {
    auto it = graph.find(c1);
    if (it != graph.end()) {
      return it->second.find(c2) != it->second.end();
    }
    return false;
  };

  long part1 = 0;
  while (fin >> line) {
    assert(line[2] == '-');

    string c1 = line.substr(0, 2);
    string c2 = line.substr(3, 2);

    // cout << c1 << " " << c2 << endl;
    graph[c1].insert(c2);
    graph[c2].insert(c1);
  }

  map<string, set<string>> compontents;
  auto toName = [](const set<string> s) {
    string ret;
    for (auto comp: s) {
      ret += comp + ",";
    }
    return ret;
  };

  int count = 0;
  int step1 = 0;
  for (auto p1 = graph.begin(); p1 != graph.end(); p1++) {
    reportProgress("Expand ", graph.size(), step1++);

    string p1n = p1->first;
    auto p2 = p1;
    for (p2++; p2 != graph.end(); p2++) {
      string p2n = p2->first;
      auto p3 = p2;
      for (p3++; p3 != graph.end(); p3++) {
        string p3n = p3->first;
        if (isConnected(p1n, p2n) && isConnected(p1n, p3n) && isConnected(p2n, p3n)) {
          // cout << p1n << "," << p2n << "," << p3n << endl;
          set<string> s{p1n, p2n, p3n};
          compontents[toName(s)] = s;
          if (p1n[0] == 't' || p2n[0] == 't' || p3n[0] == 't') {
            part1++;
          }
        }
      }
    }
  }

  int componentSize = 3;
  for (bool stillHas = true; stillHas;) {
    cout << "Number of components with " << componentSize << ": " << compontents.size() << endl;

    map<string, set<string>> biggerCompontents;

    int step = 0;
    for (auto [name, members]: compontents) {
      reportProgress("Expand ", compontents.size(), step++);
      for (auto [newMember, neighbours]: graph) {
        bool ok = 0 == members.count(newMember);
        for (auto origMember: members) {
          if (neighbours.count(origMember) == 0) {
            ok = false;
          }
        }
        if (ok) {
          set<string> newComponent = members;
          newComponent.insert(newMember);
          biggerCompontents[toName(newComponent)] = newComponent;
        }
      }
    }

    if (biggerCompontents.size()) {
      compontents.swap(biggerCompontents);
      componentSize++;
    } else {
      stillHas = false;
    }
  }

  cout << "P1: " << part1 << endl;

  string password = compontents.begin()->first;
  password = password.substr(0, password.size()-1);
  cout << "P2: " << password << endl;
  return 0;
}
