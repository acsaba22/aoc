#include <fstream>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <boost/algorithm/string.hpp>

using namespace std;

struct Node {
  std::unordered_map<char, Node> children;
  int finish = 0;
};

// P1: 324
int main() {
  ifstream fin("input.txt");
  string line;
  getline(fin, line);
  vector<string> towels;
  boost::split(towels, string(line), boost::is_any_of(","));

  Node root;
  for (string& s: towels) {
    boost::algorithm::trim(s);
    Node* node = &root;
    for (char c: s) {
      node = &(node->children[c]);
    }
    node->finish++;
  }

  getline(fin, line);
  assert(line == "");

  long p1 = 0;
  long p2 = 0;
  for (getline(fin, line); line != ""; getline(fin, line)) {
    // cout << line << endl;
    unordered_map<Node*, long> nodes{{&root, 1}};
    for (char c: line) {
      unordered_map<Node *, long> newNodes;
      for (auto& [node, count ]: nodes) {
        auto it = node->children.find(c);
        if (it != node->children.end()) {
          Node* newNode = &(it->second);
          newNodes[newNode] += count;
          if (newNode->finish) {
            newNodes[&root] += count;
          }
        }
      }
      nodes.swap(newNodes);
    }
    auto it = nodes.find(&root);
    if (it != nodes.end()) {
      p1++;
      p2 += it->second;
      // cout << nodes.size() << endl;
    }
  }

  cout << "P1: " << p1 << endl;
  cout << "P2: " << p2 << endl;

  return 0;
}