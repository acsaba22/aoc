import * as util from '../util.js'


function p1(g: Record<string, string[]>): number {
  let res = 0
  let path: string[] = []

  function dfs(node: string): void {
    if (node == 'end') {
      res++
      return
    }
    path.push(node)
    for (let neigh of g[node]) {
      if (neigh.toUpperCase() == neigh || !path.includes(neigh)) {
        dfs(neigh)
      }
    }
    path.pop()
  }
  dfs('start')
  return res
}

function p2(g: Record<string, string[]>): number {
  let res = 0
  let path: string[] = []
  let used = false

  function dfs(node: string): void {
    if (node == 'end') {
      // util.log(path)
      res++
      return
    }
    path.push(node)
    for (let neigh of g[node]) {
      if (neigh.toUpperCase() == neigh || !path.includes(neigh)) {
        dfs(neigh)
      } else {
        if (!used && neigh != 'start') {
          used = true
          dfs(neigh)
          used = false
        }
      }
    }
    path.pop()
  }
  dfs('start')
  return res
}


export async function main() {
  let file: string = await util.loadFile('/src/d12/input.txt')
  let lines: string[] = file.trim().split('\n')

  let g: Record<string, string[]> = {}

  for (let l of lines) {
    let [from, to] = l.split('-')
    g[from] = [...g[from] || [], to]
    g[to] = [...g[to] || [], from]
  }

  util.log('P1:', p1(g))  // 4749
  util.log('P2:', p2(g))  // 123054
}
