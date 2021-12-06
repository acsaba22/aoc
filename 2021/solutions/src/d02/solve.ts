import { type } from "os"

declare function log(...args: any[]) : void;
declare function loadFile(fname: string): Promise<string>

function p1(lines: string[]) {
  let x = 0
  let y = 0
  const move : Record<string, (n: number) => number>  = {
    'forward': (n : number) => x += n,
    'down': (n : number) => y += n,
    'up': (n : number) => y -= n,
  }
  for (const l of lines) {
    const [d, n] = l.split(' ');
    move[d](Number(n))
  }
  log('P1: ', x * y) // 1882980
}

function p2(lines: string[]) {
  let x = 0
  let y = 0
  let aim = 0
  let move = new Map([
    ['forward', (n : number) => {
      x += n
      y += n*aim
    }],
    ['down', (n : number) => aim += n],
    ['up', (n : number) => aim -= n],
  ])
  for (let l of lines) {
    let [d, n] = l.split(' ')
    move.get(d)!(Number(n))
  }
  log('P2: ', x * y) // 1882980
}

export async function main() {
  let s: string = await loadFile('/src/d02/input.txt')
  let lines = s.trim().split('\n')
  p1(lines)
  p2(lines)
}
