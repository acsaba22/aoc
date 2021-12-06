declare function log(...args: any[]): void;
declare function loadFile(fname: string): Promise<string>

function p12(s:string, rounds:number): number {
  let buckets = Array(9).fill(0)
  s.split(',').forEach(x => buckets[Number(x)]++)
  for (let day = 0; day < rounds; day++) {
    let hatch = buckets.shift()
    buckets[6]+=hatch
    buckets.push(hatch)
  }
  return buckets.reduce((a,b) => a+b)
}

export async function main() {
  let s: string = await loadFile('/src/d06/input.txt')
  // P1:  352151
  // P2:  1601616884019
  log('P1: ', p12(s, 80))
  log('P2: ', p12(s,256))
}
