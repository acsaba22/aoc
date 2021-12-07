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
  let s: string = await loadFile('/src/d07/input.txt')
  let v = s.split(',').map(x => Number(x))
  v.sort((a, b) => a - b)
  let median = v[v.length/2-1]
  let p1 = v.reduce((a, x) => a + Math.abs(x-median), 0)
  log('P1: ', p1) // 345035

  let maxVal = v[v.length-1]
  let p2Totals = [...Array(maxVal).keys()].map(
    middle => v.reduce((a, x) => {
      let l = Math.abs(x-middle)
      return a + l*(l+1)/2
    }, 0)
  )
  let p2 = Math.min(...p2Totals)
  log('P2: ', p2) // 97038163
}
