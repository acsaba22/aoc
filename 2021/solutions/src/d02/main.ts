import { main } from "./solve.js"
import * as fs from 'fs'

const gThis: any = globalThis

gThis.log = function (...args: any[]) {
  console.log(...args)
}

gThis.loadFile = async function (fname: string): Promise<string> {
  let b: Buffer = fs.readFileSync('.' + fname)
  return b.toString()
}

main()