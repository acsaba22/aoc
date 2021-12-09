import * as util from './util.js'
import * as fs from 'fs'

util.env.log = function (...args: any[]) {
  console.log(...args)
}

util.env.loadFile = async function (fname: string): Promise<string> {
  let b: Buffer = fs.readFileSync('.' + fname)
  return b.toString()
}


async function run() {
  var args = process.argv.slice(2);
  if (args.length != 1 || !/^\d{2}$/.test(args[0])) {
    console.log('Bad arguments. Usage, replace 01 with day number:\n$ node dist/main.js 01')
    return
  }
  const path = `./d${args[0]}/solve.js`
  const solve = await import(path);
  solve.main()
}

run()