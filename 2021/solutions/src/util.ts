export function assert(condition: boolean, message: string = "Assertion failed") {
  if (!condition) {
    throw new Error(message);
  }
}

export class Environment {
  log: (...args: any[]) => void
  loadFile: (fname: string) => Promise<string>
  constructor() {
    this.log = (...args: any[]) => assert(false, 'log not defined')
    this.loadFile = (fname) => {
      throw new Error('loadFile not defined');
    }
  }
}

export let env = new Environment()

export function log(...args: any[]): void {
  env.log(...args)
}

export function loadFile(fname: string): Promise<string> {
  return env.loadFile(fname)
}