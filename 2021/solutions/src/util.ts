export function assert(condition: boolean, message: string = "Assertion failed") {
  if (!condition) {
    throw new Error(message);
  }
}



export class Environment {
  log: (...args: any[]) => void
  loadFile: (fname: string) => Promise<string>
  clear: () => void
  pause: (ms: number) => Promise<void>
  showCanvas: () => void
  rect: (y:number, x:number, n:number, m:number, color:string) => void
  constructor() {
    this.log = (...args: any[]) => assert(false, 'log not defined')
    this.loadFile = (fname) => {
      throw new Error('loadFile not defined');
    }
    this.clear = () => assert(false, 'clear not defined')
    this.pause = (ms) => new Promise(resolve => resolve());
    this.showCanvas = () => void 0
    this.rect = (y,x,n,m,color) => void 0
  }
}

export let env = new Environment()

export function log(...args: any[]): void {
  env.log(...args)
}

export function loadFile(fname: string): Promise<string> {
  return env.loadFile(fname)
}

export function pause(ms:number): Promise<void>  {
  return env.pause(ms)
}