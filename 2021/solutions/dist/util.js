export function assert(condition, message = "Assertion failed") {
    if (!condition) {
        throw new Error(message);
    }
}
export class Environment {
    constructor() {
        this.log = (...args) => assert(false, 'log not defined');
        this.loadFile = (fname) => {
            throw new Error('loadFile not defined');
        };
        this.clear = () => assert(false, 'clear not defined');
        this.pause = (ms) => new Promise(resolve => resolve());
        this.showCanvas = () => void 0;
        this.rect = (y, x, n, m, color) => void 0;
    }
}
export let env = new Environment();
export function log(...args) {
    env.log(...args);
}
export function loadFile(fname) {
    return env.loadFile(fname);
}
export function pause(ms) {
    return env.pause(ms);
}
