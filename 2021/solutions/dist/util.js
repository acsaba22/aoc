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
    }
}
export let env = new Environment();
export function log(...args) {
    env.log(...args);
}
export function loadFile(fname) {
    return env.loadFile(fname);
}
