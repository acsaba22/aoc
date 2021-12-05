import { main } from "./solve.js";
import * as fs from 'fs';
const gThis = globalThis;
gThis.log = function (...args) {
    console.log(...args);
};
gThis.loadFile = async function (fname) {
    let b = fs.readFileSync('.' + fname);
    return b.toString();
};
main();
