"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs_1 = require("fs");
main();
function p1(nums) {
    let increases = nums.slice(1).map((v, i) => nums[i] < v ? 1 : 0);
    return increases.reduce((a, b) => a + b);
}
function p2(nums) {
    return p1(nums.slice(2).map((v, i) => v + nums[i] + nums[i + 1]));
}
function main() {
    let b = (0, fs_1.readFileSync)('./src/d01/input.txt');
    let input = b.toString().trim().split('\n').map((s) => Number(s));
    console.log("P1: ", p1(input)); // 1557
    console.log("P2: ", p2(input)); // 1608
}
