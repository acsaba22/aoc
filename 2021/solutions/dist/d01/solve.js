import * as util from '../util.js';
function p1(nums) {
    let increases = nums.slice(1).map((v, i) => nums[i] < v ? 1 : 0);
    return increases.reduce((a, b) => a + b);
}
function p2(nums) {
    return p1(nums.slice(2).map((v, i) => v + nums[i] + nums[i + 1]));
}
export async function main() {
    let s = await util.loadFile('/src/d01/input.txt');
    let input = s.trim().split('\n').map((s) => Number(s));
    util.log("P1: ", p1(input)); // 1557
    util.log("P2: ", p2(input)); // 1608
}
main();
