"use strict";
exports.__esModule = true;
exports.prsr2 = exports.prsr1 = void 0;
function prsr1(str) {
    var tokens = str.split(/\b/);
    console.log(tokens);
    if (tokens.length == 1
        && "0".charCodeAt(0) <= tokens[0].charCodeAt(0)
        && "9".charCodeAt(0) >= tokens[0].charCodeAt(0)) {
        return parseInt(tokens[0]);
    }
    else {
        console.error("expecting a single numeric digit");
    }
}
exports.prsr1 = prsr1;
function prsr2(str) {
    var tokens = str.split(/\b/);
    console.log(tokens);
    if (tokens.length == 1) {
        return parseInt(tokens[0]);
    }
    if ((tokens[0] + tokens[2]).match(/^[0-9]+$/)) {
        if (tokens[1] == "*") {
            return tokens[0] * tokens[2];
        }
        if (tokens[1] == "+") {
            return parseInt(tokens[0]) + parseInt(tokens[2]);
        }
    }
}
exports.prsr2 = prsr2;
console.log(prsr2("2") + " should be 2");
console.log(prsr2("2+2") + " should be 4");
console.log(prsr2("2*4") + " should be 8");
