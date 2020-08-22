export function prsr1 (str) {
  const tokens = str.split(/\b/);
  console.log(tokens);
  if (tokens.length == 1
    && "0".charCodeAt(0) <= tokens[0].charCodeAt(0)
    && "9".charCodeAt(0) >= tokens[0].charCodeAt(0)
     ) { return parseInt(tokens[0]) }
  else {
    console.error("expecting a single numeric digit")
  }
}

export function prsr2 (str) {
  const tokens = str.split(/\b/);
  console.log(tokens);
  if (tokens.length == 1) { return parseInt(tokens[0]) }
  if ((tokens[0]+tokens[2]).match(/^[0-9]+$/)) {
    if (tokens[1] == "*") { return tokens[0] * tokens[2]; }
    if (tokens[1] == "+") { return parseInt(tokens[0]) + parseInt(tokens[2]) }
  }
}
console.log(prsr2("2") + ` should be 2`)
console.log(prsr2("2+2") + ` should be 4`)
console.log(prsr2("2*4") + ` should be 8`)
