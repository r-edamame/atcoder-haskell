const input = require('fs').readFileSync('/dev/stdin', 'utf8');

const lines = input.trim().split('\n');
const ast = new Set(lines[0].trim().split(' '));
const bst = new Set(lines[1].trim().split(' '));

console.log(bst.difference(ast).size===0 ? "Yes" : "No");
