const int = (e) => parseInt(e, 10);
const int1 = (e) => parseInt(e, 10);
const ints = (e) => e.map(int);
const ints1 = (e) => e.map(int1);

const main = (lines) => {
    const [h, w] = ints(lines[0].split(' '));
    const maze = lines.slice(1, h+1);
    
    const start = (() => {
        for (let x=0; x<w; x++) {
            for (let y=0; y<h; y++) {
                if (maze[y][x] === 'S') {
                    return [x, y];
                }
            }
        }
    })();
    
    const passed = [...new Array(h).keys()].map(() => [...new Array(w).keys()].map(() => false));
    
    const queue = [start];
    while (queue.length > 0) {
        const [x, y] = queue.shift();
        if (passed[y][x]) {
            continue;
        }
        passed[y][x] = true;
        
        // 一番外側のマスにいるかどうか
        if (x==0 || x==w-1 || y==0 || y==h-1) {
            console.log("YES");
            return;
        }
        
        for (let nx of [-1, 1]) {
            if (maze[y][x+nx] == '.') queue.push([x+nx, y]);
        }
        for (let ny of [-1, 1]) {
            if (maze[y+ny][x] == '.') queue.push([x, y+ny]);
        }
    }
    console.log("NO");
}
main(require('fs').readFileSync("/dev/stdin", "utf8").split("\n"));
