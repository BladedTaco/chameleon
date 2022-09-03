const sleep = async (time) => {
    await new Promise(r => setTimeout(r, time));
}

const clamp = (min, num, max) => Math.max( min, Math.min(num, max));
const within = (min, num, max) => (min <= num) && (num <= max);

const deep_copy = (x) => JSON.parse(JSON.stringify(x))

const null_func = () => {};

function* group_n(arr, n) {
    let out = [];
    for (const el of arr) {
        out.push(el);
        if (out.length == n) {
            yield out;
            out = [];
        }
    }
    if (out != []) {
        yield out;
    }
}

const start_pattern_gen = (function *() {
    while (true) {
      const frames = ["▘ ", "▀ ", "▝ ", " ▘", " ▌", " ▖", "▗ ", "▄ ", "▖ ", "▌ "] 
      for (const item of frames) {
        yield item;
    }
    }
  })

function last(array) {
    return array.length == 0 
        ? undefined
        : array[array.length - 1];
}

function debounce(func, timeout = 300) {
    let keepGoing = false;
    let running = false;
    let nextArgs = []
    const debouncedFunc = (...args) => {
        // debounce, put off until cycle is finished
        if (running) {
            keepGoing = true;
            nextArgs = args;
            return;
        }

        // run function
        func.apply(this, args);
        keepGoing = false;
        running = true;
        // wait timeout amount
        sleep(timeout).then( () => {
            // finish cycle
            running = false;
            // if calls happened while waiting
            if (keepGoing) {
                // call func with last args, setting off a new cycle.
                debouncedFunc(...args)
            }
        })
    };
    return debouncedFunc;
}

export {sleep, clamp, within, group_n, null_func, deep_copy, start_pattern_gen, last, debounce};