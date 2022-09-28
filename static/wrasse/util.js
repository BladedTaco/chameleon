// a basic sleep function, a Promise version of setTimeout without function callback
const sleep = async (time) => {
    await new Promise(r => setTimeout(r, time));
}

// clamps the middle number to the range
const clamp = (min, num, max) => Math.max( min, Math.min(num, max));
// returns if the middle number is within the range
const within = (min, num, max) => (min <= num) && (num <= max);

// deep copy for an object
const deep_copy = (x) => JSON.parse(JSON.stringify(x))

// empty function
const null_func = () => {};

// a generator function to group an iterable into groups of n as arrays
function* group_n(arr, n) {
    // initialise the output array
    let out = [];
    // for each element in iterable
    for (const el of arr) {
        // push to output
        out.push(el);
        // and when the output is length n
        if (out.length == n) {
            // yield the output and reset it
            yield out;
            out = [];
        }
    }
    // if the iterable is finished with a partial group
    if (out != []) {
        // output that partial group
        yield out;
    }
}

// a generator function to make a spinning cursor over two horizontally neighbouring cells.
const start_pattern_gen = (function *() {
    // forever
    while (true) {
      // the frames for the animation
      const frames = ["▘ ", "▀ ", "▝ ", " ▘", " ▌", " ▖", "▗ ", "▄ ", "▖ ", "▌ "] 
      for (const item of frames) {
        // yield current frame
        yield item;
    }
    }
  })

// returns the last element of an array, or undefined if it doesn't exist
function last(array) {
    return array.length == 0 
        ? undefined
        : array[array.length - 1];
}

// debounces a function with a timeout, but guarantees execution at end of timeout with arguments if another call is given during debounce.
function debounce(func, timeout = 300) {
    // helper variables
    let keepGoing = false;
    let running = false;
    let nextArgs = []
    // the provided function with the debounce wrapper
    const debouncedFunc = (...args) => {
        // debounce, put off until cycle is finished
        if (running) {
            // ensure function is called at end of timeout with newest argument list.
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
    // return debounce function
    return debouncedFunc;
}

export {sleep, clamp, within, group_n, null_func, deep_copy, start_pattern_gen, last, debounce};