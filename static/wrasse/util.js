const sleep = async (time) => {
    await new Promise(r => setTimeout(r, time));
}

const clamp = (min, num, max) => Math.max( min, Math.min(num, max));
const within = (min, num, max) => min <= num && num <= max;

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

export {sleep, clamp, within, group_n};