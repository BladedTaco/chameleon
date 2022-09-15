import ESC from "./ansiEscapes";
import wrasse from "./wrasse";
import {sleep, clamp, within, group_n, null_func, deep_copy, last} from './util';

String.prototype.splice = function (index, count, add) {
    // We cannot pass negative indexes directly to the 2nd slicing operation.
    if (index < 0) {
      index = this.length + index;
      if (index < 0) {
        index = 0;
      }
    }
  
    return this.slice(0, index) + (add || "") + this.slice(index + count);
}

class Link {
    static albedo = {
        click : 1,
        hover : 0.6,
        unlit : 0.3
    }

    constructor(window, range, funcs, colour) {
        this.window = window;
        this.range = {
            start : {x : 0, y : 0, ...range.start},
            end : {x : 0, y : 0, ...range.end},
        }
        this.colour = colour ?? ESC.Colour.Red
        this.funcs = {
            enter : (link) => {
                link.setHighlight(Link.albedo.hover)
                funcs?.enter?.(link);
            },
            leave : (link) => {
                link.setHighlight(Link.albedo.unlit)
                funcs?.leave?.(link);
            },
            click : (link) => {
                link.setHighlight(Link.albedo.click)
                funcs?.click?.(link)
            }
        }

        this.active = false;
        this.highlight = {fg:{}, bg:{}, resetFG:{}, resetBG:{}};

        this.setupHighlight();
    }

    setupHighlight() {
        const [fg, bg, resetFG, resetBG] = this.window.write(
            ESC.cursorSavePosition +
            ESC.cursorTo(this.range.start.x, this.range.start.y) + 
            ESC.colouredText(ESC.Colour.White, this.colour.mul(Link.albedo.unlit), "|")
                .split("|")
                .join(ESC.cursorTo(this.range.end.x, this.range.end.y)) +
            ESC.cursorRestorePosition
        );
        this.highlight = {fg, bg, resetFG, resetBG}
    }

    setHighlight(multiplier) {
        this.highlight.bg.seq = ESC.colourSeq(this.colour.mul(multiplier), false);
        // this.window.links
        //     // sort by x
        //     .sort((a, b) =>
        //         a.range.start.x - b.range.start.x
        //     )
        //     // sort by y
        //     .sort((a, b) => 
        //         a.range.start.y - b.range.start.y
        //     )
        //     .reduce((acc, curr) => {

        //         // filter out previous links which dont run past the end of this one
        //         acc = acc.filter(el => 
        //             (el.range.end.y >= curr.range.end.y)
        //             && (el.range.end.x > curr.range.end.x)
        //         );

        //         // match colour from last link at end of current link
        //         curr.highlight.resetFG.seq = (last(acc)?.highlight.fg.seq) ?? "\u001b[39m";
        //         curr.highlight.resetBG.seq = (last(acc)?.highlight.bg.seq) ?? "\u001b[49m";

        //         // add curr to accumulator
        //         return [...acc, curr];
        //     }, []);
        // this.window.requestDraw();
    }
}

class Window {
    constructor(terminal, x, y, width, height, options) {
        this.terminal = terminal;
        this.x = Math.ceil(x);
        this.y = Math.ceil(y);
        this.width = Math.floor(width);
        this.height = Math.floor(height);
        this.line = 0;
        this.scroll = 0;
        this.content = [{text:"", esc:[]}];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        options = {movable : false, scrollable : true, resizable : false, softwrap: false, ...options};
        this.movable = options.movable;
        this.scrollable = options.scrollable;
        this.resizable = options.resizable;
        this.softwrap = options.softwrap;
        this.links = [];
        this.active = false;
        this.dirty = true;
        this.softContent = [];

        Window.setup();
    }

    // statics
    static drawReq = {
        request: false,
        setup : false,
        callbacks : [],
    };

    static setup() {
        if (Window.drawReq.setup) return;
        Window.drawReq.setup = true;

        // setup events
        Window.setupEvent('wheel', 'onWheel');
        Window.setupEvent('mousemove', 'onMouseMove');
        Window.setupEvent('mouseout', 'onMouseMove');
        Window.setupEvent('mousedown', 'onClick');
        Window.setupEvent('mouseup', 'onMouseMove');

        // setup draw requests
        (async () => {
            while(true) {
                if (Window.drawReq.request) {
                    wrasse.window.draw();
                    wrasse.perm.windows.forEach(x => x.draw());
                    Window.drawReq.request = false;
                    Window.drawReq.callbacks.forEach(x => x());
                    Window.drawReq.callbacks = [];
                }
                await sleep(1);
            }
        })();
    }

    static setupEvent(eventType, eventFunc) {
        wrasse.html.terminal.addEventListener(eventType, (event) => {
            for (let i = wrasse.perm.windows.length - 1; i >= 0; i--) {
                if (wrasse.perm.windows[i][eventFunc](event)) return;
            }
            wrasse.window[eventFunc](event);
        }, {
            capture : true,
            passive : false,
        });
    }


    onWheel(event) {
        if (!this.scrollable || !this.active) return false;

        const dir = Math.sign(event.deltaY)
        if (event.ctrlKey) { // move LR
            this.move(dir, 0, true);
        } else if (event.altKey) { // move UD
            this.move(0, dir, true);
        } else if (event.shiftKey) { // scroll horizontal
            if (this.softwrap) return;
            const textWidth = this.content.reduce(
                (acc, curr) => Math.max(acc, curr.text.length)
            , 0);
            const dirX = dir * clamp(
                1, 
                Math.round(Math.pow(textWidth / this.width, 1.3)), 
                Math.floor(this.width * 0.7)
            );
            this.scroll = clamp(
                0, 
                this.scroll + dirX,
                textWidth - this.width
            );
        } else { // scroll vertical
            const dirY = dir * clamp(
                1, 
                Math.round(this.content.length / this.height), 
                Math.floor(this.height * 0.7)
            );
            this.line = clamp(0, 
                this.line + dirY,
                Math.max(this.softContent.length, this.content.length) - this.height 
            );
        }
        this.onMouseMove(event);
        this.requestDraw();
        return true;
    }

    onMouseMove(event) {
        this.active = this.mouseWithin(event.offsetX, event.offsetY);
        // exit if mouse not within
        if (!this.active) return false;

        // get cell x and y
        let {x, y} = this.mouseToCell(event.offsetX, event.offsetY);
        x += this.scroll
        y += this.line 

        // get active link
        let activeLink = this.links
            .filter (curr => 
                within(curr.range.start.x, x, curr.range.end.x)
                && within(curr.range.start.y, y, curr.range.end.y)
            )
            .sort((a, b) => b.range.start.x - a.range.start.x)
            [0]

        // make link active if not already
        if (activeLink?.active === false) {
            activeLink.funcs.enter(activeLink)
            activeLink.active = true;
            this.requestDraw();
        }

        this.links
            // get links changing state
            .filter(curr => 
                 curr.active
            )
            .filter(x => x != activeLink)
            // perform exit functions and flip state
            .forEach(x => {
                x.funcs.leave(x)
                x.active = false;
                this.requestDraw();
            });

        return true;
    }

    onClick(event) {
        // exit if mouse not within
        if (!this.active) return false;

        // get active links
        this.links
            .filter(x => x.active)
            .forEach(x => x.funcs.click(x));

        // this.requestDraw();
        return true;
    }

    /*
        ESC.cursorSavePosition
        ESC.cursorRestorePosition
        ESC.deleteLine
        ESC.cusorTo
        ESC.insertLine
    */

    addLink(range, funcs, colour) {
        let link = new Link(
            this, range, funcs, colour
        )
        this.links.push(link);
        return link;
    }

    reset() {
        this.clean();
        this.content = [{text:"", esc:[]}];
        this.links = []
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        this.line = 0;

        return this;
    }

    write(text, callback) {
        this.dirty = true;
        const handleEscape = (text) => {
            // special character regex
            const regex = /\u001B\[(?:(?<nums>(?:[0-9]+;)*)(?<num>(?:[0-9]+)))?(?<char>[a-zA-Z])/gi;

            // handle special characters
            let cutString = [];
            let anyMatches = false;
            for (const [prefix, _nums, num, char] of group_n(text.split(regex), 4)) {
                let cut = {prefix, esc: null_func};
                if ((_nums || num || char) === undefined) {
                    cutString.push(cut)
                    break;
                }
                anyMatches = true;
                let nums = (_nums?.split(';') || [])
                    .filter(x => x !== '');
                nums.push(num);
                switch (char) {
                    // // Colours
                    // case 'm':
                    //     cut.esc = () => {
                            
                    //     }
                    // break;
                    // cursorTo
                    case 'G':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, x: num+1};
                        }
                    break;
                    case 'H':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, x: +nums[1]-1, y: +nums[0]-1};
                        }
                    break;
                    // Save Cursor
                    case 's':
                        cut.esc = () => {
                            this.cursor.saved = {x: this.cursor.x, y: this.cursor.y};
                        }
                    break;
                    // Restore Cursor
                    case 'u':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, ...this.cursor.saved};
                        }
                    break;
                    // Insert Line (current line pushed up)
                    case 'L':
                        cut.esc = () => {
                            this.content.splice(
                                this.cursor.y+1,
                                0, 
                                ...Array.from({length: +nums[0]}, _ => ({text:"", esc:[]}))
                            );
                        }
                    break;
                    // Delete Line (current line delted)
                    case 'M':
                        cut.esc = () => {
                            this.content.splice(this.cursor.y, +num);
                        }
                    break;
                    // Colours
                    case 'm':
                        cut.esc = () => `\u001B[${nums.join(";") + char}`
                    break;
                    // Fallthrough
                    default:
                        console.log(`UNMATCHED ESCAPE SEQUENCE: ESC[${nums.join(";") + char}`)
                        cut.esc = () => `\u001B[${nums.join(";") + char}`
                    break;
                }
                cutString.push(cut);
            }
            if (anyMatches == false) return [{prefix: text, esc: null_func}];

            return cutString;
        };

        let escAdds = []

        // write the string to the window
        for (const {prefix, esc} of handleEscape(text)) {
            let oldCursor = {...this.cursor};
            for (const line of (prefix ?? "").split('\n')) {
                // ensure cursor isn't off content bounds
                while (this.cursor.y >= this.content.length) {
                    this.content.push({text:"", esc:[]});
                }
                // overwrite content with line
                this.content[this.cursor.y].text = this.content[this.cursor.y].text
                    .padEnd(this.cursor.x)
                    .splice(this.cursor.x, line.length, line);
                // overwrite escape sequences too
                this.content[this.cursor.y].esc = this.content[this.cursor.y].esc
                    .filter(({pos}) => !within(1, pos - this.cursor.x, line.length - 2))
                // intercalate cursor reset
                this.cursor.x += line.length;
                oldCursor = {...this.cursor};
                this.cursor.y += 1
                this.cursor.x = 0;
            }
            // restore last oldCursor
            this.cursor = oldCursor;
            // handle escape characters
            let add = esc();
            if (add) {
                let addObj = {pos : this.cursor.x, seq : add}
                escAdds.push(addObj)
                this.content[this.cursor.y].esc.push(addObj)
            }
            // ensure cursor isn't off content bounds
            while (this.cursor.y >= this.content.length) {
                this.content.push({text:"", esc:[]});
            }
        }

        this.requestDraw(callback);

        return escAdds
    }

    writeln(text, callback) {
        return this.write(text + "\n", callback);
    }

    mouseWithin(relX, relY) {
        const {x, y} = this.mouseToCell(relX, relY);
        return within(this.x - 0.5, x, this.x + this.width  + 0.5)
            && within(this.y - 0.5, y, this.y + this.height + 0.5);
    }

    mouseToCell(relX, relY) {
        const cellHeight = wrasse.html.terminal.offsetHeight / wrasse.terminal.rows;
        const cellWidth  = wrasse.html.terminal.offsetWidth  / wrasse.terminal.cols;
        return {
            x : Math.floor(relX / cellWidth  - 1),
            y : Math.floor(relY / cellHeight - 1),
        }
    }

    resize(width, height, relative, force = false) {
        if (!this.resizable && !force) return;

        // make softContent dirty
        this.dirty = true;
        // check for relative resizing
        if (relative) {
            width += this.width;
            height += this.height;
        }

        // clean old position
        this.clean();

        // update size
        this.width = Math.floor(width);
        this.height = Math.floor(height);

        this.move(0,0, true, true)

        this.onWheel({deltaY: 0})

        this.requestDraw();
    }

    expand(force = false) {
        this.move(0, 0, false, force);
        this.resize(this.terminal.cols - 2, this.terminal.rows - 2, false, force);
    }

    move(x, y, relative, force = false) {
        if (!this.movable && !force) return;

        // check for relative positioning
        if (relative) {
            x += this.x;
            y += this.y;
        }

        x = clamp(0, x, this.terminal.cols - this.width - 2)
        y = clamp(0, y, this.terminal.rows - this.height - 2)

        // clean old position
        this.clean();

        // update position
        this.x = Math.floor(x);
        this.y = Math.floor(y);
        this.requestDraw();
    }

    *lines() {
        for (const line of this.content.slice(this.line, this.line + this.height)) {
            yield {
                text: line.text.slice(this.scroll, this.width + this.scroll),
                esc: line.esc
                .map(({pos, seq}) => {return {pos: clamp(0, pos - this.scroll, this.width), seq}})
                // .filter(({pos, seq}) => within(0, pos, this.width) || seq.match(/\[(3|4)9m/g))
                // esc: line.esc,
            }
        }
    }

    // inefficient function that regenerates on each draw
    // soft-wraps the lines
    *linesWrap() {
        const window = this;
        function* softlines() {
            let lines = 0;
            for (const line of window.content) {
                console.log(line)
                let range = {start: 0, end: window.width}
                let lastEsc = []
                for (const text of group_n(line.text, window.width)) {
                    const escs = lastEsc.concat(
                        line.esc
                            .filter(({pos}) => within(range.start, pos, range.end))
                            .map(({pos, ...rest}) => {return {...rest, pos: pos - range.start}})
                    )

                    console.log(text.join(""), escs)

                    yield {
                        text : text.join(""),
                        esc: escs
                    }
                    range.start = range.end
                    range.end += window.width
                    lines += 1
                    lastEsc = Object.values(escs.reduce((acc, curr) => {
                        const {type} = curr.seq.matchAll(/\x1b\[(?<type>3|4)(?<col>9m|8;2)/g)?.next()?.value?.groups
                        return {...acc, ...(
                              type === '3' ? {fg : curr} 
                            : type === '4' ? {bg : curr} 
                            : {}
                        )}
                    }, {}))
                        .filter(x => x !== undefined)
                        .map(({pos, ...rest}) => {return {...rest, pos: 0}})
                }
            }
        }

        if (this.dirty) {
            this.dirty = false;
            this.softContent = Array.from(softlines())
        }

        for (const x of this.softContent.slice(this.line, this.line + this.height)) {
            yield x;
        }
        
    }

    
    draw() {
        const textWidth = this.content.reduce(
            (acc, curr) => Math.max(acc, curr.text.length)
        , 0);

        const scrollbarX = {
            low: Math.max(-0.01, this.scroll / textWidth) * this.width,
            hi: Math.min(1, (this.scroll + this.width) / textWidth) * this.width,
        }
        scrollbarX.hi = Math.max(Math.ceil(scrollbarX.low)+0.5, scrollbarX.hi)

        let scrollbarXChar = '▅';
        if (scrollbarX.low <= 0 && scrollbarX.hi >= this.height) {
            scrollbarXChar = '═'
        }

        const top = Array.from(Array(this.width).keys())
            .map(x => within(scrollbarX.low, x, scrollbarX.hi) 
            ? scrollbarXChar 
            : '═')
            .join('')


        const borderChar = (s) => ESC.colouredText(ESC.Colour.LtDkGrey, {}, s)

        // draw top and bottom border
        let writeString = borderChar(
            ESC.cursorSavePosition 
          + ESC.cursorTo(this.x, this.y) 
          + `╔${top}╗`
          + ESC.cursorTo(this.x, this.y + this.height + 1) 
          + `╚${top}╝`);
        
        // draw content
        let lines = this.softwrap 
            ? this.linesWrap() 
            : this.lines()
        const len = Math.max(this.content.length, this.softContent.length)
        const scrollbar = {
            low: Math.max(-0.01, this.line / len) * this.height,
            hi: Math.min(1, (this.line + this.height) / len) * this.height,
        }
        scrollbar.hi = Math.max(Math.ceil(scrollbar.low)+0.5, scrollbar.hi)

        let scrollbarChar = '█';
        if (scrollbar.low <= 0 && scrollbar.hi >= this.height) {
            scrollbarChar = '║'
        }

        let escString = ""

        for (let i = 1; i <= this.height; i++) {
            // get the next line
            const {text, esc} = lines.next().value ?? {text:"", esc:[]};
            const sideChr = borderChar(within(scrollbar.low, i, scrollbar.hi) 
                ? scrollbarChar 
                : '║')

            console.log(text)

            const bodyText = esc
            // sort by position
            .sort((a, b) => a.pos - b.pos)
            // map to colour pairs
            .reduce((acc, curr) => {
                // get fg/bg and colour/reset
                console.log(curr)
                const {type, col} = curr.seq.matchAll(/\x1b\[(?<type>3|4)(?<col>9m|8;2)/g).next().value.groups;
                // get stack based on foreground or background
                const stack = (type == "3" ? acc.fgstack : acc.bgstack);
                // if resetting colour
                if (col == "9m") {
                    // remove current colour
                    stack.pop();
                    // pop stack to out with curr
                    acc.out.push(stack.length == 0 
                        // push colour reset
                        ? curr
                        // reset to last colour
                        : {pos: curr.pos, seq: last(stack).seq }
                    )
                // adding colour
                } else {
                    // push current to stack and out
                    stack.push(curr)
                    acc.out.push(curr)
                }
                // return accumulator
                return acc;
            }, {fgstack:[], bgstack:[], out:[]})
            // map colour pairs to escape sequence strings
            .out.reduceRight(
                // insert the escape sequence into the string
                (acc, {pos, seq}) => {
                    return acc.splice(pos, 0, seq)
                }
                , (text || "").padEnd(this.width)
            )

            // add it to the write string
            writeString += `${ESC.cursorTo(this.x, this.y + i)}${sideChr}${
                bodyText
            }${sideChr}`;
        }
        
        // restore cursor
        this.terminal.write(writeString + ESC.cursorRestorePosition);
    }
    
    requestDraw(callback) {
        // request draw
        Window.drawReq.request = true;
        if (callback) Window.drawReq.callbacks.push(callback);
    }

    clean() {
        // save cursor
        this.terminal.write(ESC.cursorSavePosition )
            
        // draw clear
        for (let i = 0; i <= this.height+1; i++) {
            this.terminal.write(
                `${ESC.cursorTo(this.x, this.y + i)}${"".padEnd(this.width + 2)}`
            );
        }

        // restore cursor
        this.terminal.write(ESC.cursorRestorePosition);
    }

    
}



const termWindows = {
    "Window" : Window,
};

export default termWindows;