import ESC from "./ansiEscapes";
import wrasse from "./wrasse";
import {sleep, clamp, within, group_n, null_func, deep_copy, last} from './util';

// a string prototype function to implement array-like splicing for strings
String.prototype.splice = function (index, count, add) {
    // We cannot pass negative indexes directly to the 2nd slicing operation.
    if (index < 0) {
      index = this.length + index;
      if (index < 0) {
        index = 0;
      }
    }
  
    // return two slices with any additions made in the middle.
    return this.slice(0, index) + (add || "") + this.slice(index + count);
}

// a class for actionable highlights in the wrasse window
class Link {
    // the different multipliers for different states of the link
    static albedo = {
        click : 1,
        hover : 0.6,
        unlit : 0.3
    }

    constructor(window, range, funcs, colour = ESC.Colour.Red) {
        this.window = window;
        // range with optional start and end x/y
        this.range = {
            start : {x : 0, y : 0, ...range.start},
            end : {x : 0, y : 0, ...range.end},
        }
        // optional colour
        this.colour = colour
        // optional, extensible functions for interaction
        this.funcs = {
            // function for starting to hover the link
            enter : (link) => {
                link.setHighlight(Link.albedo.hover)
                funcs?.enter?.(link);
            },
            // function for unhovering a link
            leave : (link) => {
                link.setHighlight(Link.albedo.unlit)
                funcs?.leave?.(link);
            },
            // function for clicking a link
            click : (link) => {
                link.setHighlight(Link.albedo.click)
                funcs?.click?.(link)
            }
        }

        // start inactive with no highlights
        this.active = false;
        this.highlight = {fg:{}, bg:{}, resetFG:{}, resetBG:{}};

        // setup highlight handling
        this.setupHighlight();
    }

    // intiializes the various things needed for link highlighting
    setupHighlight() {
        // write the current highlight colour
        const [fg, bg, resetFG, resetBG] = this.window.write(
            ESC.cursorSavePosition +
            ESC.cursorTo(this.range.start.x, this.range.start.y) + 
            ESC.colouredText(ESC.Colour.White, this.colour.mul(Link.albedo.unlit), "|")
                .split("|")
                .join(ESC.cursorTo(this.range.end.x, this.range.end.y)) +
            ESC.cursorRestorePosition
        );
        // set incoming and outgoing highlight colours.
        this.highlight = {fg, bg, resetFG, resetBG}
    }

    // sets the new highlight multiplier
    setHighlight(multiplier) {
        this.highlight.bg.seq = ESC.colourSeq(this.colour.mul(multiplier), false);
    }
}

// class for a window inside the terminal
class Window {
    constructor(terminal, x, y, width, height, options) {
        // basic constructed parameters
        this.terminal = terminal;
        this.x = Math.ceil(x);
        this.y = Math.ceil(y);
        this.width = Math.floor(width);
        this.height = Math.floor(height);
        this.line = 0;
        this.scroll = 0;
        this.content = [{text:"", esc:[]}];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        // optional options for optioning the window's options.
        options = {movable : false, scrollable : true, resizable : false, softwrap: false, ...options};
        this.movable = options.movable;
        this.scrollable = options.scrollable;
        this.resizable = options.resizable;
        this.softwrap = options.softwrap;
        this.links = [];
        this.active = false;
        this.dirty = true;
        this.softContent = [];

        // setup the terminal for windows
        Window.setup();
    }

    // statics
    static drawReq = {
        request: false,
        setup : false,
        callbacks : [],
    };

    // setup the terminal for windows
    static setup() {
        // only call once
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
            // forever once a 'frame'
            while(true) {
                // if a window needs to be redrawn
                if (Window.drawReq.request) {
                    // redraw lowest window
                    wrasse.window.draw();
                    wrasse.perm.windows.forEach(x => x.draw());
                    // redraw all other windows from lowest to highest so highest ends on top.
                    Window.drawReq.request = false;
                    Window.drawReq.callbacks.forEach(x => x());
                    Window.drawReq.callbacks = [];
                }
                // wait until next event loop
                await sleep(1);
            }
        })();
    }

    // set up an event for all windows
    static setupEvent(eventType, eventFunc) {
        // bind the event listener
        wrasse.html.terminal.addEventListener(eventType, (event) => {
            // call each windows callback in order from top to bottom
            for (let i = wrasse.perm.windows.length - 1; i >= 0; i--) {
                if (wrasse.perm.windows[i][eventFunc](event)) return;
            }
            wrasse.window[eventFunc](event);
        }, {
            // ensure events are consumed
            capture : true,
            passive : false,
        });
    }

    // event for scrolling
    onWheel(event) {
        // exit if not scrollable
        if (!this.scrollable || !this.active) return false;

        // get direction to move
        const dir = Math.sign(event.deltaY)
        if (event.ctrlKey) { // move LR
            this.move(dir, 0, true);
        } else if (event.altKey) { // move UD
            this.move(0, dir, true);
        } else if (event.shiftKey) { // scroll horizontal
            if (this.softwrap) return; // no horizontal scroll with softwrap
            // get text width from maximum length line
            const textWidth = this.content.reduce(
                (acc, curr) => Math.max(acc, curr.text.length)
            , 0);
            // get amount to move based on amount that can be scrolled as clamped percentage
            const dirX = dir * clamp(
                1, 
                Math.round(Math.pow(textWidth / this.width, 1.3)), 
                Math.floor(this.width * 0.7)
            );
            // scroll while keeping to limits
            this.scroll = clamp(
                0, 
                this.scroll + dirX,
                textWidth - this.width
            );
        } else { // scroll vertical
            // get amount to move scaled by content height
            const dirY = dir * clamp(
                1, 
                Math.round(this.content.length / this.height), 
                Math.floor(this.height * 0.25)
            );
            // scroll while keeping to limits
            this.line = clamp(0, 
                this.line + dirY,
                Math.max(this.softContent.length, this.content.length) - this.height 
            );
        }
        // call mouse move event and redraw
        this.onMouseMove(event);
        this.requestDraw();
        return true;
    }

    // event for mouse move
    onMouseMove(event) {
        // exit if mouse not within bounds
        this.active = this.mouseWithin(event.offsetX, event.offsetY);
        if (!this.active) return false;

        // get cell x and y
        let {x, y} = this.mouseToCell(event.offsetX, event.offsetY);
        x += this.scroll
        y += this.line 

        // get active link
        let activeLink = this.links
            .filter (curr => 
                within(curr.range.start.x, x, curr.range.end.x - 1)
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

    // event for clicks
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

    // adds a new link to the window
    addLink(range, funcs, colour) {
        let link = new Link(
            this, range, funcs, colour
        )
        this.links.push(link);
        return link;
    }

    // resets the window to a clean state
    reset() {
        this.clean();
        this.content = [{text:"", esc:[]}];
        this.links = []
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        this.line = 0;

        return this;
    }

    // writes text into the window's buffer
    write(text, callback) {
        this.dirty = true;
        const handleEscape = (text) => {
            // special character regex
            const regex = /\u001B\[(?:(?<nums>(?:[0-9]+;)*)(?<num>(?:[0-9]+)))?(?<char>[a-zA-Z])/gi;

            // handle special characters
            let cutString = [];
            let anyMatches = false;
            // extract escape sequences from string
            for (const [prefix, _nums, num, char] of group_n(text.split(regex), 4)) {
                // base case for unmatched escape sequence or malformed escape sequence
                let cut = {prefix, esc: null_func};
                if ((_nums || num || char) === undefined) {
                    cutString.push(cut)
                    break;
                }
                anyMatches = true;
                // get numbers from the escape sequence
                let nums = (_nums?.split(';') || [])
                    .filter(x => x !== '');
                nums.push(num);
                // match the escape sequence to its effect
                switch (char) {
                    // cursor xpos
                    case 'G':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, x: num+1};
                        }
                    break;
                    // cursorTo
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
                                // insert n empty lines
                                ...Array.from({length: +nums[0]}, _ => ({text:"", esc:[]}))
                            );
                        }
                    break;
                    // Delete Line (current line and below deleted)
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
                // add effect to list
                cutString.push(cut);
            }
            // on no matches, just return a simple string
            if (anyMatches == false) return [{prefix: text, esc: null_func}];
            // otherwise return strings with effects
            return cutString;
        };

        let escAdds = []

        // write the string to the window
        // for each substring and effect
        for (const {prefix, esc} of handleEscape(text)) {
            let oldCursor = {...this.cursor};
            // for each line of the string
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

        // request redraw
        this.requestDraw(callback);
        // return effects with positions
        return escAdds
    }

    // write to the window with appended newline
    writeln(text, callback) {
        return this.write(text + "\n", callback);
    }

    // check if the mouse is within the window based on relative position
    mouseWithin(relX, relY) {
        const {x, y} = this.mouseToCell(relX, relY);
        // return within x range && within y range
        return within(this.x - 0.5, x, this.x + this.width  + 0.5)
            && within(this.y - 0.5, y, this.y + this.height + 0.5);
    }

    // map mouse position to cell position
    mouseToCell(relX, relY) {
        // cell dimensions
        const textLayer = wrasse.html.terminal.querySelector('.xterm-text-layer')
        const cellHeight = textLayer.offsetHeight / wrasse.terminal.rows;
        const cellWidth  = textLayer.offsetWidth  / wrasse.terminal.cols;
        // perform mapping
        return {
            x : Math.floor(relX / cellWidth  - 1),
            y : Math.floor(relY / cellHeight - 1),
        }
    }

    // resize the window
    resize(width, height, relative, force = false) {
        // exit if not resizable
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

        // update and validate position and graphics
        this.move(0,0, true, true)
        this.onWheel({deltaY: 0})
        this.requestDraw();
    }

    // fill terminal area
    expand(force = false) {
        this.move(0, 0, false, force);
        this.resize(this.terminal.cols - 2, this.terminal.rows - 2, false, force);
    }

    // move window
    move(x, y, relative, force = false) {
        // exit if not movable or forced
        if (!this.movable && !force) return;

        // check for relative positioning
        if (relative) {
            x += this.x;
            y += this.y;
        }

        // clamp x and y to limits
        x = clamp(0, x, this.terminal.cols - this.width - 2)
        y = clamp(0, y, this.terminal.rows - this.height - 2)

        // clean old position
        this.clean();

        // update position
        this.x = Math.floor(x);
        this.y = Math.floor(y);
        this.requestDraw();
    }

    // lines generator function for drawing
    *lines() {
        // for each visible section of a line
        for (const line of this.content.slice(this.line, this.line + this.height)) {
            //  yield its text and effects
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
    // soft-wrap line generator for drawing with soft-wrap
    *linesWrap() {
        const window = this;
        // inner function for turning the content array into a soft-wrapped content array
        function* softlines() {
            let lines = 0;
            // for each line of content
            for (const line of window.content) {
                let range = {start: 0, end: window.width}
                let lastEsc = []
                // for each line of softwrapped content
                for (const text of group_n(line.text, window.width)) {
                    // attach relevant effects/escape sequences in relevant positions
                    const escs = lastEsc.concat(
                        line.esc
                            .filter(({pos}) => within(range.start, pos, range.end))
                            .map(({pos, ...rest}) => {return {...rest, pos: pos - range.start}})
                    )
                    
                    // yield line with effects
                    yield {
                        text : text.join(""),
                        esc: escs
                    }
                    // update tracked state
                    range.start = range.end
                    range.end += window.width
                    lines += 1
                    // get the last escape sequence to pass along to next soft-wrapped line in current real line
                    lastEsc = Object.values(escs.reduce((acc, curr) => {
                        // match the escape sequence for colours
                        const {type} = curr.seq.matchAll(/\x1b\[(?<type>3|4)(?<col>9m|8;2)/g)?.next()?.value?.groups
                        // pass along
                        return {...acc, ...(
                              type === '3' ? {fg : curr} 
                            : type === '4' ? {bg : curr} 
                            : {}
                        )}
                    }, {}))
                    // filter out invalid escape sequences and map to new start of line
                        .filter(x => x !== undefined)
                        .map(({pos, ...rest}) => {return {...rest, pos: 0}})
                }
            }
        }

        // if content has changed since last soft-wrap draw
        if (this.dirty) {
            // clean and regenerate softContent
            this.dirty = false;
            this.softContent = Array.from(softlines())
        }

        // for each line of softcontent in the visible range
        for (const x of this.softContent.slice(this.line, this.line + this.height)) {
            // yield the line
            yield x;
        }
        
    }

    // drawing function
    draw() {
        // get the width of the longest line
        const textWidth = this.content.reduce(
            (acc, curr) => Math.max(acc, curr.text.length)
        , 0);

        // get the range in the window to draw the scrollbar
        const scrollbarX = {
            low: Math.max(-0.01, this.scroll / textWidth) * this.width,
            hi: Math.min(1, (this.scroll + this.width) / textWidth) * this.width,
        }
        scrollbarX.hi = Math.max(Math.ceil(scrollbarX.low)+0.5, scrollbarX.hi)

        // dont draw a horizontal scrollbar with a softwrap terminal or no scrolling ability
        let scrollbarXChar = '▅';
        if (this.softwrap || (scrollbarX.low <= 0 && scrollbarX.hi >= this.height)) {
            scrollbarXChar = '═'
        }

        // map a list of natural numbers to the list of characters to denote the horizontal scrollbar
        const top = Array.from(Array(this.width).keys())
            .map(x => within(scrollbarX.low, x, scrollbarX.hi) 
            ? scrollbarXChar 
            : '═')
            .join('')

        // slightly darken border characters
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

        // handle vertical scrollbar based on content length
        const len = Math.max(this.content.length, this.softContent.length)
        const scrollbar = {
            low: Math.max(-0.01, this.line / len) * this.height,
            hi: Math.min(1, (this.line + this.height) / len) * this.height,
        }
        scrollbar.hi = Math.max(Math.ceil(scrollbar.low)+0.5, scrollbar.hi)

        // dont draw vertical scrollbar if not scrollable
        let scrollbarChar = '█';
        if (scrollbar.low <= 0 && scrollbar.hi >= this.height) {
            scrollbarChar = '║'
        }

        // for each line visible in the window
        for (let i = 1; i <= this.height; i++) {
            // get the next line
            const {text, esc} = lines.next().value ?? {text:"", esc:[]};
            const sideChr = borderChar(within(scrollbar.low, i, scrollbar.hi) 
                ? scrollbarChar 
                : '║');

            const bodyText = esc
            // sort by position
            .sort((a, b) => a.pos - b.pos)
            // map to colour pairs
            .reduce((acc, curr) => {
                // get fg/bg and colour/reset
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
    
    // request for the window to be redrawn on next event loop
    requestDraw(callback) {
        // request draw
        Window.drawReq.request = true;
        if (callback) Window.drawReq.callbacks.push(callback);
    }

    // clean the windows effect on the terminal
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

// export list
const termWindows = {
    "Window" : Window,
};

export default termWindows;